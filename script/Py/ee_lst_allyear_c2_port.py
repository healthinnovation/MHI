# Python version of script/Js/ee_LST_allyear_C2_port.js.
# Downloads the Collection 2 port composite in 64 tiles on the grid of raw_data/tiff/LST_2017_2021_mean.tif
# (EPSG:32718, origin 254700 / 8723130, 30 m, 2449 x 3644 cells); script/R/c2_port_mosaic.R merges the tiles.
# Also writes the scene list (<out_dir>/LST_<years>_C2_scenes.csv) and the run record (<out_dir>/<label>.json).
# Usage, from the repository root:
# python3 script/Py/ee_lst_allyear_c2_port.py 2017-01-01 2021-12-31 base raw_data/tiff <median|mean>
# base: the cloud and cloud shadow mask of the original script, QA_PIXEL bits 3 and 4 in Collection 2.
# Optional sixth argument summer: only the scenes acquired from the start date to the end date, both included,
# with month-day 12-21 to 03-21 (files LST_<years>_C2_summer_*); without it the end date is excluded.
# python3 script/Py/ee_lst_allyear_c2_port.py 2017-01-01 2021-12-31 base raw_data/tiff <median|mean> summer

import concurrent.futures
import csv
import datetime
import json
import os
import sys
import time
import urllib.request

import ee

ee.Initialize()

# 1. STUDY AREA AND GRID ---------------------------------------------------------
aoi = ee.Geometry.Polygon(
    [[
        [-77.39449018517418, -11.473924913677363],
        [-77.39449018517418, -12.56993498175432],
        [-76.42769331017418, -12.56993498175432],
        [-76.42769331017418, -11.473924913677363],
    ]],
    None,
    False,
)
x_origin = 254700
y_origin = 8723130
n_col = 2449
n_row = 3644
out_dir = sys.argv[4] if len(sys.argv) > 4 else "rasters"
os.makedirs(out_dir, exist_ok=True)

# 2. ARGUMENTS ------------------------------------------------------------------
date_start = sys.argv[1]
date_end = sys.argv[2]
mask_variant = sys.argv[3]
reducer = sys.argv[5] if len(sys.argv) > 5 else "median"
period = "_summer" if len(sys.argv) > 6 and sys.argv[6] == "summer" else ""
label = f"LST_{date_start[:4]}_{date_end[:4]}_C2{period}_{reducer}"
mask_bits = (1 << 3) | (1 << 4)
date_filter = ee.Filter.date(date_start, date_end)
if period:
    date_end_next = (datetime.date.fromisoformat(date_end) + datetime.timedelta(days=1)).isoformat()
    date_filter = ee.Filter.And(
        ee.Filter.date(date_start, date_end_next),
        ee.Filter.Or(
            [
                ee.Filter.date(f"{y}-12-21", f"{y + 1}-03-22")
                for y in range(int(date_start[:4]) - 1, int(date_end[:4]) + 1)
            ]
        ),
    )
run_start_utc = datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")

# 3. COLLECTION: C2 L2 SR JOINED TO C2 TOA BRIGHTNESS TEMPERATURE --------------
l2 = (
    ee.ImageCollection("LANDSAT/LC08/C02/T1_L2")
    .filter(date_filter)
    .filterBounds(aoi)
)
toa = (
    ee.ImageCollection("LANDSAT/LC08/C02/T1_TOA")
    .filter(date_filter)
    .filterBounds(aoi)
)
joined = ee.Join.saveFirst("toa").apply(
    l2,
    toa,
    ee.Filter.equals(leftField="system:index", rightField="system:index"),
)
joined = ee.ImageCollection(joined)
col = joined.map(
    lambda img: ee.Image(img)
    .select(["SR_B4", "SR_B5"])
    .multiply(0.0000275)
    .add(-0.2)
    .rename(["B4", "B5"])
    .addBands(ee.Image(img.get("toa")).select("B10"))
    .updateMask(ee.Image(img).select("QA_PIXEL").bitwiseAnd(mask_bits).eq(0))
)
n_l2 = l2.size().getInfo()
n_toa = toa.size().getInfo()
n_join = col.size().getInfo()
fields = ["system:index", "LANDSAT_PRODUCT_ID", "DATE_ACQUIRED", "WRS_PATH", "WRS_ROW", "CLOUD_COVER", "CLOUD_COVER_LAND"]
scenes = joined.reduceColumns(ee.Reducer.toList(len(fields)), fields).get("list").getInfo()
scenes = sorted(scenes, key=lambda s: (s[2], s[0]))
with open(os.path.join(out_dir, f"LST_{date_start[:4]}_{date_end[:4]}_C2{period}_scenes.csv"), "w", newline="") as fh:
    writer = csv.writer(fh, lineterminator="\n")
    writer.writerow(fields)
    writer.writerows(scenes)
n_per_year = {}
for s in scenes:
    n_per_year[s[2][:4]] = n_per_year.get(s[2][:4], 0) + 1

# 4. TEMPORAL COMPOSITE, NDVI, FV, EMISSIVITY, LST --------------------------------
image = col.mean() if reducer == "mean" else col.median()
ndvi = image.normalizedDifference(["B5", "B4"]).rename("NDVI")
thermal = image.select("B10")
ndvi_min = ee.Number(
    ndvi.reduceRegion(
        reducer=ee.Reducer.min(), geometry=aoi, scale=200, maxPixels=1e12
    ).values().get(0)
).getInfo()
ndvi_max = ee.Number(
    ndvi.reduceRegion(
        reducer=ee.Reducer.max(), geometry=aoi, scale=200, maxPixels=1e9
    ).values().get(0)
).getInfo()
fv = ndvi.subtract(ndvi_min).divide(ndvi_max - ndvi_min).pow(2).rename("FV")
em = fv.multiply(0.004).add(0.986).rename("EMM")
lst = (
    thermal.expression(
        "(Tb/(1 + (0.00115* (Tb / 1.438))*log(Ep)))-273.15",
        {"Tb": thermal.select("B10"), "Ep": em.select("EMM")},
    )
    .rename("LST")
    .clip(aoi)
    .unmask(-9999)
    .toFloat()
)
meta = {
    "label": label,
    "date_start": date_start,
    "date_end_exclusive": date_end,
    "mask_bits": mask_bits,
    "reducer": reducer,
    "n_l2": n_l2,
    "n_toa": n_toa,
    "n_joined": n_join,
    "n_joined_per_year": n_per_year,
    "ndvi_min_aoi_200m": ndvi_min,
    "ndvi_max_aoi_200m": ndvi_max,
    "ee_api_version": ee.__version__,
    "run_start_utc": run_start_utc,
}
if period:
    meta["date_end_exclusive"] = date_end_next
    meta["month_day"] = "12-21 to 03-21"
print(json.dumps(meta), flush=True)
with open(os.path.join(out_dir, label + ".json"), "w") as fh:
    json.dump(meta, fh, indent=1)

# 5. TILED DOWNLOAD ON THE DEPOSITED RASTER GRID --------------------------------
n_tile = 8
col_edges = [round(n_col * i / n_tile) for i in range(n_tile + 1)]
row_edges = [round(n_row * j / n_tile) for j in range(n_tile + 1)]
os.makedirs(os.path.join("tiles", label), exist_ok=True)
jobs = [
    (i, j)
    for i in range(n_tile)
    for j in range(n_tile)
]


def fetch(ij):
    i, j = ij
    path = os.path.join("tiles", label, f"t_{i}_{j}.tif")
    if os.path.exists(path) and os.path.getsize(path) > 1000:
        return path
    c0, c1 = col_edges[i], col_edges[i + 1]
    r0, r1 = row_edges[j], row_edges[j + 1]
    params = {
        "crs": "EPSG:32718",
        "crs_transform": [30, 0, x_origin + 30 * c0, 0, -30, y_origin - 30 * r0],
        "dimensions": f"{c1 - c0}x{r1 - r0}",
        "format": "GEO_TIFF",
    }
    for k in range(5):
        try:
            url = lst.getDownloadURL(params)
            urllib.request.urlretrieve(url, path)
            return path
        except Exception as e:
            print(f"tile {i},{j} retry {k}: {str(e)[:160]}", flush=True)
            time.sleep(10 * (k + 1))
    raise RuntimeError(path)


t0 = time.time()
with concurrent.futures.ThreadPoolExecutor(max_workers=6) as ex:
    done = list(ex.map(fetch, jobs))
print(f"{label}: {len(done)} tiles in {time.time() - t0:.0f}s", flush=True)
