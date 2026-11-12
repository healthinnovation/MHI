// Collection 2 port of ee_LST_allyear_C01_original.js (commit 17ca6d8, 2022-11-28).
// Landsat 8 Collection 1 has been retired from Earth Engine, so the original script can no longer run.
// Agreement with the deposited raster raw_data/tiff/LST_2017_2021_mean.tif: see README.md.

// 1. STUDY AREA ------------------------------------------------------------------
var geom = ee.Geometry.Polygon([[
  [-77.39449018517418, -11.473924913677363],
  [-77.39449018517418, -12.56993498175432],
  [-76.42769331017418, -12.56993498175432],
  [-76.42769331017418, -11.473924913677363]
]], null, false);
var geometry = geom;

// 2. COLLECTION 2 INPUTS ---------------------------------------------------------
var date_start = '2017-01-01';
var date_end = '2021-12-31';
var mask_bits = (1 << 3) | (1 << 4);
// Period switch: 'allyear' keeps date_start to date_end, end date excluded (default).
// 'summer' keeps date_start to date_end, both included, with month-day 12-21 to 03-21.
var period = 'allyear';
var date_filter = ee.Filter.date(date_start, date_end);
if (period === 'summer') {
  var windows = [];
  for (var y = Number(date_start.slice(0, 4)) - 1; y <= Number(date_end.slice(0, 4)); y++) {
    windows.push(ee.Filter.date(y + '-12-21', (y + 1) + '-03-22'));
  }
  date_filter = ee.Filter.and(
    ee.Filter.date(date_start, ee.Date(date_end).advance(1, 'day')),
    ee.Filter.or.apply(null, windows)
  );
}

var l2 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
  .filter(date_filter)
  .filterBounds(geometry);
var toa = ee.ImageCollection('LANDSAT/LC08/C02/T1_TOA')
  .filter(date_filter)
  .filterBounds(geometry);
var joined = ee.ImageCollection(ee.Join.saveFirst('toa').apply(
  l2,
  toa,
  ee.Filter.equals({leftField: 'system:index', rightField: 'system:index'})
));
var col = joined.map(function(img) {
  return ee.Image(img).select(['SR_B4', 'SR_B5'])
    .multiply(0.0000275).add(-0.2)
    .rename(['B4', 'B5'])
    .addBands(ee.Image(img.get('toa')).select('B10'))
    .updateMask(ee.Image(img).select('QA_PIXEL').bitwiseAnd(mask_bits).eq(0));
});
print(l2.size(), 'scenes L2');
print(col.size(), 'scenes joined');

// 3. MEDIAN COMPOSITE AND NDVI ---------------------------------------------------
// Reducer switch: 'median' is the reducer of the original script (default).
// 'mean' gives the per-pixel mean composite (raw_data/tiff/LST_2017_2021_C2_mean.tif);
// every later step (NDVI range, emissivity, LST) is unchanged.
var reducer = 'median';
var image = reducer === 'mean' ? col.mean() : col.median();
var ndvi = image.normalizedDifference(['B5', 'B4']).rename('NDVI');
var thermal = image.select('B10');

// 4. NDVI RANGE, FRACTIONAL VEGETATION, EMISSIVITY -------------------------------
var min = ee.Number(ndvi.reduceRegion({
  reducer: ee.Reducer.min(), geometry: geometry, scale: 200, maxPixels: 1e12
}).values().get(0));
var max = ee.Number(ndvi.reduceRegion({
  reducer: ee.Reducer.max(), geometry: geometry, scale: 200, maxPixels: 1e9
}).values().get(0));
print(min, 'min');
print(max, 'max');
var fv = ndvi.subtract(min).divide(max.subtract(min)).pow(ee.Number(2)).rename('FV');
var EM = fv.multiply(ee.Number(0.004)).add(ee.Number(0.986)).rename('EMM');

// 5. LAND SURFACE TEMPERATURE (CELSIUS) -----------------------------------------
var LST = thermal.expression(
  '(Tb/(1 + (0.00115* (Tb / 1.438))*log(Ep)))-273.15', {
    'Tb': thermal.select('B10'),
    'Ep': EM.select('EMM')
  }).rename('LST').clip(geometry);
Map.addLayer(LST, {min: 20.5, max: 29.3, palette: ['040274', '30c8e2', 'fff705', 'ff0000']}, 'LST');

// 6. EXPORT ----------------------------------------------------------------------
Export.image.toDrive({
  image: LST,
  description: 'LST_2017_2021_' + (period === 'summer' ? 'summer_' : '') + reducer + '_C2',
  crs: 'EPSG:32718',
  crsTransform: [30, 0, 254700, 0, -30, 8723130],
  dimensions: '2449x3644'
});
