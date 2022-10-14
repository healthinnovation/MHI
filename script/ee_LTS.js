// Study area --------------------------------------------------------------------------------
var geom = ee.Geometry.Polygon([[
          [-77.39449018517418, -11.473924913677363],
          [-77.39449018517418, -12.56993498175432],
          [-76.42769331017418, -12.56993498175432],
          [-76.42769331017418, -11.473924913677363]
          ]], null, false);

// Cloud mask ---------------------------------------------------------------------------------
function maskL8sr(col) {
  // Bits 3 and 5 are cloud shadow and cloud, respectively.
  var cloudShadowBitMask = (1 << 3);
  var cloudsBitMask = (1 << 5);
  // Get the pixel QA band.
  var qa = col.select('pixel_qa');
  // Both flags should be set to zero, indicating clear conditions.
  var mask = qa.bitwiseAnd(cloudShadowBitMask).eq(0)
                 .and(qa.bitwiseAnd(cloudsBitMask).eq(0));
  return col.updateMask(mask);
}

// Palette color for the vis ------------------------------------------------------------------
var vizParams = {
bands: ['B5', 'B6', 'B4'],
min: 0,
max: 4000,
gamma: [1, 0.9, 1.1]
};

var vizParams2 = {
bands: ['B4', 'B3', 'B2'],
min: 0,
max: 3000,
gamma: 1.4,
};

// Reading the collections image: --------------------------------------------------------------
 {
var col = ee.ImageCollection('LANDSAT/LC08/C01/T1_SR')
.map(maskL8sr)
.filterDate('2017-01-01','2017-12-31')
.filterBounds(geometry);
}
print(col, 'coleccion');

// Imagen reduction ------------------------------------------------------------------------------
{
var image = col.median();
print(image, 'image');
Map.addLayer(image, vizParams2);
}


// Median ---------------------------------------------------------------------------------------
{
var ndvi = image.normalizedDifference(['B5', 
'B4']).rename('NDVI');
var ndviParams = {min: -1, max: 1, palette: ['blue', 'white', 
'green']};
print(ndvi,'ndvi');
Map.addLayer(ndvi, ndviParams, 'ndvi');
}

// Select thermal band 10(with brightness tempereature), no calculation --------------------------
var thermal= image.select('B10').multiply(0.1);
var b10Params = {min: 291.918, max: 302.382, palette: ['blue', 
'white', 'green']};
Map.addLayer(thermal, b10Params, 'thermal');

// Find the min and max of NDVI ------------------------------------------------------------------
{
var min = ee.Number(ndvi.reduceRegion({
  reducer: ee.Reducer.min(),
  geometry: geometry,
  scale: 100,
  maxPixels: 1e9}).values().get(0));
  print(min, 'min');
var max = ee.Number(ndvi.reduceRegion({
  reducer: ee.Reducer.max(),
  geometry: geometry,
  scale: 100,
  maxPixels: 1e9}).values().get(0));
  print(max, 'max')
}

// Fractional vegetation ---------------------------------------------------------------------------
{
var fv =(ndvi.subtract(min).divide(max.subtract(min))).pow(ee.Number(2)).rename('FV'); 
print(fv, 'fv');
Map.addLayer(fv);
}

// Emissivity --------------------------------------------------------------------------------------

var a= ee.Number(0.004);
var b= ee.Number(0.986);
var EM=fv.multiply(a).add(b).rename('EMM');
var imageVisParam3 = {min: 0.9865619146722164, max:0.989699971371314};
Map.addLayer(EM, imageVisParam3,'EMM');

// LST in Celsius Degree bring -273.15 -------------------------------------------------------------
// NB: In Kelvin don't bring -273.15
var LST = thermal.expression(
'(Tb/(1 + (0.00115* (Tb / 1.438))*log(Ep)))-273.15', {
 'Tb': thermal.select('B10'),
'Ep': EM.select('EMM')
}).rename('LST').clip(geometry);

// Map final of LST --------------------------------------------------------------------------------
Map.addLayer(LST, {min: 20.569706944223423, max:29.328077233404645, palette: [
'040274', '040281', '0502a3', '0502b8', '0502ce', '0502e6',
'0602ff', '235cb1', '307ef3', '269db1', '30c8e2', '32d3ef',
'3be285', '3ff38f', '86e26f', '3ae237', 'b5e22e', 'd6e21f',
'fff705', 'ffd611', 'ffb613', 'ff8b13', 'ff6e08', 'ff500d',
'ff0000', 'de0101', 'c21301', 'a71001', '911003'
 ]},'LST');
