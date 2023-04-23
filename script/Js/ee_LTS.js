/*
This script is based in the public code of Sean McCartney (sean.mccartney@nasa.gov)
based in ARSET Training: Satellite Remote Sensing for Measuring Urban Heat Islands and Constructing Heat Vulnerability Indices
August 2, 2022 - August 11, 2022
*/

// Define study area
var aoi = ee.Geometry.Polygon([[
          [-77.39449018517418, -11.473924913677363],
          [-77.39449018517418, -12.56993498175432],
          [-76.42769331017418, -12.56993498175432],
          [-76.42769331017418, -11.473924913677363]
          ]], null, false);

// Set the basemap to display as satellite.
Map.setOptions('SATELLITE');

// Assign a variable to the sensor-specific bands unique to each Landsat mission.
var LC08_bands = ['ST_B10', 'QA_PIXEL']; // Landsat 8 surface temperature (ST) & QA_Pixel bands


//****************** CLOUD MASK FUNCTION *****************//
// Create a function to mask clouds and cloud shadows based on the QA_PIXEL band of Landsat 8 & 9
// For information on bit values for the QA_PIXEL band refer to: 
// https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC08_C02_T1_L2#bands
function cloudMask(image) {
  var qa = image.select('QA_PIXEL');
  var mask = qa.bitwiseAnd(1 << 3)
    .or(qa.bitwiseAnd(1 << 4));
  return image.updateMask(mask.not());
}

/* Assign variables to import the Landsat Collection 2, Tier 1, Level 2 image collections, selecting 
the ST and QA_PIXEL bands, and spatially filtering the image collection by your aoi. */
var L8 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
  .select('ST_B10', 'QA_PIXEL')
  .filterBounds(aoi)
  .filterDate('2017-12-21','2021-03-23')
  .filter(ee.Filter.calendarRange(2017,2021,'year'))
  .filter(ee.Filter.calendarRange(12,3,'month'))
  .map(cloudMask)

Map.addLayer(L8)

// Use print statements to print the argument to the console.
print('Landsat 8 Collection: ', L8);

/* Create a funtion using Landsat scale factors for deriving ST in Kelvin and Celsius.
For more information on ST scale factors, refer to:
https://www.usgs.gov/landsat-missions/landsat-collection-2-level-2-science-products */
function applyScaleFactors(image) {
  var thermalBands = image.select('ST_B10').multiply(0.00341802).add(149.0) // Scale factors for Kelvin
  .subtract(273.15); // Scale factor for degrees Celsius
  return image.addBands(thermalBands, null, true);
}

// Define a variable to apply scale factors to the filtered image collection.
var landsatST = L8.map(applyScaleFactors);
// Use print statements to print the argument to the console.
print('Landsat ST (Celsius): ',landsatST);

//****************** CALCULATE MEAN SURFACE TEMPERATURE *****************//
// Define a variable to calculate mean ST for each pixel geography 
// throughout the filtered image collection.
var mean_LandsatST = landsatST.median();

// Define a variable to use the clip funtion to subset your imagery to the aoi.
var clip_mean_ST = mean_LandsatST.clip(aoi);
print(clip_mean_ST)
// Add the image to the map window, defining min/max values, a palette for 
// symbology, assign a name to the visualization, and display the result.
Map.addLayer(clip_mean_ST, {
  bands: "ST_B10", 
  min: 28, max: 47, 
  palette: ['blue','white','red']}, "ST", true);

Export.image.toDrive({
  image: clip_mean_ST.select("ST_B10"),
  description: 'LST_summer',
  region: aoi,
  scale:30,
  crs:'EPSG:32718'
});
