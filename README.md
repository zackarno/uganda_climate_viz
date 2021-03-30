# uganda_climate_viz
An exploratory dashboard to allow easy investigation of climatic data  (historical, current, and forcasted) at both spatial and temporal scales useful for planning purposes

## Description
### Environmental variables
a.) precipitation (CHIRPS), b.) NDVI (LANDSATE 8 , MODIS) 
### Spatial resolutions
The user can select the following spatial scales: a.) admin 2 (district), b.) admin 3 (sub-county), and c) wastershed (HydroSHEDS) basin levels 4 & 5.
### Temporal resolutions
- If the user selects Historical, charts will display most current data available will be compared to the 10 year average and choropleth map will be filled with using standard deviations from the mean (z-score). 
- If user selects "current" maps and charts will show the current precipitaiton accumulation (mm) and NDVI values. 
- If the user selects forecasted the charts will display precipitation forecast for the next 30 days and the map will display forecasted cummulative precipitation (mm) for aggregated at the selecte admin level for the next 30 days. 
## Further information
Data set was obtained from Google Earth Engine using rgee. For more information and/or to request the data set used please contact the author
