# Rosario, Argentina Solar Potential 

This is a repository including a solar potential analysis for the city of Rosario in Argentina.  
The analysis is part of the thesis *Estimating building rooftop solar potential in a city from a developing country*.

## The repository includes the following: 

1. The ```index.html``` code is for creating a web map displaying solar potential for building rooftops in the city Rosario, Argentina.  
The building polygon layer was collected from [OpenStreetMap](https://www.openstreetmap.org/#map=13/-32.9671/-60.6759) using [GEOFABRIK](https://www.geofabrik.de/) and the solar potential was calculated using The World Bank's [Global Solar Atlas](https://globalsolaratlas.info/global-pv-potential-study) global horizontal irradiance (GHI) layer.  
The web map was built using the libraries [jQuery API](https://api.jquery.com/), [Leaflet](https://leafletjs.com/) and hosted on [GitHub Pages](https://pages.github.com/).    
The web map is available [here](https://einavg7.github.io/Rosario_Solar_Potential/).
2. The ```data``` folder includes the building polygon layers for the web map. 
3. The ```image``` folder includes images used for the web map.
4. The ```QGIS``` folder includes the ```rosario.qgz``` project and the shapefiles used in the project (see the floder ```QGIS/README.md```).  
5. The ```sampling``` folder includes a R Markdown code for the sampling analysis (see the floder ```sampling/README.md```). 

## License
MIT


