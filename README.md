# Visualising Earthquake data from USGS with R Shiny

Visualise earthquake data in real time from [USGS](https://www.usgs.gov/programs/earthquake-hazards/earthquakes)

Steps
- Data was collected via the USGS API in a user-defined function. There is a limit, hence the code retrieves past 30 days of data. A function that makes multiple calls (e.g. multiple 30 day calls) could be used to get more data from the endpoint.
- Some cleaning steps were performed, e.g. to extract out coordinates, clean dates, etc.
- Visualisations: leaflet used for map plot, and plotly with ggplot. Added a time animated plot to show the different earthquakes that happened across time from a magnitude - depth perspective.

[View dashboard](https://jojorabbit111.shinyapps.io/earthquakes_visualisation_with_shiny/)

## Leaflet map of all earthquakes
![Screenshot](https://github.com/ianian-dot/Shiny-earthquake-data-visualisation/blob/main/Screenshot%20from%202024-09-17%2016-24-51.png)
## Most common areas 
![Screenshot2](https://github.com/ianian-dot/Shiny-earthquake-data-visualisation/blob/main/Screenshot%20from%202024-09-19%2015-55-58.png)
## Magnitude-Depth relationship
![Screenshot3](https://github.com/ianian-dot/Shiny-earthquake-data-visualisation/blob/main/Screenshot%20from%202024-09-19%2016-06-32.png)



## Some insights 
1. Shallow and weaker earthquakes: Dominated by USA, Alaska
![Screenshot4](https://github.com/ianian-dot/Shiny-earthquake-data-visualisation/blob/main/shallow-weak.png)

2. Shallow and strong: Mostly on the left of the Pacific Ring, with a notable amount in oceanic regions
![Screenshot5](https://github.com/ianian-dot/Shiny-earthquake-data-visualisation/blob/main/shallow-strong.png)

3. Deep but weak: Rare, as seem in mag-depth distribution (2 are found near Japan/Russia)
![Screenshot5](https://github.com/ianian-dot/Shiny-earthquake-data-visualisation/blob/main/deep-weak.png)

4. Worst earthquakes: Found in Japan, Fiji and Phillipines
![Screenshot5](https://github.com/ianian-dot/Shiny-earthquake-data-visualisation/blob/main/deep-strong.png)
