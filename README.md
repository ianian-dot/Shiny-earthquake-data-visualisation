# Visualising Earthquake data from USGS 

Visualise earthquake data in real time from [USGS](https://www.usgs.gov/programs/earthquake-hazards/earthquakes)

Steps
- Data was collected via the USGS API in a user-defined function. There is a limit, hence the code retrieves past 30 days of data. A function that makes multiple calls (e.g. multiple 30 day calls) could be used to get more data from the endpoint.
- Some cleaning steps were performed, e.g. to extract out coordinates, clean dates, etc.
- Visualisations: <leaflet> used for map plot, and <plotly> with <ggplot>

[View dashboard](https://jojorabbit111.shinyapps.io/earthquakes_visualisation_with_shiny/)

![Screenshot](https://github.com/ianian-dot/Shiny-earthquake-data-visualisation/blob/main/Screenshot%20from%202024-09-17%2016-24-51.png)
