## get the packages:
# install.packages(c("shiny", "leaflet", "httr", "jsonlite", "dplyr", "lubridate", "ggplot2"))
library(httr)
library(jsonlite)
library(ggplot2)
library(shiny)
library(leaflet)
library(dplyr)
library(plotly)
library(DT)
library(lubridate)

## GETTING DATA ==============================

## METHOD 1 ===================================================
today = as.Date(Sys.time())
default_start = today - 365 ## a year ago

## Function to get date exactly n days ago
n_days_ago <- function(n){
  today - days(n)
}

# Construct the query URL
get_eq_data <- function(start_time , end_time){
  query_url <- paste0(
    "https://earthquake.usgs.gov/fdsnws/event/1/query?",
    "format=geojson",
    "&starttime=", start_time,
    "&endtime=", end_time)  # Adjust minimum magnitude to manage data volume
    
    ## show url
    print(query_url)
    
    response <- GET(query_url)
    data <- fromJSON(content(response, "text"))
    data
}

## Fetch data with user defined function
data <- get_eq_data(n_days_ago(30),today)
names(data)
## One level down 
features <-(data$features)

## Two levels down 
names(features)
properties_df <- data.frame(features$properties)
geometry_df <- data.frame(features$geometry)
dim(properties_df) == dim(geometry_df)

## Concatenate both properties and geometry dfs 
combined_df <- cbind(properties_df, geometry_df)
dim(combined_df) == dim(properties_df) ## same number of rows 

# response <- GET(query_url)
# data <- fromJSON(content(response, "text"), flatten = TRUE)
# earthquakes <- data$features

## METHOD 2 (get data from only the previous day) ===================================================
# less recommended, since not much data 
# response <- GET("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_day.geojson")
# data <- fromJSON(content(response, 'text'))

## Simple data exploration / Data cleaning =====================================================

## Explore 
earthquakes_df <- combined_df
rm(combined_df)
cat('Number of earthquakes in dataset: ', nrow(earthquakes_df), '\n')
colnames(earthquakes_df) ## useful cols: mag, place, time, 

## Drop unwanted cols 
# Cols that dont vary 
more_than_1_cols <- sapply(earthquakes_df, function(x) length(unique(x)) > 1)
earthquakes_df <- earthquakes_df[more_than_1_cols]

## Cleaning 
earthquakes_df$time <- as.POSIXct(earthquakes_df$time/1000, origin = '1970-01-01') ## time are milliseconds since epoch i.e. 1 jan 1970 -- convert to seconds
earthquakes_df$date <- as.Date(earthquakes_df$time)
earthquakes_df$updated <- as.POSIXct(earthquakes_df$updated/1000, origin = '1970-01-01') ## time are milliseconds since epoch i.e. 1 jan 1970 -- convert to seconds

## Extract the coordinates individually 
typeof(earthquakes_df$coordinates)
earthquakes_df$longitude <- sapply(earthquakes_df$coordinates, FUN = function(x) x[1])
earthquakes_df$latitude <- sapply(earthquakes_df$coordinates, FUN = function(x) x[2])
earthquakes_df$depth <- sapply(earthquakes_df$coordinates, FUN = function(x) x[3])
earthquakes_df <- earthquakes_df %>% select(-type)



## Extract the area/region/state/country
earthquakes_df['country_US_state'] = sapply(strsplit(earthquakes_df$place, ','), function(x)x[2])
earthquakes_df['country_US_state'] = as.character(earthquakes_df['country_US_state'])
earthquakes_df['country_US_state'] = as.character(trimws(earthquakes_df['country_US_state'], 
                                             which = 'both'))
unique_state_countries <- unique(earthquakes_df$country_US_state)

## (More efficient method)
earthquakes_df$place <- as.character(earthquakes_df$place)
# Extract everything after the first comma and trim whitespace
earthquakes_df$country_US_state <- trimws(sub(".*?,", "", earthquakes_df$place))
unique_state_countries <- unique(earthquakes_df$country_US_state)

## Missing values analysis 
na_counts_ordered <- colSums(is.na(earthquakes_df))[order(colSums(is.na(earthquakes_df)), decreasing = TRUE)]
na_counts_positive <- na_counts_ordered[na_counts_ordered >0]
barplot(na_counts_positive, ylim = c(0, nrow(earthquakes_df)+10), 
        main = 'Number of NAs per col')
abline(h = nrow(earthquakes_df), col = 'red', lty = 2)
legend(x = 6, y = 4000, yjust = 0.5,legend = 'max nrows', col = 'red', lty = 2)

## Remove rows with almost all NAs 
earthquakes_df <- earthquakes_df %>% select(-names(na_counts_ordered[1:4]))
dim(earthquakes_df)

## Some visualisations =================
# -------- Histogram of earthquake magnitudes
ggplot(earthquakes_df, aes(x = mag)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Earthquake Magnitudes", 
       subtitle = paste(as.Date(min(earthquakes_df$time)), 'to', as.Date(max(earthquakes_df$time))),
       x = "Magnitude", y = "Count") +
  theme_minimal()

# -------- Scatterplot of longitude and latitude (size = mag)
ggplot(earthquakes_df, aes(x = longitude, y = latitude, colour =mag)) +
  geom_point() +
  theme_classic()

## ==== SHINY APP ========================================================
ui <- fluidPage(
  titlePanel("Interactive Earthquake Data Explorer"),
  
  ## Side bar for inputs : range for magnitude and for dates
  sidebarLayout(
    sidebarPanel(
      ## for magnitude
      sliderInput("magRange", 
                 "Magnitude Range",
                 min = min(earthquakes_df$mag), 
                 max = max(earthquakes_df$mag),
                 value = range(earthquakes_df$mag), 
                 step = 0.1),
      ## for date
      dateRangeInput("dateRange", ## enter date range to filter earthquakes
                     "Date Range", 
                     start = min(earthquakes_df$time),
                     end = max(earthquakes_df$time)
                     ),
      ## for tsunami related earthquakes (binary)
      checkboxInput('showTsunami', "Show Only Tsunami-Related Earthquake"),
      
      ## add conditional pandel for second and third tabs: number of bins for histograms
      conditionalPanel(
        condition = "input.tabs == 'Magnitude Distribution' || input.tabs == 'Depth Distribution'",
        sliderInput(inputId = 'histBins', 
                    label = 'Number of bins', 
                    min = 1,
                    max = 40,
                    step = 2, 
                    value = 15 ##initial value of the slider
                    ))
      
      ), 
  
  mainPanel(
    tabsetPanel( ## organises outputs into TABS for easier management
      tabPanel("Map", ## name of the output tab
               ## insert plot object 
               leafletOutput("quakeMap", ##output name (from server)
                             height = 800)), ## all these ids will come from server functions
      tabPanel("Magnitude distribution", 
               plotlyOutput("magPlot")), 
      tabPanel("Depth Distribution", 
               plotlyOutput("depthPlot")), 
      tabPanel("Mag - Depth Distribution", 
               plotlyOutput("magdepthPlot")), 
      tabPanel("Data Table", 
               dataTableOutput("dataTable"))
    )
  )
))

server <- function(input, output, session){
  
  ## Filtering based on user inputs: date and magnitude ==========================================================
  filtered_data <- reactive({ ## based on Shiny reactive concepts -- dynamic changes to output based on what the user inputs!
    data <- earthquakes_df %>% filter(
      mag >= input$magRange[1],
      mag <= input$magRange[2], 
      time >= input$dateRange[1], 
      time <= input$dateRange[2] 
      )
    ## Show tsunami? 
    if (input$showTsunami){ 
      data <- data %>% filter(tsunami == 1)}
    ## return filtered data
    return(data)
  })
  ## Filtering based on user inputs: date and magnitude ==========================================================
  
  
  ## FIRST OUTPUT: LEAFLET MAP PLOT =========================================================================
  output$quakeMap <- renderLeaflet({ ## use a leaflet for this map plot
    leaflet(data = filtered_data()) %>% addTiles() %>% 
      addCircles(lng = ~longitude, 
                 lat = ~latitude, 
                 radius = ~ ifelse(mag <1, 2, mag*2), ## set a lower limit to 2, and then multiply the size of the rest by 2
                 color = ~ifelse(mag > 5, 'red', ifelse(mag >3, 'orange', 'green')), 
                 # stroke = FALSE, ## for the borders of circles 
                 popup = ~paste0( ##pop up message for more info 
                   "<strong>Location: </strong>", place, "<br>", 
                   "<strong>Magnitude: </strong>", mag, "<br>", 
                   "<strong>Depth: </strong>", depth, "<br>", 
                   "<strong>Time: </strong>", time, "<br>" 
                 )) %>% 
      addLegend(position = 'bottomright', 
                colors = c('red', 'orange', 'green'), ## add legend for the coloring used in addCircles
                labels = c('Magnitude >5','Magnitude 3-5', 'Magnitude <3' ))
    
    })
  
  ## SECOND OUTPUT: HISTOGRAM OF MAGNITUDE ====================================
  output$magPlot <- renderPlotly({ ## use a plotly object for this plot
    p <- ggplot(filtered_data(), aes(x = mag)) +
      geom_histogram(binwidth = 0.1, fill = 'skyblue', color = 'black') +
      labs(title = paste('Magnitude Distribution for', input$magRange[1], 'to', input$magRange[2]),
           x = 'Magnitude', 
           y = 'Frequency') + 
      theme_minimal()
    
    ## Insert ggplot object into plotly for interactive functionality
    ggplotly(p)
    
  })
  
  ## THIRD OUTPUT: DEPTH DISTRIBUTION
  output$depthPlot <- renderPlotly({
    filtered_data <- filtered_data()
    q <- ggplot(filtered_data, aes(x= depth, y = 1)) +
      geom_jitter(alpha = 0.1)+
      geom_text(x = 500, y = 1.5, 
                label= paste('Mean:', round(mean(filtered_data$depth, na.rm =T), 2), '\n', 
                             'Median:', median(filtered_data$depth, na.rm = T))) + 
      ylim(c(0,2)) +
      labs(y = '')+
      theme(axis.ticks.y = element_blank(),
            axis.text.y = element_blank(), 
            panel.background = element_rect(fill = 'white'), 
            panel.grid.major = element_line(size = 0.4, 
                                            color = 'grey'))
    
    ## Insert ggplot object into plotly for interactive functionality
    ggplotly(q)
    
  })
  
  ## FOURTH OUTPUT: MAG-DEPTH SCATTERPLOT
  output$magdepthPlot <- renderPlotly({
   o <- ggplot(filtered_data(), aes(x = depth, y = mag))+
     geom_point(size = 0.5) +
     geom_smooth(method = 'loess',color = 'darkblue', 
                 method.args = list(degree = 1))+
     # geom_smooth(method = 'lm',color = 'lightblue')+
     ## add legend for both lines
     scale_color_manual(name = 'Line fit', 
                        values = c('Loess' = 'darkblue', 
                                   'LM' = 'lightblue'))+
     theme_classic()
   
   ggplotly(o)
  })
  
  ## FIFTH OUTPUT: SUMMARY TABLE
  output$dataTable <- renderDataTable({
    filtered_data() %>%
      select(time, mag, depth, place, tsunami) %>%
      arrange(desc(time))
  })
}

shinyApp(ui = ui, server = server)


## Testing the plots 
leaflet(data = earthquakes_df) %>% addTiles() %>% 
  addCircles(lng = ~longitude, 
             lat = ~latitude, 
             radius = ~ ifelse(mag <1, 2, mag*2), ## set a lower limit to 2, and then multiply the size of the rest by 2
             color = ~ifelse(mag > 5, 'red', ifelse(mag >3, 'orange', 'green')), 
             # stroke = FALSE, ## for the borders of circles 
             popup = ~paste0( ##pop up message for more info 
               "<strong>Location: </strong>", place, "<br>", 
               "<strong>Magnitude: </strong>", mag, "<br>", 
               "<strong>Depth: </strong>", depth, "<br>", 
               "<strong>Time: </strong>", time, "<br>" 
             )) %>% 
  addLegend(position = 'bottomright', 
            colors = c('red', 'orange', 'green'), ## add legend for the coloring used in addCircles
            labels = c('Magnitude >5','Magnitude 3-5', 'Magnitude <3' ))


url <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/detail/ci40734959.geojson"
response <- GET(url)
data <- fromJSON(content(response, 'text'))
data$

GET()

######## APPENDIX ##############################################################
######## APPENDIX ##############################################################

# names(data)
# str(data)
# sapply(data$features, typeof)
# 
# ## 1. type
# data$type
# ## 2. metadata
# metadata <- data$metadata
# names(metadata)
# unlist(metadata)
# ## 3. features
# features <- data$features
# sapply(features, typeof)
# table(features$type)
# names(features$properties) ## a list of a lot of information 
# 
# ## Extracting earthquake information 
# earthquakes <- (features)
# head(earthquakes)
# typeof(earthquakes)
# properties_df <- as.data.frame(earthquakes$properties)
# geometry_df <- as.data.frame(earthquakes$geometry)
# colnames(properties_df) ; colnames(geometry_df)
# dim(properties_df) ; dim(geometry_df)
# 
# ## Combine the two tables into one to include geometrical information (longitude and latitude)
# dim(cbind(properties_df, geometry_df)) ##verify 
# earthquakes_df <- cbind(properties_df, geometry_df)

server_2 <- function(input, output, session){
  
  # Filtering based on user inputs
  filtered_data <- reactive({
    data <- earthquakes_df %>% filter(
      mag >= input$magRange[1],
      mag <= input$magRange[2], 
      time >= input$dateRange[1], 
      time <= input$dateRange[2]
    )
    if (input$showTsunami){ 
      data <- data %>% filter(tsunami == 1)
    }
    data
  })
  
  # Initialize the map
  output$quakeMap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(lng = mean(earthquakes_df$longitude, na.rm = TRUE), 
              lat = mean(earthquakes_df$latitude, na.rm = TRUE), 
              zoom = 2) %>%
      addLegend(position = 'bottomright', 
                colors = c('red', 'orange', 'green'),
                labels = c('Magnitude >5', 'Magnitude 3-5', 'Magnitude <3'),
                title = "Earthquake Magnitude")
  })
  
  # Update the map based on filtered data
  observe({
    data <- filtered_data()
    
    leafletProxy("quakeMap", data = data) %>%
      clearMarkers() %>%
      clearShapes() %>%
      addCircles(
        lng = ~longitude, 
        lat = ~latitude, 
        radius = ~ifelse(mag < 1, 20000, mag * 20000),
        color = ~ifelse(mag > 5, 'red', ifelse(mag > 3, 'orange', 'green')),
        stroke = FALSE,
        fillOpacity = 0.6,
        popup = ~paste0(
          "<strong>Location: </strong>", place, "<br>", 
          "<strong>Magnitude: </strong>", mag, "<br>", 
          "<strong>Depth: </strong>", depth, "<br>", 
          "<strong>Time: </strong>", time, "<br>"
        )
      )
  })
}

ggplot(earthquakes_df, aes(x= depth, y = 1)) +
  geom_jitter(alpha = 0.1)+
  ylim(c(0,2)) +
  labs(y = '')+
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), 
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major = element_line(size = 0.4, 
                                        color = 'grey'))

ggplot(earthquakes_df, aes(x= depth))+
  geom_histogram()
