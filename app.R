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
library(countrycode)
library(stringr)
library(wordcloud)
library(tm)

set.seed(42)

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

## Larger wrapper function that retrieves data from a larger period of time 
## by iteratively getting 1 month worth of data 
get_longer_eq_data <- function(start_time, end_time){
  if (!(is.Date(start_time) & is.Date(end_time))){
    stop('Enter in proper dates for start and end time')
     
  }
  start_time <- as.Date(start_time)
  end_time <- as.Date(end_time)
  
  data_list <- list()
  
  ## Prepare for for loop
  loop_start = start_time
  i <- 1
  
  while (loop_start < end_time){
    ## start with earliest
    loop_end <- min(loop_start + days(29), end_time)
    ## Call API function 
    data <- get_eq_data(loop_start, loop_end)
    ## DATA CLEANING STEPS 
    features <-(data$features)
    properties_df <- data.frame(features$properties)
    geometry_df <- data.frame(features$geometry)
    combined_df <- cbind(properties_df, geometry_df)
    
    ## Append cleaned df into data_list
    data_list[[i]] <- combined_df
    
    ## Reset variables for next iteration
    loop_start = loop_end + days(1)
    Sys.sleep(5)#
    i <- i+1
  }
  
  full_data <- bind_rows(data_list) ## bind many dfs by row 
  return(full_data)
}

## Test longer data 
long_data <- get_longer_eq_data(n_days_ago(100), today)
## check 
dim(long_data)
format(as.Date(range(as.POSIXct(long_data$time/1000, start = '1970-01-01'))), '%d %b %Y')

## OLDER METHOD OF FETCHING LIMITED DATA ========================================
## Fetch data with user defined function
# data <- get_eq_data(n_days_ago(30),today)
# ## One level down 
# features <-(data$features)
# ## Two levels down 
# names(features)
# properties_df <- data.frame(features$properties)
# geometry_df <- data.frame(features$geometry)
# dim(properties_df) == dim(geometry_df)
## Concatenate both properties and geometry dfs 
# combined_df <- cbind(properties_df, geometry_df)
# dim(combined_df) == dim(properties_df) ## same number of rows 

# response <- GET(query_url)
# data <- fromJSON(content(response, "text"), flatten = TRUE)
# earthquakes <- data$features

## Simple data exploration / Data cleaning =====================================================

cleaning_function <- function(df){
  
  ##1. remove useless cols (cols with only 1 value) 
  more_than_1_cols <- sapply(df, function(x) length(unique(x)) > 1)
  df <- df[more_than_1_cols]
  
  ##2. clean dates
  df$time <- as.POSIXct(df$time/1000, origin = '1970-01-01') ## time are milliseconds since epoch i.e. 1 jan 1970 -- convert to seconds
  df$date <- as.Date(df$time)
  
  ##3. extract coordinates 
  df$longitude <- sapply(df$coordinates, FUN = function(x) x[1])
  df$latitude <- sapply(df$coordinates, FUN = function(x) x[2])
  df$depth <- sapply(df$coordinates, FUN = function(x) x[3])

  ##4. extract place
  df$place <- as.character(df$place)
  # Extract everything after the first comma and trim whitespace
  df$country_US_state <- trimws(sub(".*?,", "", df$place))
  
  return(df)
  
}

## PLACES =============================== 
## Examining places in df 
earthquakes_df <- cleaning_function(long_data)
places_unique <- unique(earthquakes_df$country_US_state)

## Cleaning places in df 
# note: I chose to exclude alaska and hawaii as it had so many of its own earthquakes, hence
# i wanted to preserve that level of granularity instead of just including into a 
# massive 'US' category
us_states <- c("Alabama",  "Arizona", "Arkansas", "California", "Colorado",
               "Connecticut", "Delaware", "Florida", "Georgia", "Idaho",
               "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
               "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
               "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
               "New Hampshire", "New Jersey", "New Mexico", "New York",
               "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
               "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
               "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
               "West Virginia", "Wisconsin", "Wyoming")
state_abbr <- c("AL",  "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
                 "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
                "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
                "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
ocean_keywords <- c('ridge', 'trench', 'ocean', 'sea', 'gulf', "island", "islands",
                    "lake", "bay", "coast", "plateau", "rise")
ocean_patterns <- paste0("\\b", ocean_keywords, "\\b")

categorise_place <- function(place_name){
  place_name <- trimws(tolower(place_name))
  
  ## 1. CATEGORISE AS UNITED STATES FOR MOST STATES 
  if (place_name %in% tolower(us_states) || place_name %in% tolower(state_abbr)){
    return('United States')
  }
  
  ## 2. RETURN STANDARD NAME OF COUNTRY 
  country <- countrycode(place_name, origin = 'country.name', 
                         destination = 'country.name', 
                         warn = F, 
                         nomatch = NA)
  # if valid country
  if (!is.na(country)){
    return(country)
  }
  
  ## 3. OCEANIC REGIONS 
  
  if (any(stringr::str_detect(place_name, regex(ocean_patterns, ignore_case = T)))){
    return('Oceanic Region')
  }
  
  ## Default otherwise
  return('Non-identified countries')
}

## Apply function
s = Sys.time()
earthquakes_df$place_cleaned <- sapply(earthquakes_df$country_US_state, categorise_place)
e = Sys.time()
e-s


## Missing values analysis ====================
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

## remove those without magnitudes 
earthquakes_df <- earthquakes_df[!is.na(earthquakes_df$mag), ]

ui <- fluidPage(
  titlePanel("Interactive Earthquake Data Explorer"),
  
  ## Side bar for inputs : range for magnitude and for dates
  sidebarLayout(
    sidebarPanel(
      ## for magnitude
      sliderInput("magRange", 
                 "Magnitude Range",
                 min = 0, 
                 max = 8,
                 value = c(0,8), 
                 step = 0.1),
      ## for date
      dateRangeInput("dateRange", ## enter date range to filter earthquakes
                     "Date Range", 
                     start = min(earthquakes_df$time),
                     end = max(earthquakes_df$time)
                     ),
      ## for tsunami related earthquakes (binary)
      checkboxInput('showTsunami', "Show Only Tsunami-Related Earthquake")
      ),
       
  
  mainPanel(
    tabsetPanel( ## organises outputs into TABS for easier management
      ## first tab
      tabPanel("Map", ## name of the output tab
               ## insert plot object 
               leafletOutput("quakeMap", ##output name (from server)
                             height = 800)), ## all these ids will come from server functions
      ## second tab -- 2 plots
      tabPanel('Places with most earthquakes', 
               plotOutput('wordCloud', height = '500px'),
               plotOutput('barPlot', height = '400px')
               ),
      ## third tab
      tabPanel("Magnitude distribution", 
               plotlyOutput("magPlot")), 
      tabPanel("Depth Distribution", 
               plotlyOutput("depthPlot"), 
               plotOutput('depthBoxplot')), 
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
  
  ## 1.2: WORD CLOUD =================================================
  output$wordCloud <- renderPlot({
    req(filtered_data())
    category_counts <- filtered_data() %>% count(country_US_state)
    
    set.seed(42)
    wordcloud(words = category_counts$country_US_state, 
              freq = category_counts$n, 
              scale = c(5,1), 
              colors = brewer.pal(8, "Dark2"), 
              random.order = FALSE) ## number of colors in palette
  })
  
  ## 1.3: BARPLOT =======================================================
  output$barPlot <- renderPlot({
    req(filtered_data()) 

      top_places <- filtered_data() %>%
      count(place_cleaned) %>%
      arrange(desc(n)) %>%
      slice_max(order_by = n, n = 5)
      
    
    ggplot(top_places, aes(x = reorder(place_cleaned, n), y = n, fill = place_cleaned)) +
      geom_bar(stat = "identity") +
      coord_flip() +  # Flip coordinates to make it horizontal
      labs(title = "Most Common Major Regions", x = "Region", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none")  # Hide the legend
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
  
  ## 2.2: Boxplot of DEPTH -- to show how many outliers there are 
  output$depthBoxplot <- renderPlot({
    ggplot(filtered_data(), aes(x = depth)) +
      geom_boxplot(fill = 'skyblue', color = 'black', outlier.shape = 4) +
      labs(y = '    ')
      theme_bw() 
  })
  
  ## THIRD OUTPUT: DEPTH DISTRIBUTION
  output$depthPlot <- renderPlotly({
    filtered_data <- filtered_data()
    q <- ggplot(filtered_data, aes(x= depth)) +
      geom_histogram(bins = 25)+
      geom_text(x = 500, y= 15000, 
                label= paste('Mean:', round(mean(filtered_data$depth, na.rm =T), 2), '\n', 
                             'Median:', median(filtered_data$depth, na.rm = T))) +
      theme_bw()

    ## Insert ggplot object into plotly for interactive functionality
    ggplotly(q)
    
  })
  
  ## get top 5 most common earthquake areas 
  top_places_data <- reactive({
    data <- filtered_data()
    
    top_places <- data %>% count(place_cleaned, sort = T) %>% 
      top_n(8,n) %>% 
      pull(place_cleaned)
    
    # set the rest to others
    data <- data %>% mutate(region = ifelse(place_cleaned %in% top_places,
                                                   place_cleaned, 'Other'))
    
    return(data)
    
    })
  
  ## FOURTH OUTPUT: MAG-DEPTH SCATTERPLOT ======================================
  output$magdepthPlot <- renderPlotly({
   o <- ggplot(top_places_data(), aes(x = depth, y = mag))+
     geom_point(aes(color = (region)),
                size = 0.5, alpha = 0.5) +
     geom_smooth(aes(group = 1), ## in order to plot one geom_smooth line instead of many (one per region) 
                 method = 'loess',color = 'darkblue',
                 method.args = list(degree = 1))+
     guides(color = guide_legend(title = 'Region'))+
     # geom_smooth(method = 'lm',color = 'lightblue')+
     ## add legend for both lines +
     theme_classic()
   
   ggplotly(o)
  })
  
  ## FIFTH OUTPUT: SUMMARY TABLE
  output$dataTable <- renderDataTable({
    summary(filtered_data() %>%
      select(time, mag, depth, place, tsunami))
      
  })
}

shinyApp(ui = ui, server = server)



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
# 
# server_2 <- function(input, output, session){
#   
#   # Filtering based on user inputs
#   filtered_data <- reactive({
#     data <- earthquakes_df %>% filter(
#       mag >= input$magRange[1],
#       mag <= input$magRange[2], 
#       time >= input$dateRange[1], 
#       time <= input$dateRange[2]
#     )
#     if (input$showTsunami){ 
#       data <- data %>% filter(tsunami == 1)
#     }
#     data
#   })
#   
#   # Initialize the map
#   output$quakeMap <- renderLeaflet({
#     leaflet() %>% 
#       addTiles() %>% 
#       setView(lng = mean(earthquakes_df$longitude, na.rm = TRUE), 
#               lat = mean(earthquakes_df$latitude, na.rm = TRUE), 
#               zoom = 2) %>%
#       addLegend(position = 'bottomright', 
#                 colors = c('red', 'orange', 'green'),
#                 labels = c('Magnitude >5', 'Magnitude 3-5', 'Magnitude <3'),
#                 title = "Earthquake Magnitude")
#   })
#   
#   # Update the map based on filtered data
#   observe({
#     data <- filtered_data()
#     
#     leafletProxy("quakeMap", data = data) %>%
#       clearMarkers() %>%
#       clearShapes() %>%
#       addCircles(
#         lng = ~longitude, 
#         lat = ~latitude, 
#         radius = ~ifelse(mag < 1, 20000, mag * 20000),
#         color = ~ifelse(mag > 5, 'red', ifelse(mag > 3, 'orange', 'green')),
#         stroke = FALSE,
#         fillOpacity = 0.6,
#         popup = ~paste0(
#           "<strong>Location: </strong>", place, "<br>", 
#           "<strong>Magnitude: </strong>", mag, "<br>", 
#           "<strong>Depth: </strong>", depth, "<br>", 
#           "<strong>Time: </strong>", time, "<br>"
#         )
#       )
#   })
# }
# 
# ggplot(earthquakes_df, aes(x= depth, y = 1)) +
#   geom_jitter(alpha = 0.1)+
#   ylim(c(0,2)) +
#   labs(y = '')+
#   theme(axis.ticks.y = element_blank(),
#         axis.text.y = element_blank(), 
#         panel.background = element_rect(fill = 'white'), 
#         panel.grid.major = element_line(size = 0.4, 
#                                         color = 'grey'))
# 
# ggplot(earthquakes_df, aes(x= depth))+
#   geom_histogram()
