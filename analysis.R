##--load the libraries--------------------------------------------------------
library(ggplot2)
library(dplyr) #tables
library(maps)
library(plotly)
library(tidyr)
library(ggmap) #map
library(leaflet) #interactive map
library(kableExtra) #data table
library(lintr) #checks for errors
library(RColorBrewer) #colour palette

##--read data------------------------------------------------------------------------------------------------------------------------
data18 <- read.csv("data/shootings-2018.csv", stringsAsFactors = FALSE) #2018 shootings
data20 <- read.csv("data/gunarchive2020.csv", stringsAsFactors = FALSE) #2020 shootings
stateLocation <- read.csv("data/statelatlong.csv", stringsAsFactors = FALSE) # stateLocation (long/lat)
state <- read.csv("data/statedata.csv", stringsAsFactors = FALSE) #census data
state_two <- read.csv("data/kaggle_income.csv", stringsAsFactors = FALSE) #average family income data

##--cleaning--------------------------------------------------------------------------------------------------------------------------
colnames(stateLocation)[which(names(stateLocation) == "City")] <- "State" #change col name city to state for joining
stateLocation <- stateLocation[, -1] #drop first state symbol

## data 20----------
#drop columns
data20 <- select(data20, -City.Or.County)
data20 <- select(data20, -Address)
data20 <- select(data20, -Operations)

#join with state long/lat info
data20 <- inner_join(data20, stateLocation, by="State")

#to get total number of people killed and injured
data20 <- data20 %>% 
  group_by(State) %>%
  summarize(X..Killed = sum(X..Killed),
            X..Injured = sum(X..Injured))

#join with state long/lat info
data20 <- inner_join(data20, stateLocation, by="State")

#rename columns
colnames(data20)[which(names(data20) == "X..Killed")] <- "total_killed" #change name
colnames(data20)[which(names(data20) == "X..Injured")] <- "total_injured" #change name
colnames(data20)[which(names(data20) == "Longitude")] <- "long" #change name
colnames(data20)[which(names(data20) == "Latitude")] <- "lat" #change name

## state_two-------------
state_two <- select(state_two, "State_Name", Mean)

#summarise averages
state_two <- state_two %>%
  group_by(State_Name)  %>%
  summarize(Mean = mean(Mean))

#change names
colnames(state_two)[which(names(state_two) == "State_Name")] <- "state" #change name
colnames(state_two)[which(names(state_two) == "Mean")] <- "avg_income" #change name

## state----------------
#change name
colnames(state)[which(names(state) == "state.name")] <- "state" #change name

#join with state
state <- inner_join(state_two, state, by="state")

#delete columns
state <- select(state, -c(state.division, state.region, state.area, state.abb, x, y, Area, Frost))

#join data18 with state
joined <- inner_join(data18, state, by="state") #joined df

#data for 2018
joined2018 <- joined %>%
  group_by(state) %>%
  summarize(num_killed = sum(num_killed),
            num_injured = sum(num_injured),
            avg_income = mean(avg_income)
            )

#rename columns
colnames(joined2018)[which(names(joined2018) == "state")] <- "State" #change name

#join states
joined2018 <- inner_join(joined2018, stateLocation, by="State")

colnames(joined2018)[which(names(joined2018) == "Longitude")] <- "long" #change name
colnames(joined2018)[which(names(joined2018) == "Latitude")] <- "lat" #change name

##--summary statistics--------------------------------------------------------------------------------------------------------------------------

#2018---------------------------
joined18_2 <- joined2018[with(joined2018,order(-num_killed)),] #arr by num_killed
joined18_2 <- joined18_2[1:10,] #top 15

#visualised 2018
bar2018 <- ggplot(joined18_2, aes(x = reorder(State, -num_killed), y = num_killed, fill = num_injured)) + 
  geom_bar(stat = "identity") + 
  labs(x = "State", fill = "Number of people injured", y = "Number of people killed") +
  ggtitle("Top 10 States with highest number of people killed")

#2020---------------------------
data18_nrow <- nrow(data18)
data20_nrow <- nrow(data20)
data2020bar <- data20[with(data20,order(-total_killed)),] #arrange by total_killed
data2020bar <- data2020bar[1:10,] #top 15

#visualised 2020
bar2020 <- ggplot(data2020bar, aes(x = reorder(State, -total_killed), y = total_killed, fill = total_injured)) + 
  geom_bar(stat = "identity") + 
  labs(x = "State", fill = "Number of people injured", y = "Number of people killed") +
  ggtitle("Top 10 States with highest number of people killed (2020)")

data20_sumkilled <- sum(data20$total_killed)
data18_sumkilled <- sum(joined$num_killed)
data20_suminj <- sum(data20$total_injured)
data18_suminj <- sum(joined$num_injured)

#compare 2018 and 2020 shooting trends (data frame)
compare18_20 <- data.frame(
  year = c(2018, 2020),
  total_killed = c(data18_sumkilled, data20_sumkilled),
  total_injured = c(data18_suminj, data20_suminj),
  total_shooting_incidences = c(data18_nrow, data20_nrow)
)

#visualised 2018/2020
barcompare <- ggplot(compare18_20, aes(x = year, y = total_killed, fill = total_injured)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Year", fill = "Number of people injured", y = "Number of people killed") +
  ggtitle("Comparison of shooting incidences across 2018 and 2020")


##--visualising--------------------------------------------------------------------------------------------------------------------------

#2020 interactive map---------
#colour palette
pal2020 <- colorNumeric(
  palette = "Reds",
  domain = data20$total_injured)

#map
interactivemap2020 <- leaflet(data = data20) %>%
  #map
  addProviderTiles("CartoDB.Positron") %>%
  #initial zoom
  fitBounds(lng1 = min(data18$long),
            lat1 = min(data18$lat),
            lng2 = max(data18$long),
            lat2 = max(data18$lat)) %>%
  #adds the datapoints
  addCircleMarkers(
    lat = ~lat,
    lng = ~long,
    weight = 1,
    # interactive popup information with
    # address, date, no. killed and injured
    popup = paste(#"<b> Location: </b>" , data18$address, "<br>",
      '<b> <span style = "color: #800015; text-transform: uppercase;">',
      "<b> Number of people injured: </b>", data18$num_injured, "<br>",
      "<b> Number of people killed: </b>", data18$num_killed, "<br>"),
    radius = ~total_killed,
    stroke = TRUE,
    color = "Red",
    fillOpacity = 0.3,
    fillColor = ~pal2020(total_injured) #colour based on number of people killed
  ) %>%
  #scale bar
  addScaleBar(position = c("topright", "bottomright", "bottomleft",
                           "topleft"), options = scaleBarOptions(maxWidth = 200, metric = TRUE, imperial = TRUE,
                                                                 updateWhenIdle = TRUE)) %>%
  #state names
  addLabelOnlyMarkers(~long, ~lat, label =  ~as.character(State), 
                      labelOptions = labelOptions(noHide = T, direction = 'middle', textOnly = T)) %>%
  #legend
  addLegend(
    position = "bottomright",
    title = "Total number of <br> people harmed",
    pal = pal2020, # the color palette described by the legend
    values = ~total_injured, # the data values described by the legend
    opacity = 1
  )