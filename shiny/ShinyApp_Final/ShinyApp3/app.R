library(shiny)
library(tidyverse)
library(bslib)
library(dplyr)
library(plotly)
library(sf)
library(tmap)
library(DT)
library(corrplot)
library(gplots)
library(forcats)
library(shinydashboard)
library(lubridate)
library(naniar)
library(stringi)
library(MASS)
library(car)
library(stats)
library(ISLR)
library(jtools)

thematic::thematic_shiny()

A2020 <- read_csv("data/road-casualty-statistics-accident-2020.csv")
A2019 <- read_csv("data/road-casualty-statistics-accident-2019.csv")
A2018 <- read_csv("data/road-casualty-statistics-accident-2018.csv")
A2017 <- read_csv("data/road-casualty-statistics-accident-2017.csv")
AA2020 <- filter(A2020, local_authority_ons_district %in% c("E09000001", "E09000002","E09000003","E09000004",
                                                            "E09000005","E09000006","E09000007","E09000008",
                                                            "E09000009","E09000010","E09000011","E09000012",
                                                            "E09000013","E09000014","E09000015","E09000016",
                                                            "E09000017","E09000018","E09000019","E09000020",
                                                            "E09000021","E09000022","E09000023","E09000024",
                                                            "E09000025","E09000026","E09000027","E09000028",
                                                            "E09000029","E09000030","E09000031","E09000032",
                                                            "E09000033"))
AA2019 <- filter(A2019, local_authority_ons_district %in% c("E09000001", "E09000002","E09000003","E09000004",
                                                            "E09000005","E09000006","E09000007","E09000008",
                                                            "E09000009","E09000010","E09000011","E09000012",
                                                            "E09000013","E09000014","E09000015","E09000016",
                                                            "E09000017","E09000018","E09000019","E09000020",
                                                            "E09000021","E09000022","E09000023","E09000024",
                                                            "E09000025","E09000026","E09000027","E09000028",
                                                            "E09000029","E09000030","E09000031","E09000032",
                                                            "E09000033"))
AA2018 <- filter(A2018, local_authority_ons_district %in% c("E09000001", "E09000002","E09000003","E09000004",
                                                            "E09000005","E09000006","E09000007","E09000008",
                                                            "E09000009","E09000010","E09000011","E09000012",
                                                            "E09000013","E09000014","E09000015","E09000016",
                                                            "E09000017","E09000018","E09000019","E09000020",
                                                            "E09000021","E09000022","E09000023","E09000024",
                                                            "E09000025","E09000026","E09000027","E09000028",
                                                            "E09000029","E09000030","E09000031","E09000032",
                                                            "E09000033"))
AA2017 <- filter(A2017, local_authority_ons_district %in% c("E09000001", "E09000002","E09000003","E09000004",
                                                            "E09000005","E09000006","E09000007","E09000008",
                                                            "E09000009","E09000010","E09000011","E09000012",
                                                            "E09000013","E09000014","E09000015","E09000016",
                                                            "E09000017","E09000018","E09000019","E09000020",
                                                            "E09000021","E09000022","E09000023","E09000024",
                                                            "E09000025","E09000026","E09000027","E09000028",
                                                            "E09000029","E09000030","E09000031","E09000032",
                                                            "E09000033"))
accidentfjh <- rbind(AA2020, AA2019, AA2018, AA2017)


mpld <- st_read(dsn = "data/geospatial", 
                layer = "London_Borough_Excluding_MHW")

accident_2020 <- read_csv("data/dft-road-casualty-statistics-accident-2020.csv")
accident_2019 <- read_csv("data/dft-road-casualty-statistics-accident-2019.csv")
accident_2018 <- read_csv("data/dft-road-casualty-statistics-accident-2018.csv")
accident_2017 <- read_csv("data/dft-road-casualty-statistics-accident-2017.csv")
accident <- rbind(accident_2020, accident_2019, accident_2018, accident_2017)


greater_london_division <- read_csv("data/Local_Authority_District_to_County_(April_2020)_Lookup_in_England.csv")
joint <- accident %>% left_join(greater_london_division, by = c ('local_authority_ons_district' = 'LAD20CD'))


accident_greater_london_2 <- joint %>%
  filter(LAD20NM == 'City of London'|
           LAD20NM == 'Westminster'|
           LAD20NM == 'Kensington and Chelsea'|
           LAD20NM == 'Hammersmith and Fulham'|
           LAD20NM == 'Wandsworth'|
           LAD20NM == 'Lambeth'|
           LAD20NM == 'Southwark'|
           LAD20NM == 'Tower Hamlets'|
           LAD20NM == 'Hackney'|
           LAD20NM == 'Islington'|
           LAD20NM == 'Camden'|
           LAD20NM == 'Brent'|
           LAD20NM == 'Ealing'|
           LAD20NM == 'Hounslow'|
           LAD20NM == 'Richmond upon Thames'|
           LAD20NM == 'Kingston upon Thames'|
           LAD20NM == 'Merton'|
           LAD20NM == 'Sutton'|
           LAD20NM == 'Croydon'|
           LAD20NM == 'Bromley'|
           LAD20NM == 'Lewisham'|
           LAD20NM == 'Greenwich'|
           LAD20NM == 'Bexley'|
           LAD20NM == 'Havering'|
           LAD20NM == 'Barking and Dagenham'|
           LAD20NM == 'Redbridge'| 
           LAD20NM == 'Newham'|
           LAD20NM == 'Waltham Forest'|
           LAD20NM == 'Haringey'|
           LAD20NM == 'Enfield'|
           LAD20NM == 'Barnet'|
           LAD20NM == 'Harrow'|
           LAD20NM == 'Hillingdon')


accident_greater_london_2017_2020_2 <- left_join(mpld,
                                                 accident_greater_london_2,
                                                 by = c("NAME" = "LAD20NM"))


#data processing for regression and time series
path = "data/dft-road-casualty-statistics-accident-"

read_files <- function(year){
  data <- read_csv(paste(path,year,".csv", sep =""))
}

#yr16 <- read_files("2016")
yr17 <- read_files("2017")
yr18 <- read_files("2018")
yr19 <- read_files("2019")
yr20 <- read_files("2020")

data_regts <- rbind(yr17,yr18,yr19,yr20)

#data processing
greaterlondon_postcodes <- c(
  "E09000001","E09000002","E09000003","E09000004","E09000005","E09000006",
  "E09000007","E09000008","E09000009","E09000010","E09000011","E09000012",
  "E09000013","E09000014","E09000015","E09000016","E09000017","E09000018",
  "E09000019","E09000020","E09000021","E09000022","E09000023","E09000024",
  "E09000025","E09000026","E09000027","E09000028","E09000029","E09000030",
  "E09000031","E09000032","E09000033"
)


data_gl <- data_regts %>% filter(local_authority_ons_district %in% greaterlondon_postcodes)


##excluding not necessary columns
data_gl <- data_gl %>% subset(
  select = -c(police_force,local_authority_district,local_authority_highway,
              first_road_number, second_road_number,location_easting_osgr,
              location_northing_osgr,longitude,latitude))

#transforming date variables into correct type
data_gl$date <- dmy(data_gl$date, tz = "Europe/London")
data_gl$day_of_week <- wday(data_gl$date, label=TRUE)
data_gl$accident_year <- format(data_gl$date, "%Y")

##Matching London Postcode to Boroughs
greaterlondon_boroughs <- c(
  "City of London","Barking and Dagenham","Barnet","Bexley","Brent","Bromley",
  "Camden","Croydon","Ealing","Enfield","Greenwich","Hackney",
  "Hammersmith and Fulham","Haringey","Harrow","Havering","Hillingdon",
  "Hounslow","Islington","Kensington and Chelsea","Kingston upon Thames",
  "Lambeth","Lewisham","Merton","Newham","Redbridge","Richmond upon Thames",
  "Southwark","Sutton","Tower Hamlets","Waltham Forest","Wandsworth","Westminster"
)

data_gl$borough <- data_gl$local_authority_ons_district
data_gl$borough <- data_gl$borough %>% stri_replace_all_regex(
  greaterlondon_postcodes,
  greaterlondon_boroughs,
  vectorize=F)
###source: https://www.roelpeters.be/combining-gsubs-to-replace-multiple-patterns-in-r-solved/

## Matching other numerical columns with their string meanings
### function to map numeric values onto string
num_to_str <- function(numlist,strlist,columnname){
  c_str <- as.character(data_gl[[columnname]])
  c_str %>%
    stri_replace_all_regex(
      as.character(numlist),
      strlist,
      vectorize=F
    )
}

### lists to match numeric and string values
frc_num <- c(1,2,3,4,5,6)
frc_str <- c("Motorway", "A(M)", "A", "B", "C", "Unclassified")

rt_num <- c(-1,1,2,3,6,7,9,12)
rt_str <- c(NA,"Roundabout","One way street","Dual carriageway","Single carriageway",
            "Slip road","Unknown","One way street/Slip road")

jd_num <- c(-1,99,0,1,2,3,5,6,7,8,9)
jd_str <- c(NA,NA,"Not at junction or within 20 metres","Roundabout","Mini-roundabout",
            "T or staggered junction","Slip road","Crossroads",
            "More than 4 arms (not roundabout)","Private drive or entrance",
            "Other junction")

jc_num <-c(-1,9,0,1,2,3,4)
jc_str <-c(NA,NA,"Not at junction or within 20 metres","Authorised person",
           "Auto traffic signal","Stop sign","Give way or uncontrolled")

src_num <- c(0,1,2,3,4,5,6)
src_str <- c("Not at junction or within 20 metres", "Motorway", "A(M)", 
             "A", "B", "C", "Unclassified")

pchc_num <- c(-1,9,0,1,2)
pchc_str <- c(NA,NA,"None within 50 metres ","Control by school crossing patrol",
              "Control by other authorised person")

pcpf_num <- c(-1,9,0,1,4,5,7,8)
pcpf_str <- c(NA,NA,"No physical crossing facilities within 50 metres","Zebra",
              "Pelican, puffin, toucan or similar non-junction pedestrian light crossing",
              "Pedestrian phase at traffic signal junction",
              "Footbridge or subway","Central refuge")

lc_num <- c(-1,1,4,5,6,7)
lc_str <- c(NA,"Daylight","Darkness - lights lit","Darkness - lights unlit",
            "Darkness - no lighting","Darkness - lighting unknown")

wc_num <- c(-1,9,1,2,3,4,5,6,7,8)
wc_str <- c(NA,NA,"Fine no high winds","Raining no high winds","Snowing no high winds",
            "Fine + high winds","Raining + high winds","Snowing + high winds",
            "Fog or mist","Other")

rsc_num <- c(-1,9,1,2,3,4,5,6,7)
rsc_str <- c(NA,NA,"Dry","Wet or damp","Snow","Frost or ice","Flood over 3cm. deep",
             "Oil or diesel","Mud")

scas_num <- c(-1,9,0,1,2,3,4,5,6,7)
scas_str <- c(NA,NA,"None","Auto traffic signal - out","Auto signal part defective",
              "Road sign or marking defective or obscured",
              "Roadworks","Road surface defective","Oil or diesel","Mud")

ch_num <- c(-1,9,0,1,2,3,4,5,6,7)
ch_str <- c(NA,NA,"None","Vehicle load on road","Other object on road",
            "Previous accident","Dog on road","Other animal on road",
            "Pedestrian in carriageway - not injured",
            "Any animal in carriageway (except ridden horse)")

ura_num <- c(-1,1,2,3)
ura_str <- c(NA,"Urban","Rural","Unallocated")

dpoasa_num <- c(-1,1,2,3)
dpoasa_str <- c(NA,"Yes","No",
                "No - accident was reported using a self completion  form (self rep only)")

trf_num <- c(-1,1,2)
trf_str <- c(NA,"Trunk (Roads managed by Highways England)","Non-trunk")


###composite list of lists for numeric and string values for all related variables
numlists <- list(frc_num,rt_num,jd_num,jc_num,src_num,pchc_num,pcpf_num,lc_num,
                 wc_num,rsc_num,scas_num,ch_num,ura_num,dpoasa_num,trf_num)

strlists <- list(frc_str,rt_str,jd_str,jc_str,src_str,pchc_str,pcpf_str,lc_str,
                 wc_str,rsc_str,scas_str,ch_str,ura_str,dpoasa_str,trf_str)

###list of all related variable names
varnames <- c("first_road_class","road_type","junction_detail", 
              "junction_control","second_road_class",
              "pedestrian_crossing_human_control",
              "pedestrian_crossing_physical_facilities","light_conditions",
              "weather_conditions","road_surface_conditions",
              "special_conditions_at_site","carriageway_hazards",
              "urban_or_rural_area","did_police_officer_attend_scene_of_accident",
              "trunk_road_flag")

##for loop to automate matching of numeric to string values within the dataframe
for (i in 1:length(numlists)){
  data_gl[varnames[i]] <- num_to_str(numlists[[i]],strlists[[i]],varnames[i])
}

##placing missing values in numerical variables at appropriate positions
data_gl <- data_gl %>% replace_with_na(replace = list(speed_limit = c(-1,99)))
###source: https://cran.r-project.org/web/packages/naniar/vignettes/replace-with-na.html

data_gl <- drop_na(data_gl)

print(data_gl)

ts <- data_gl[,c("date","accident_reference","accident_severity",
                 "number_of_casualties")]

ts_grouped <- ts %>% group_by(date) %>% dplyr::summarise(
  "count_accidents" = n(),
  "accident_severity" = mean(accident_severity),
  "number_of_casualties" = mean(number_of_casualties))
ts_grouped$monthyear <- format(ts_grouped$date, "%Y/%m")

ts_grouped_grouped <- ts_grouped %>% group_by(monthyear) %>% summarise(
  "no_accidents" = sum(count_accidents),
  "accident_severity" = mean(accident_severity),
  "number_of_casualties" = mean(number_of_casualties)
)



ui <- dashboardPage(skin = 'blue',
                    dashboardHeader(title = 'Car Accident in Greater London', titleWidth = 350),
                    dashboardSidebar(width = 300,
                                     sidebarMenu(id = 'sbm',
                                                 menuItem('Exploratory Data Analysis', tabName = 'EDA', icon = icon('signal')),
                                                 menuItem('Geovisual Analysis', tabName = 'GeoVisual', icon = icon('map')),
                                                 menuItem('Regression Analysis', tabName = "Regression", icon = icon("chart-line")),
                                                 menuItem('Time-Series Analysis', tabName = "Time-Series", icon = icon("calendar"))
                                     )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = 'EDA',
                                fluidPage(
                                  titlePanel("Exploratory Data Analysis (EDA)"),
                                  fluidRow(column(width = 6,
                                                  selectInput(inputId = "cat1",
                                                              label = "Environment conditions:",
                                                              choices = c("light conditions" = "light_conditions",
                                                                          "weather conditions" = "weather_conditions",
                                                                          "day of week" = "day_of_week",
                                                                          "road type" = "road_type",
                                                                          "speed limit" = "speed_limit",
                                                                          "pedestrian crossing human control" = "pedestrian_crossing_human_control",
                                                                          "road surface conditions" = "road_surface_conditions",
                                                                          "special conditions at site" = "special_conditions_at_site",
                                                                          "carriageway hazards" = "carriageway_hazards",
                                                                          "urban or rural area" = "urban_or_rural_area"),
                                                              selected = "light_conditions"),
                                                  selectInput(inputId = "cat2",
                                                              label = "Accident conditions:",
                                                              choices = c("accident severity" = "accident_severity",
                                                                          "number of casualties" = "number_of_casualties",
                                                                          "number of vehicles" = "number_of_vehicles"),
                                                              selected = "accident_severity"),
                                                  submitButton()
                                                  
                                  )),
                                  fluidRow(column(width = 8,offset = 0,
                                                  div(style = "height:50px;width:100%",tags$h3("Univariate Analyis on Distribution of Accident Count")),
                                                  box(width = 12,
                                                      height = 420,
                                                      solidHeader = TRUE,
                                                      collapsible = FALSE,
                                                      collapsed = FALSE,plotOutput("barplot1"))),
                                          column(width = 4,offset = 0,
                                                  div(style = "height:50px;width:100%",tags$h3(" ")),
                                                  box(width = 12,
                                                      height = 420,
                                                      solidHeader = TRUE,
                                                      collapsible = FALSE,
                                                      collapsed = FALSE,plotOutput("barplot2")))),
                                  fluidRow(column(width = 12,offset = 0,
                                                  div(style = "height:30px;width:100%",tags$h3("Chi-square Test Results on Chosen Variables")),
                                                  box(width = 12,
                                                      height = 50,
                                                      solidHeader = TRUE,
                                                      collapsible = FALSE,
                                                      collapsed = FALSE,tableOutput("results")))),
                                  
                                  fluidRow(column(width = 12,
                                                  div(style = "height:30px;width:100%",tags$h3("Balloon Plot of Frequency Table")),
                                                  box(width = 12,
                                                      height = 420,
                                                      solidHeader = TRUE,
                                                      collapsible = FALSE,
                                                      collapsed = FALSE,plotOutput("balloonplot")))),
                                  fluidRow(column(width = 12,
                                                  div(style = "height:30px;width:100%",tags$h3("Corrplot on Chi-square Residuals")),
                                                  box(width = 12,
                                                      height = 420,
                                                      solidHeader = TRUE,
                                                      collapsible = FALSE,
                                                      collapsed = FALSE,plotOutput("corrplot"))),
                                  
                                  fluidRow(column(width = 12,offset = 0,
                                                  div(style = "height:30px;width:100%",tags$h3("Chi-square Frequency Table")),
                                                  tableOutput("tables")))
                                ) # end of fluidPage
                        )), # end of tabItem for Introduction
                        tabItem(tabName = 'GeoVisual',
                                fluidPage(
                                  titlePanel("Car Accident Analysis in Greater London"),
                                  sidebarLayout(
                                    sidebarPanel("Cluster Sidebar Panel", position='', width=3, fluid=TRUE,
                                                 selectInput(inputId = "class",
                                                             label = "Classification method:",
                                                             choices = list("Fixed" = "fixed", 
                                                                            "SD" = "sd", 
                                                                            "Equal" = "equal", 
                                                                            "Pretty" = "pretty", 
                                                                            "Quantile" = "quantile", 
                                                                            "Kmeans" = "kmeans", 
                                                                            "Hclust" = "hclust", 
                                                                            "Bclust" = "bclust", 
                                                                            "Fisher" = "fisher", 
                                                                            "Jenks" = "jenks"),
                                                             selected = "pretty"),
                                                 sliderInput(inputId = "classes",
                                                             label = "Number of classes",
                                                             min = 6,
                                                             max = 12,
                                                             value = c(6)),
                                                 selectInput(inputId = "Time",
                                                             label = "Year of Accidents",
                                                             choices = list("2017" = "2017", 
                                                                            "2018" = "2018", 
                                                                            "2019" = "2019",
                                                                            "2020" = "2020"),
                                                             multiple = FALSE,
                                                             selected = "2020"),
                                                 selectInput(inputId = "Severity",
                                                             label = "Accident Severity",
                                                             choices = list("Slight" = "3", 
                                                                            "Serious" = "2", 
                                                                            "Fatal" = "1"),
                                                             multiple = FALSE,
                                                             selected = "2"),
                                                 submitButton()
                                    ),
                                    mainPanel(width=9,
                                              fluidRow(
                                                column(6,
                                                       tmapOutput("mapPlot",
                                                                  width="700px",
                                                                  height="500px")
                                                )
                                              )
                        )
        ))), # end of tabItem for GeoVisual
        tabItem(tabName = "Regression",
                fluidPage(
                  
                  # Application title
                  titlePanel("Regression Analysis"),
                  # Sidebar with a slider input for number of bins 
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(inputId = "TV",
                                       label = "Target Variable",
                                       choices = c("Accident Severity" = "accident_severity",
                                                   "Number of Casualties" = "number_of_casualties"),
                                       selected = "number_of_casualties"),
                           selectInput(inputId = "IV",
                                       label = "Independent Variable",
                                       choices = c("No. of Vehicles" = "number_of_vehicles",
                                                   "Weekday" = "day_of_week",
                                                   "Primary Road Classification" = "first_road_class",
                                                   "Road Type" = "road_type",
                                                   "Speed Limit" = "speed_limit",
                                                   "Junction Info" = "junction_detail",
                                                   "Junction Control" = "junction_control",                           
                                                   "Secondary Road Classification" = "second_road_class",                        
                                                   "Human Control of Crossing" = "pedestrian_crossing_human_control",         
                                                   "Type of Pedestrian Crossing" = "pedestrian_crossing_physical_facilities",   
                                                   "Light Conditions"="light_conditions",                          
                                                   "Weather Conditions"="weather_conditions",                        
                                                   "Road Surface Conditions" = "road_surface_conditions",                    
                                                   "Special Conditions at Site" = "special_conditions_at_site",                 
                                                   "Carriageway Hazards" = "carriageway_hazards",                      
                                                   "Urban or Rural Area" = "urban_or_rural_area",                       
                                                   "Police Officer at Scene?"="did_police_officer_attend_scene_of_accident",
                                                   "Trunk Road?" = "trunk_road_flag"),
                                       multiple = TRUE,
                                       selected = "number_of_vehicles"),
                           submitButton()
                         ),
                         
                         # Show a plot of the generated distribution
                         mainPanel(verbatimTextOutput("SummaryL"),
                                   plotOutput("ModelL")),
                         ))), #end of tabItem for Regression and Time Series
        tabItem(tabName = "Time-Series",
                fluidPage(
                  
                  # Application title
                  titlePanel("Time Series Viz"),
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(inputId="yvar",
                                       label = "Y-Variable",
                                       choices = c("Number of Accidents"="no_accidents",
                                                   "Average Accident Severity"="accident_severity",
                                                   "Average Number of Casualties per Accident"="number_of_casualties"),
                                       selected ="no_accidents"),
                           selectInput(inputId="xvar",
                                       label = "X-Variable",
                                       choices = c("Time"="monthyear")),
                           submitButton()
                         ),
                         mainPanel(
                           plotOutput("TSGraph"))
                         ))) #end of tabItem for Regression and Time Series
  )))

                                                
  
server <- function(input, output) {
  
  
  output$barplot1 <- renderPlot({
    ggplot(data=accidentfjh,
           aes_string(x=input$cat1)) +
      theme(axis.text.x = element_text(angle = -20, hjust = 0))+
      labs(y = 'Accident Count')+
      geom_bar(fill="lightblue")
  })

  output$barplot2 <- renderPlot({
    ggplot(data=accidentfjh,
           aes_string(x = input$cat2)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      labs(y = 'Accident Count')+
      geom_bar(fill="#FF9999")
  })
  
  London_Accident <- reactive({
    req(input$cat1,input$cat2)
    accidentfjh  %>% {table(.[[input$cat1]], .[[input$cat2]])}
  })
  
  
  output$results <- renderPrint({
    print(chisq.test(London_Accident()))
  })
  
  
  output$tables <- renderTable({
    accidentfjh  %>% {table(.[[input$cat1]], .[[input$cat2]])}
  })
  
  output$balloonplot <- renderPlot({
    balloonplot(t(accidentfjh  %>% {table(.[[input$cat1]], .[[input$cat2]])}),main ="", xlab ="", ylab="",
                label = FALSE, show.margins = FALSE)
  })
  
  output$corrplot <- renderPlot({
    corrplot(chisq.test(London_Accident())$residuals, is.cor = FALSE, tl.col = "black")
  })
  
  data_input <- reactive({
    accident_greater_london_2017_2020_2 %>%
      filter(accident_year==input$Time) %>%
      filter(accident_severity==input$Severity) %>%
      group_by(NAME) %>%
      summarise(No_of_Accidents = n())
  })
  
  output$mapPlot <- renderTmap({
    tmap_options(check.and.fix = TRUE) +
      tm_shape(shp = data_input())+
      tm_fill("No_of_Accidents",
              n = input$classes,
              style = input$class,
              palette = "Blues") +
      tm_borders(lwd = 0.4,  alpha = 1)
  })
  
  output$ModelL <- renderPlot({
    
    par(mfrow=c(2,2))
    
    plot(glm(paste(input$TV,
                   "~",gsub(", ","+",toString(input$IV))),
             data = data_gl))
  })
  
  output$SummaryL <- renderPrint({
    summ(glm(paste(input$TV,
                   "~",gsub(", ","+",toString(input$IV))),
             data = data_gl),
         confint = TRUE)
  })
  
  output$TSGraph <- renderPlot({
    ts_grouped_grouped %>% ggplot(aes_string(input$xvar,input$yvar)) + geom_point() + 
      geom_line(aes(group=1)) + 
      geom_area() +
      theme(axis.text.x = element_text(angle = 90))
  })
  
}

shinyApp(ui = ui, server = server)