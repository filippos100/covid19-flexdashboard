library(shiny)
library(tidyverse)
library(DT)
library(scales)
library(utils)
library(lubridate)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(maps)
library(viridisLite)
library(shinydashboard)
library(plotly)

options(scipen = 999)

#Import data from the site.Import with tidyverse pack
data <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
#Create Date column as date class with libridate function
data <- data %>% mutate(Date = dmy(dateRep))

#Prepare data, create total cases, total deaths and mortality rate.
dat <- data %>% group_by(countriesAndTerritories) %>%
  summarise(Total_Cases = sum(cases),
            Total_Deaths = sum(deaths),
            Mortality = round(sum(deaths)/sum(cases)*100,2),
            Population = unique(popData2018),
            Cases_per_100k = round(Total_Cases/Population*100000,2),
            Deaths_per_100k = round(Total_Deaths/Population*100000,2))



#subset data for data table, probably it can be removed.
dat1 <- data %>% group_by(countriesAndTerritories) %>%
  select(Date,countriesAndTerritories,deaths,cases)

#create new column names country and change name of united states and united kingdom to prepare
#for join with dat and create the map.
dat2 <- dat %>%
  mutate(country=if_else(
    countriesAndTerritories=="United_States_of_America","USA",
    if_else(countriesAndTerritories=="United_Kingdom","UK",
            countriesAndTerritories)))

#Data for the map
dat_maps <- world.cities %>%
  filter(capital == 1) %>%
  dplyr::select(country = country.etc,lat,lng=long) %>%
  left_join(dat2,.,by=c("country"="country"))

#create palette for coloring the map circles
domain <- range(dat_maps$Mortality)
#pal <- colorNumeric(palette = inferno(100),domain = domain)

#####Application#############

ui <- dashboardPage(
  dashboardHeader(title = "COVID-19"),
  dashboardSidebar(
    selectInput("country","Select Country:",
                choices = data$countriesAndTerritories),
    sidebarMenu(
      menuItem("Global",tabName = "Global"),
      menuItem("Country",tabName = "Country"),
      menuItem("Map",tabName = "Map"),
      menuItem("Data",tabName = "Data")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Global",
              fluidRow(infoBoxOutput("progressbox"),
                       infoBoxOutput("progressbox1"),
                       infoBoxOutput("progressbox2"),
                       fluidPage(
                         plotlyOutput("plot1")
                       ),
                       fluidPage(
                         plotlyOutput("plot2")
                       )
              )),
      tabItem(tabName = "Country",
              fluidRow(infoBoxOutput("progressbox3"),
                       infoBoxOutput("progressbox4"),
                       infoBoxOutput("progressbox5"),
                       fluidPage(
                         plotlyOutput("plot3")
                       ),
                       fluidPage(
                         plotlyOutput("plot4")
                       )
              )),
      tabItem(tabName = "Map",
              mainPanel(leafletOutput("mymap",width = 1400,height = 600))
      ),
      
      tabItem(tabName = "Data",
              mainPanel(DT::dataTableOutput("datatable"))
      )
    )))




server <- function(input,output){
  
  output$progressbox5 <- renderInfoBox({
    infoBox(
      title = paste0(input$country," Mortality rate"),data %>% filter(countriesAndTerritories == input$country) %>%
        summarise(Mortality_Rate = round(sum(deaths)/sum(cases)*100,2))
    )
  })
  
  output$progressbox3 <- renderInfoBox({
    infoBox(
      title = paste0(input$country," Total Cases"),prettyNum(data %>% filter(countriesAndTerritories == input$country) %>%
                                                               summarise(Total_Cases = sum(cases)),big.mark = ",",scientific=FALSE)
    )
  })
  
  output$progressbox4 <- renderInfoBox({
    infoBox(
      title = paste0(input$country," Total Deaths"), prettyNum(data %>% filter(countriesAndTerritories == input$country) %>%
                                                                 summarise(Total_Deaths = sum(deaths)),big.mark = ",",scientific=FALSE)
    )
  })
  
  output$progressbox <- renderInfoBox({
    infoBox(
      "Total Cases",prettyNum(sum(dat$Total_Cases),big.mark = ",",scientific=FALSE),
      icon = icon("list")
    )
  })
  
  output$progressbox1 <- renderInfoBox({
    infoBox(
      "Total Deaths",prettyNum(sum(dat$Total_Deaths),big.mark = ",",scientific=FALSE),
      icon = icon("list")
    )
  })
  
  output$progressbox2 <- renderInfoBox({
    infoBox(
      "Global Mortality Rate",paste0(round(sum(dat$Total_Deaths)/sum(dat$Total_Cases)*100,2),"%"),
      icon = icon("list")
    )
  })
  
  
  output$plot1 <- renderPlotly({
    fig1 <- data %>% ggplot(aes(x=Date,y=cases)) + geom_col(fill="blue") +theme_light()
    
    ggplotly(fig1)
    
  })
  
  output$plot2 <- renderPlotly({
    fig2 <- data %>% ggplot(aes(x=Date,y=deaths)) + geom_col(fill="blue") + theme_light()
    
    ggplotly(fig2)
  })
  
  output$plot3 <- renderPlotly({
    fig3 <- data %>% filter(countriesAndTerritories == input$country) %>%
      ggplot(aes(x=Date,y=cases)) + geom_col(fill="blue") + theme_light()
    
    ggplotly(fig3)
  })
  
  output$plot4 <- renderPlotly({
    fig4 <- data %>% filter(countriesAndTerritories == input$country) %>%
      ggplot(aes(x=Date,y=deaths)) + geom_col(fill="blue") + theme_light()
    
    ggplotly(fig4)
  })
  
  output$mymap <- renderLeaflet({
    leaflet(dat_maps) %>%
      addTiles() %>%
      addCircleMarkers(
        radius = ~dat_maps$Deaths_per_100k/2,
        stroke = TRUE,
        #color = ~pal(Mortality),
        fillOpacity = 0.5,
        popup = paste("<br>Country</b>:",dat_maps$country,
                      "<br>Total Cases</b>:",dat_maps$Total_Cases,
                      "<br>Total Deaths</b>:",dat_maps$Total_Deaths,
                      "<br>Mortality Rate</b>:",dat_maps$Mortality,
                      "<br>Deaths per 100k</b>:",dat_maps$Deaths_per_100k,
                      "<br>Cases per 100k</b>:",dat_maps$Cases_per_100k)
      )
  })
  
  output$datatable <- DT::renderDataTable({
    dat
  })
  
}



shinyApp(ui,server)
