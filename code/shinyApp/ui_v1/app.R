#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library("shiny")
library("bslib")
library("dplyr")
library("ggplot2")
library("tidyverse")
library("hrbrthemes")
library("viridis")
library("DT")
library("knitr")
library("kableExtra")

library("googleway")
library("ggmap")
key <- "AIzaSyDQ-4w81Ze7BCZgGanWcsYTr0wq69VP1Ncet_key(key)
register_google(key)

# sequestration files
library("readxl") # to read the xlsx files

uu <- read_excel("C:/Users/Emily/OneDrive/Documents/GitHub/24-fall-EFUGH/Fugh_Capstone/data/ghgp_data_2022.xlsx", 
                 sheet = "CO2 Injection", 
                 skip=3,
                 col_names = TRUE)

rr <- read_excel("C:/Users/Emily/OneDrive/Documents/GitHub/24-fall-EFUGH/Fugh_Capstone/data/ghgp_data_2022.xlsx", 
                 sheet = "Geologic Sequestration of CO2", 
                 skip=3,
                 col_names = TRUE)

seq <- rbind(subset(uu,select=c(1,2,3,4,5,6,7,8,9,10,11,12)),
             subset(rr,select=c(1,2,3,4,5,6,7,8,9,10,11,12)))

# census stuff
zip <- readr::read_csv("C:/Users/Emily/OneDrive/Documents/GitHub/24-fall-EFUGH/Fugh_Capstone/data/zip.csv")
Age <- readr::read_csv("C:/Users/Emily/OneDrive/Documents/GitHub/24-fall-EFUGH/Fugh_Capstone/data/age.csv")
Income <- readr::read_csv("C:/Users/Emily/OneDrive/Documents/GitHub/24-fall-EFUGH/Fugh_Capstone/data/income.csv")
Ethnicity <- readr::read_csv("C:/Users/Emily/OneDrive/Documents/GitHub/24-fall-EFUGH/Fugh_Capstone/data/race.csv")

locations <- readr::read_csv("C:/Users/Emily/OneDrive/Documents/GitHub/24-fall-EFUGH/Fugh_Capstone/data/locations.csv")
locationTable <- readr::read_csv("C:/Users/Emily/OneDrive/Documents/GitHub/24-fall-EFUGH/Fugh_Capstone/data/locationTypes.csv")
stateText <- readr::read_csv("C:/Users/Emily/OneDrive/Documents/GitHub/24-fall-EFUGH/Fugh_Capstone/data/stateText.csv")

ui <- ui <- navbarPage(
  title="Carbon Capture Dashboard",  # The title of the navbar,
  tabPanel("Overview", uiOutput("introUI")),
  tabPanel("Census Demographics", uiOutput("censusUI")),
  tabPanel("Facility Locations", uiOutput("communityUI")),
  tabPanel("Individual Sites", uiOutput("siteUI"))
)


server <- function(input, output, session) {
  
  output$introUI <- renderUI({
    
    
    
    navset_card_underline(
      # Panel with plot ----
      nav_panel("Introduction", includeHTML("introductionText.html")),
      
      # Panel with summary ----
      nav_panel("Background", includeHTML("history.html")),
      
      # Panel with table ----
      nav_panel("Relevant Events", includeHTML("events.html"))
    )
  })
  
  output$censusUI <- renderUI({
    sidebarLayout(
      # Sidebar Panel
      sidebarPanel(
        selectInput("census", "Choose a category:", choices = c("Age", "Income", "Ethnicity")),
        uiOutput("censuSlider"),
        renderText({
          "These graphs are created using data from the US Census Bureau's 2022 American Community Survey.
        The facilities’ demographics are represented by the average of the zipcodes encompassing a 20 mile radius of the facility."
        })
      ),
      
      # Main Panel
      mainPanel(
        fluidRow(
          column(6, plotOutput("censusPlot")),  # Plot for census data
          column(6, plotOutput("censusPie")),   # Pie chart for census data
        )
      )
    )
    
  })  
  
  census <- reactive({
    req(input$census)
    data <- get(input$census)
    return(data)
  })
  
  output$censuSlider <- renderUI({
    req(census()) 
    sliderInput(
      "popRange", 
      "Select Population Range:",
      min = round(min(census()$totalpop, na.rm = TRUE), 2),
      max = round(max(census()$totalpop, na.rm = TRUE), 2),
      value = c(round(min(census()$totalpop, na.rm = TRUE), 2), 
                round(max(census()$totalpop, na.rm = TRUE), 2)),  # initial range values
      step = 1000
    )
  })
  
  library("tidyr")
  filteredCensus <- reactive({
    req(census(), input$popRange)  
    
    census() %>%
      filter(totalpop >= input$popRange[1] & totalpop <= input$popRange[2]) %>% 
      pivot_longer(
        cols = -c(1,2,3),  
        names_to = "variable",  
        values_to = "value"
      ) %>%
      mutate(variable = factor(variable)) 
  })
  
  
  output$censusPlot <- renderPlot({
    req(filteredCensus()) 
    
    filteredCensus() %>%
      ggplot(aes(x = variable, y = value, fill = variable)) +
      geom_boxplot() +
      scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
      geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 0.8),
        plot.title = element_text(size = 11)
      ) +
      xlab("")
  })
  
  output$censusPie <- renderPlot({
    req(filteredCensus())
    
    # Summarizing data by variable
    pie_data <- filteredCensus() %>%
      group_by(variable) %>%
      summarise(total = sum(value, na.rm = TRUE))
    
    # Now use 'total' for the y aesthetic in the pie chart
    ggplot(pie_data, aes(x = "", y = total, fill = variable)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      scale_fill_viridis(discrete = TRUE, alpha=0.6)
  })
  
  output$communityUI <- renderUI({
    sidebarLayout(
      sidebarPanel (
        selectInput("state","Choose a State:",choices=unique(locations$state)),
        textOutput("stateCommunity")
      )
      ,
      mainPanel(
        column(12, plotOutput("stateMap")),
        column(12, plotOutput("locBoxplot")),
        column(12, tableOutput("loctype"))
      )
    )
  })
  
  stateIntro <- reactive({
    req(stateText)
    req(input$state)
    stateText %>% 
      filter(State==input$state)
  })
  
  output$stateCommunity <- renderText(stateIntro()$Text)
  
  locCounts <- reactive({
    req(locations)  
    locations %>%
      count(site, type, state) %>%
      filter(state == input$state) 
  })
  
  output$locBoxplot <- renderPlot(
    ggplot(data=locCounts(),aes(x=type, y=n, fill=type)) +
      geom_boxplot() +
      scale_fill_viridis(discrete = TRUE, alpha=0.6) +
      geom_jitter(color="black", size=0.4, alpha=0.9) +
      theme(
        legend.position="none",
        axis.text.x = element_text(angle=45,hjust=.8),
        plot.title = element_text(size=11)
      ) +
      ylim(0,200) +
      xlab("") 
  )
  
  output$loctype <- renderTable({locationTable})
  
  
  output$siteUI <- renderUI({
    sidebarLayout(
      sidebarPanel(
        selectInput("site", "Choose a Site:",choices=(unique(locations$site))),
        uiOutput("facilityInfo")
      ),
      mainPanel(
        column(8, plotOutput("siteMap")),
        column(4, tableOutput("locationCount")),
        column(12, tableOutput("loctype"))
      )
    )
  })
  
  output$stateMap <- renderPlot({
    ggmap(get_map(location = input$state, 
                  zoom = 6, 
                  maptype = 'terrain',
                  color = 'bw')) +
      geom_point(data = subset(locations,locations$state==input$state), 
                 aes(x = longitude, y = latitude),
                 size=2, color = 'darkblue') + 
      ggtitle(paste("Sites Located in", input$state)) +
      theme_minimal()
  })
  
  facility <- reactive({
    req(seq)
    subset(seq,seq$`Facility Id`==input$site)
  })
  
  output$facilityInfo <- renderUI({
    df <- subset(seq,seq$`Facility Id`==input$site,select=c(1,3,4,5,12))
    df <- as.data.frame(t(df)) %>% kable(col.names = NULL) %>% kable_styling()
    HTML(df)
  })
  
  
  output$siteMap <- renderPlot({
    ggmap(get_map(location=c(lon=facility()$Longitude,lat=facility()$Latitude),
                  zoom="auto",
                  scale="auto",
                  maptype='terrain',
                  color='bw')
    ) + geom_point(data=subset(locations,locations$site==input$site),
                   aes(x=lng,y=lat,color=type),
                   size=2) +
      scale_color_manual(values=c("Medical"='#0074D9',
                                  "Schools"="#FF4136",
                                  "Social Locations"='#2ECC40',
                                  "Stores"='#FFDC00',
                                  "Essential Services"='#F012BE', 
                                  "Places of Worship"='#39CCCC',
                                  "Transportation"="#B10DC9")) +
      geom_point(data=facility(),
                 aes(x=Longitude,y=Latitude),
                 size=5,
                 shape=18,
                 color='black') +
      labs(color="Category") +
      ggtitle(facility()$`Facility Name`)
  })
  
  siteCounts <- reactive({
    req(locations)  
    locations %>%
      count(site, type) %>%
      filter(site == input$site) 
  })
  
  output$locationCount <- renderTable({subset(siteCounts(),select=c(2,3))})
  
}

shinyApp(ui, server)