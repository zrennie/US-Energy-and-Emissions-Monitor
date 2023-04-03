library(shiny)
library(shinydashboard)
library(sf)
library(dplyr)
library(janitor)
library(here)
library(leaflet)
library(ggplot2)
library(plotly)
library(htmltools)
library(tictoc)
library(magrittr)
library(tidyverse)
library(RColorBrewer)
library(shinycssloaders)
library(tmaptools)
library(scales)
library(gridExtra)

source('data_cleaning.R')

########## UI ################
ui <- dashboardPage(
  
  dashboardHeader(title  = "U.S. Energy and Emissions Monitor", titleWidth = 400),
  dashboardSidebar(width = 275,
                   sidebarMenu(id = "sidebarid", 
                               menuItem("About This Shiny App", tabName = "about", icon=icon("question")),
                               menuItem("Emissions Maps", icon=icon("map"),
                                        menuSubItem("Total Emissions", tabName="totalemissions_map_plot", icon = icon("flag-usa")),
                                        menuSubItem("Emission per Capita", tabName="percapemissions_map_plot", icon = icon("person"))),
                               menuItem("Sector and Fuel Emissions Plots",
                                        menuSubItem("State Scale", tabName="emissions_by_fuel", icon = icon("line-chart")),
                                        menuSubItem("National Scale", tabName= "emissions_persector_fuel", icon = icon("chart-column"))),
                               menuItem("Energy Use by Sector", icon = icon("line-chart"), tabName="energy_use_by_sector"),
                               menuItem("Energy Generation by State", icon=icon("chart-column"), tabName="elec_generation"),
                               checkboxInput("colorblind", label="Enable colorblind assist"),
                               ####data download
                               selectInput("dataset", "Choose a dataset:",
                                           choices = c("emissions data", "energy use data", "energy generation data")),
                               downloadButton("downloadData", "Download", style = "color: #fff; background-color: #3792cb; border-color: #fff;border-color: #fff;width:130;padding: 10px 10px 10px 10px;margin: 10px 10px 10px 10px;")),
  
  hr(),
  conditionalPanel("input.sidebarid == 'totalemissions_map_plot'",
                   fluidRow(
                     column(1),
                     column(10,
                            sliderInput("rangeyears", label = "Select range of years", min = 1970, max = 2020, value = c(2000, 2020), sep="")))),
  conditionalPanel("input.sidebarid == 'percapemissions_map_plot'",
                   fluidRow(
                     column(1),
                     column(10,
                            sliderInput("rangeyears2", label = "Select range of years", min = 1970, max = 2020, value = c(2000, 2020), sep="")))),
  conditionalPanel("input.sidebarid == 'emissions_by_fuel'",
                   fluidRow(
                     column(1),
                     column(10,
                            selectInput("pick_state", label="Select state", selected = "California", choices =  unique(emissions_total_allsectors$state_name))))),
  
  conditionalPanel("input.sidebarid == 'emissions_persector_fuel'",
                   fluidRow(
                     column(1),
                     column(10,
                            checkboxGroupInput(inputId="whichPlot",
                                               label = "Select sector",
                                               choices = c("Industrial"= "industrial",
                                                           "Electric power" = "electric_power",
                                                           "Commercial" = "commercial",
                                                           "Transportation" = "transportation"),
                                               selected="industrial"
                            )))),
  
  conditionalPanel("input.sidebarid == 'energy_use_by_sector'",
                   fluidRow(
                     column(1),
                     column(10,
                            checkboxGroupInput(
                              inputId = "pick_sector3",
                              label = "Pick sector:",
                              selected = "Commercial",
                              choices = c("Commercial", "Industrial", "Transportation", "All sectors"))))),
  
  conditionalPanel("input.sidebarid == 'elec_generation'",
                   fluidRow(
                     column(1),
                     column(10,
                            selectInput("pick_state2", label="Pick state", selected = "CA", choices =  unique(clean_power_generation_states$state_name)))))),

dashboardBody(
  tags$head(
    tags$style(HTML(".shiny-output-error-validation {color: black; font-size:200%;}"))),
  tabItems(
    tabItem(tabName = "about", 
            box(width=NULL,
                h2(strong("About This Shiny App")),
                p(h4("This Shiny app explores energy usage across time by both sector and state throughout the United States. 
                All energy usage data was recorded from 1970 to 2020 across a variety of different sectors, including the industrial, 
                commerical, electric power, and transportation sectors.
                Emissions are presented in million metric tons of CO2 (MMT), and metric tons (MT). Specific fuels explored here are petroleum, 
                natural gas, coal, wind, wood, nuclear, solar, and hydroelectric. Furthermore, here we explore how much 
                electricity was generated each year by these types of fuels across each state. All data was collected from the U.S.
                Energy Information Administration (EIA).")),
                br(),
                h2(strong("User Information")),
                p(h4("The features of this application are interactive, meaning the user can select options, click on, and scroll over elements to view information. 
                     The maps section includes interactive choropleth maps that display percent change in emissions in total and per capita measurments through 1970-2020 for 
                     every state in the U.S. The plots show different comparisons for states and sectors through time regarding carbon emissions and energy usage. Insights
                     from the visualization are written below the generated plots, and serve as a justification for the relavence of the application.")),
                br(),
                p(h4("Author: Zoe Rennie, Master's student at Bren School of Environmental 
                   Science & Management at the University of California, Santa Brarbara. This Shiny App 
                   was created out of her interest in energy for ESM 244 - Advanced Data Science. ")),
                br(),
                p(h4("Citations: Total energy annual data - U.S. energy information administration (EIA). 
                Total Energy Annual Data - U.S. Energy Information Administration (EIA). 
                Retrieved March 3, 2023, from https://www.eia.gov/totalenergy/data/annual/ ")),
                br(),
                p(h4("This application was built in R version 4.2.1")))),
    
    ### MAP: Total Emissions by State 
    tabItem(tabName = "totalemissions_map_plot",
            box(width=NULL, status="primary", solidHeader=T, title = "Percent Change in Total Emissions", 
                withSpinner(leafletOutput("totalemissions")),
                br(),
                withSpinner(plotlyOutput("plot_totalemissions_state"))),
            br(),
            box(width=NULL, status="primary", solidHeader=T, title = "Quick Facts",
                p(h4("State emissions can be largely dependent on a number of factors: firstly, some states rely heavily on fossil 
                       fuel industries, such as coal, for income generation. A prime example of this is Wyoming, which produces more coal than any other state. 
                       Emissions also can be dependent on policy, with more stringent air quality and emissions policies leading to lower greenhouse gas emissions, which can 
                       be observed in the percent change in emissions.")))),
    
    ### MAP: Per Capita Emissions 
    tabItem(tabName = "percapemissions_map_plot",
            box(width=NULL, status="primary", solidHeader=T, title = "Percent Change in Per Capita Emissions",
                withSpinner(leafletOutput("percapemissions")),
                br(),
                withSpinner(plotlyOutput("plot_percapemissions_state"))),
            br(),
            box(width=NULL, status="primary", solidHeader=T, title = "Quick Facts",
                p(h4("State emissions can be largely dependent on a number of factors: firstly, some states rely heavily on fossil 
                       fuel industries, such as coal, for income generation. A prime example of this is Wyoming, which produces more coal than any other state. 
                       Emissions also can be dependent on policy, with more stringent air quality and emissions policies leading to lower greenhouse gas emissions, which can 
                       be observed in the percent change in emissions.")))),
    
    tabItem(tabName = "emissions_by_fuel",
            box(width=NULL, status="primary", solidHeader=T, title="Emissions by Fuel",  withSpinner(plotlyOutput("plot_fuel_emissions"))),
            br(),
            box(width=NULL, status="primary", solidHeader = T, title="Emissions by Sector", withSpinner(plotlyOutput("plot_emissions_sector"))),
            br(),
            box(width=NULL, status="primary", solidHeader = T, title="Quick Facts", 
                p(h4("Particular aspects of the transportation sector are notoriously difficult to decarbonize, as more than a third of emissions comes from heavy transport 
                  from trucks and freight aircraft, as well as heat-intensive material manufacturing. Electric power generation as well is a large contributor to emissions, 
                  however this is dependent on the grid mix used in the state, meaning the fuel composition used in power plants to produce electricity.")))),
    # Widget 3
    tabItem(tabName = "energy_use_by_sector",
            box(width=NULL, status="primary", solidHeader=T, title="Energy Use by Sector", withSpinner(plotlyOutput("plot_sector_energy"))),
            br(),
            box(width=NULL, status="primary", solidHeader=T, title="Proportion Electrical System Losses to Total Energy Use", withSpinner(plotlyOutput("plot_losses"))),
            br(),
            box(width=NULL, status="primary", solidHeader=T, title = "Quick Facts",
                p(h4("The upper plot shows the energy use (in trillion BTUs) of the commercial, industrial, and transportation sectors, which have been steadily 
increasing since 1970, with a tapering off since 2005. This plateau can be attributed to improvements in energy efficiency and the 
shift towards renewable energies such as solar and wind power, as well as policies that reduce energy use, such as building codes and
energy efficiency standards for appliances and equipment.")),
                
                p(h4("The impact of COVID-19 on energy consumption is evident in all sectors, with transportation suffering the most. A drop in energy 
consumption is also visible during the 2008 economic recession.")),
                
                p(h4("The bottom graph illustrates the proportion of energy lost in comparison to how much energy was used, with a decreasing trend in 
both commercial and industrial sectors, which can be attributed to energy efficiency standards from public policies. The transportation
sector is relatively insignificant when compared to other sectors, but its trend can be isolated.")),
                
                p(h4("Overall, public policies such as the American Recovery and Reinvestment Act of 2009 have invested in renewable energy and energy 
efficiency, as well as the modernization of the electric grid and other energy infrastructure, contributing to the decrease in 
energy loss and the shift towards renewable energies.")))),    
    
    #widget 4
    tabItem(tabName = "emissions_persector_fuel",
            box(width=NULL, status="primary", solidHeader=T, title="Emissions by Sector and Fuel Type", withSpinner(uiOutput("plot_emissions_persector_fuel"))),
            box(width=NULL, status="primary", solidHeader = T, title="Quick Facts", 
                p(h4("This tab explores the emissions of different fuel types by each sector from 1970-2020. Some of the notable trends are that coal emissions or electrical power 
                 peaked at about 2008 and have slowly decreased as natural gas emissions increased. Natural gas emissions are the most abundant for the commercial sector, and 
                 petroleum emissions dominate the transportation sector. ")))),
    tabItem(tabName="elec_generation",
            box(width=NULL, status="primary", solidHeader=T, title="Electricity Generation by State", withSpinner(plotlyOutput("plot_generation"))),
            box(width=NULL, status="primary", solidHeader = T, title="Quick Facts", 
                p(h4("Grid mix in different states largely varies based on 1) economic viability of the fuel source and 2) accessibility. For example, Colorado has a larger portion 
                       of hydroelectric and wind power generation because it is geographically well-positioned to take advantage of these renewable resources. On the other hand, states 
                       like Wyoming and Alaska which have strong economic and social ties to their energy sources, particulary coal for Wyoming, have grown a dependency on these fuels.
                       Policy also plays a large roll in grid mix - climate-progressive states like California show a more mixed grid with a fair amount of renewable and gray power sources.")))
    )
  ))
)


########## SERVER ################
server <- function(input, output) {
  st <- read_sf(here( "cb_2021_us_state_500k", "cb_2021_us_state_500k.shp")) %>% 
    st_transform('+proj=longlat +datum=WGS84')
  US <-  st %>%
    clean_names() %>%
    mutate(state_name=name)
  states_emissions <- inner_join(US, emissions_all_fuels, by="state_name") %>%
    select(state_name, value, geometry, period, value_units, emissions_per_capita_units, emissions_per_capita_value)
  #########REACTIVE OBJECTS############
  ggplot_totalstate_data <- reactive({
    states_emissions %>%
      filter(state_name %in% input$totalemissions_shape_click$id & period %in% input$rangeyears[1]:input$rangeyears[2])
  })
  
  
  ggplot_percapstate_data <- reactive({
    states_emissions %>% 
      filter(state_name %in% input$percapemissions_shape_click$id & period %in% input$rangeyears2[1]:input$rangeyears2[2])
  })
  
  ggplot_emissions_sector_data <- reactive({
    emissions_persector %>% filter(state_name %in% input$pick_state)
  })
  
  
  ggplot_fuel_data <- reactive({
    emissions_total_allsectors %>%
      select(state_name, fuel_name, value, period, value_units, emissions_per_capita_units) %>%
      filter(state_name %in% input$pick_state)
  })
  
  ggplot_sector_data2 <- reactive({
    filtered_sector2 %>%
      subset(description %in% input$pick_sector3)
  })
  
  ggplot_sector_losses <- reactive({
    sector_total_losses %>%
      subset(sector%in% input$pick_sector3)
  })
  
  date_emissions_total  <- reactive({
    states_emissions %>%
      select(period, value, state_name) %>%
      filter(period %in% input$rangeyears[1]:input$rangeyears[2]) %>%
      group_by(state_name) %>%
      slice(c(1,n())) %>%
      mutate(diff = diff(c(NA,value))) %>%
      mutate(initial=value[1]) %>%
      mutate(pct_change = diff/initial*100) %>%
      drop_na()
  })
  
  date_emissions_capita  <- reactive({
    states_emissions %>%
      select(period, emissions_per_capita_value, state_name) %>%
      filter(period %in% input$rangeyears2[1]:input$rangeyears2[2]) %>%
      group_by(state_name) %>%
      slice(c(1,n())) %>%
      mutate(diff = diff(c(NA,emissions_per_capita_value))) %>%
      mutate(initial=emissions_per_capita_value[1]) %>%
      mutate(pct_change = diff/initial*100) %>%
      drop_na()
  })
  
  ggplot_emissions_persector_fuel <- reactive({
    emissions_persector_fuel %>%
      filter(sector_name %in% input$whichPlot)
  })
  
  ggplot_electricity_generation <- reactive({
    clean_power_generation_states  %>%
      clean_names() %>%
      filter(state_name %in% input$pick_state2)
  })
  
  #########OUTPUTS#############
  output$totalemissions <- renderLeaflet({
    pal <- colorNumeric(if(input$colorblind==T){"Blues"}else{"YlOrRd"},date_emissions_total()$pct_change)
    leaflet(date_emissions_total(), options=leafletOptions(doubleClickZoom=F, minZoom = 2)) %>%
      addTiles() %>%
      addPolygons(layerId = ~unique(state_name),
                  fillColor = ~pal(pct_change),
                  weight = 0.5,
                  fillOpacity = 0.5,
                  smoothFactor = 0.2,
                  label = ~paste0(round(pct_change), "%"),
                  stroke = T, color = "black") %>%
      addLegend("bottomright", pal = pal, values = date_emissions_total()$pct_change,
                title = "% change in carbon <br>emissions</br>", labFormat = labelFormat(suffix = "%")) %>%
      setView(lng = -96.25, lat = 39.50, zoom = 4)
  })
  
  output$percapemissions <- renderLeaflet({
    pal <- colorNumeric(if(input$colorblind==T){"Blues"}else{"YlOrRd"},date_emissions_capita()$pct_change)
    leaflet(date_emissions_capita(), options=leafletOptions(doubleClickZoom=F, minZoom = 2)) %>%
      addTiles() %>%
      addPolygons(layerId = ~unique(state_name),
                  fillColor = ~pal(pct_change),
                  weight = 0.5,
                  fillOpacity = 0.5,
                  smoothFactor = 0.2,
                  label = ~paste0(round(pct_change), "%"),
                  stroke = T, color = "black"
      ) %>%
      addLegend("bottomright", pal = pal, values = date_emissions_capita()$pct_change,
                title = "% change in per capita <br>carbon emissions</br>", labFormat = labelFormat(suffix = "%")) %>%
      setView(lng = -96.25, lat = 39.50, zoom = 4)
  })
  
  observe({
    if(length(input$totalemissions_shape_click$id)!=0){
      leafletProxy("totalemissions") %>%
        removeShape("highlighted_polygon") %>%
        addPolylines(stroke=TRUE, weight = 5,color="yellow",data=subset(date_emissions_total(),date_emissions_total()$state_name==input$totalemissions_shape_click$id),layerId="highlighted_polygon")
    }
  })
  
  observe({
    if(length(input$percapemissions_shape_click$id)!=0){
      leafletProxy("percapemissions") %>%
        removeShape("highlighted_polygon") %>%
        addPolylines(stroke=TRUE, weight = 5,color="yellow",data=subset(date_emissions_capita(),date_emissions_capita()$state_name==input$percapemissions_shape_click$id),layerId="highlighted_polygon")
    }
  })
  
  output$plot_totalemissions_state <- renderPlotly({
    shiny::validate(need(input$totalemissions_shape_click$id, "Click on state to generate plot"))
    ggplot(data=ggplot_totalstate_data(), 
           aes(period, value)) +
      geom_line(size=1) +
      geom_point(size=2)+
      theme_minimal()+
      theme(legend.key.size = unit(2, 'cm'), 
            legend.title = element_text(size=15, face="bold"), 
            legend.text = element_text(size=12),
            axis.text=element_text(size=12),
            axis.title=element_text(size=12,face="bold"), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
      ylab("Change in CO<sub>2</sub> Emissions (MMT)")+
      xlab("Year") +
      ggtitle(paste("Gross Change in CO<sub>2</sub> Emissions Over Time For", input$totalemissions_shape_click$id)) + 
      scale_x_continuous(breaks=seq(input$rangeyears[1], input$rangeyears[2], 5)) + 
      scale_y_continuous(limits = c(0, NA)) +
      scale_color_manual(values=if(input$colorblind==T){safe_pal}else{c("#1f77b4","black",  "#d62728", "#2ca02c")})+
      theme(plot.title = element_text(size=22))
    ggplotly() %>% layout(hoverlabel=list(bgcolor="white"))
    
  })
  
  output$plot_percapemissions_state <- renderPlotly({
    shiny::validate(need(input$percapemissions_shape_click$id, "Click on state to generate plot"))
    ggplot(data=ggplot_percapstate_data(), 
           aes(period, emissions_per_capita_value)) + 
      geom_line(size=1) +
      geom_point(size=2)+
      theme_minimal()+
      theme(legend.key.size = unit(2, 'cm'), 
            legend.title = element_text(size=15, face="bold"), 
            legend.text = element_text(size=12),
            axis.text=element_text(size=12),
            axis.title=element_text(size=12,face="bold"), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
      ylab("Change in CO<sub>2</sub> Emissions Per Capita (MT)")+
      xlab("Year") + 
      ggtitle(paste("Per Capita Change in CO<sub>2</sub> Emissions Over Time For", input$percapemissions_shape_click$id)) + 
      scale_x_continuous(breaks=seq(input$rangeyears2[1], input$rangeyears2[2], 5)) +
      scale_y_continuous(limits = c(0, NA)) +
      theme(plot.title = element_text(size=22))
    ggplotly() %>% layout(hoverlabel=list(bgcolor="white"))
    
  })
  pal <- c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2")
  safe_pal <- c("#9ECAE1", "#6BAED6","#4292C6", "#2171B5")
  
  
  output$plot_emissions_sector <- renderPlotly({
    names(safe_pal) <- unique(ggplot_emissions_sector_data()$sector_name)
    ggplot(data=ggplot_emissions_sector_data(),
           aes(period, value, color = sector_name)) +
      geom_line(size=1) +
      geom_point(size=2)+
      theme_minimal()+
      theme(legend.key.size = unit(2, 'cm'), 
            legend.title = element_text(size=14, face="bold"), 
            legend.text = element_text(size=12),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
      ggtitle(paste("Emissions By Sector for", input$pick_state)) + 
      labs(color = "Sector")+
      ylab("CO<sub>2</sub> emissions (MMT)") +
      xlab("Year") + 
      scale_color_manual(values=if(input$colorblind==T){safe_pal}else{c("#1f77b4","black",  "#d62728", "#2ca02c")})
    ggplotly() %>% layout(hoverlabel=list(bgcolor="white"))
  })
  
  
  output$plot_fuel_emissions <- renderPlotly({
    names(safe_pal) <- unique(ggplot_fuel_data()$fuel_name)
    ggplot(data=ggplot_fuel_data(), 
           aes(period, value, color = fuel_name)) +
      geom_line(size=1) +
      geom_point(size=2)+
      theme_minimal()+
      ggtitle(paste("Emissions By Fuel Type for", input$pick_state)) +
      theme(legend.key.size = unit(2, 'cm'), 
            legend.title = element_text(size=14, face="bold"), 
            legend.text = element_text(size=12),
            legend.box.background = element_rect(colour = "black"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
      labs(color = "Fuel Type") +
      ylab("CO<sub>2</sub> emissions (MMT)") +
      xlab("Year") + 
      scale_color_manual(values=if(input$colorblind==T){safe_pal}else{c("#1f77b4","black",  "#d62728", "#2ca02c")})
    ggplotly() %>% layout(hoverlabel=list(bgcolor="white"))
  })
  
  output$plot_sector_energy <- renderPlotly({
    shiny::validate(need(input$pick_sector3, "Please choose a sector"))
    names(safe_pal) <- unique(ggplot_sector_data2()$description)
    ggplot(data=ggplot_sector_data2(),
           aes(year, value, color = description))+
      theme_minimal()+
      scale_y_continuous(labels = function(x) paste0(x/1000, " k"))+
      geom_point(size=2) + 
      geom_line(size=1)+
      theme(legend.key.size = unit(2, 'cm'), 
            legend.title = element_text(size=14, face="bold"), 
            legend.text = element_text(size=12),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"))+
      labs(color = "Sector")+
      ylab("Energy Use (trillion BTU)") +
      xlab("Year")+
      expand_limits(y = 0) +
      scale_color_manual(values=if(input$colorblind==T){safe_pal}else{c("#1f77b4","black",  "#d62728", "#2ca02c")})
    ggplotly() %>% layout(hoverlabel=list(bgcolor="white"))
  })
  
  
  output$plot_losses <- renderPlotly({
    shiny::validate(need(input$pick_sector3, "Please choose a sector"))
    names(safe_pal) <- unique(ggplot_sector_losses()$sector)
    ggplot(ggplot_sector_losses(), 
           aes(year, proportion_losses, color=sector)) +
      geom_point(size=2) + 
      geom_line(size=1) +
      theme_minimal() + 
      theme(legend.key.size = unit(2, 'cm'),
            legend.title = element_text(size=14, face="bold"), 
            legend.text = element_text(size=12),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
      labs(color = "Sector")+
      ylab("Proportion Losses") +
      xlab("Year")+
      scale_color_manual(values=if(input$colorblind==T){safe_pal}else{c("#1f77b4","black",  "#d62728", "#2ca02c")})
    ggplotly() %>% layout(hoverlabel=list(bgcolor="white"))
  })

  output$electric_power <- renderPlotly({    
    names(safe_pal) <- unique(emissions_persector_fuel$fuel_name)
    if("electric_power" %in% input$whichPlot){
      emissions_persector_fuel %>%
        filter(sector_name %in% "electric_power" & !fuel_name %in% "All Fuels") %>%
        ggplot(aes(period, total, fill=fuel_name)) +
        geom_col()+
        scale_y_continuous(labels = function(x) paste0(x/1000, " k"))+
        labs(fill= "Fuel Type")+
        xlab("Year")+
        ylab("CO2 Emissions (MMT)")+
        theme_minimal()+
        theme(legend.key.size = unit(2, 'cm'),
              legend.title = element_text(size=14, face="bold"), 
              legend.text = element_text(size=12),
              axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold"), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"))+
        ggtitle("Emissions By Fuel Type for Electric Power") +
        scale_fill_manual(values=if(input$colorblind==T){safe_pal}else{pal}) 
      ggplotly() %>% layout(hoverlabel=list(bgcolor="white"))
      
    }
  })
  
  output$commercial <- renderPlotly({ 
    names(safe_pal) <- unique(emissions_persector_fuel$fuel_name)
    if("commercial" %in% input$whichPlot){
      emissions_persector_fuel %>%
        filter(sector_name %in% "commercial" & !fuel_name %in% "All Fuels") %>%
        ggplot(aes(period, total, fill=fuel_name)) +
        geom_col()+
        scale_y_continuous(labels = function(x) paste0(x/1000, " k"))+
        labs(fill= "Fuel Type")+
        xlab("Year")+
        ylab("CO2 Emissions (MMT)")+
        theme_minimal()+
        theme(legend.key.size = unit(2, 'cm'),
              legend.title = element_text(size=14, face="bold"), 
              legend.text = element_text(size=12),
              axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold"), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"))+
        ggtitle("Emissions By Fuel Type for Commercial") +
        scale_fill_manual(values=if(input$colorblind==T){safe_pal}else{pal})
      ggplotly() %>% layout(hoverlabel=list(bgcolor="white"))
      
    }
  })
  
  output$industrial <- renderPlotly({ 
    names(safe_pal) <- unique(emissions_persector_fuel$fuel_name)
    if("industrial" %in% input$whichPlot){
      emissions_persector_fuel %>%
        filter(sector_name %in% "industrial" & !fuel_name %in% "All Fuels") %>%
        ggplot(aes(period, total, fill=fuel_name)) +
        geom_col()+
        scale_y_continuous(labels = function(x) paste0(x/1000, " k"))+
        labs(fill= "Fuel Type")+
        xlab("Year")+
        ylab("CO2 Emissions (MMT)")+
        theme_minimal()+
        theme(legend.key.size = unit(2, 'cm'),
              legend.title = element_text(size=14, face="bold"), 
              legend.text = element_text(size=12),
              axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold"), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"))+
        ggtitle("Emissions By Fuel Type for Industrial") +
        scale_fill_manual(values=if(input$colorblind==T){safe_pal}else{pal}) 
      ggplotly() %>% layout(hoverlabel=list(bgcolor="white"))
      
    }
  })
  
  output$transportation <- renderPlotly({ 
    names(safe_pal) <- unique(emissions_persector_fuel$fuel_name)
    if("transportation" %in% input$whichPlot){
      emissions_persector_fuel %>%
        filter(sector_name %in% "transportation" & !fuel_name %in% "All Fuels") %>%
        ggplot(aes(period, total, fill=fuel_name)) +
        geom_col()+
        scale_y_continuous(labels = function(x) paste0(x/1000, " k"))+
        labs(fill= "Fuel Type")+
        xlab("Year")+
        ylab("CO2 Emissions (MMT)")+
        theme_minimal()+
        theme(legend.key.size = unit(2, 'cm'),
              legend.title = element_text(size=14, face="bold"), 
              legend.text = element_text(size=12),
              axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold"), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"))+
        ggtitle("Emissions By Fuel Type for Transportation") +
        scale_fill_manual(values=if(input$colorblind==T){safe_pal}else{pal}) 
      ggplotly() %>% layout(hoverlabel=list(bgcolor="white"))
      
    }
  })
  
  output$plot_emissions_persector_fuel <- renderUI({
    plot_output_list <- lapply(input$whichPlot, 
                               function(plotname) {
                                 column(width=5, plotlyOutput(plotname))
                               })
    do.call(tagList, plot_output_list)
  })
  
  output$plot_generation <- renderPlotly({
    safe_pal <- c("#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B", "#2B15FC", "#0CA0FF", "#0465A3", "#75C9FF", "#055589")
    names(safe_pal) <- unique(clean_power_generation_states$energy_source)
    ggplot(ggplot_electricity_generation(),
           aes(year, generation_megawatthours, fill=energy_source)) +
      geom_col()+
      labs(x="Year", y="Generation (mWh)", fill= "Energy Source")+
      theme_minimal()+
      theme(legend.key.size = unit(2, 'cm'),
            legend.title = element_text(size=14, face="bold"), 
            legend.text = element_text(size=12),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
      ggtitle(paste("Grid Mix for",input$pick_state2))+
      scale_fill_manual(values=if(input$colorblind==T){safe_pal}else{pal}) 
    ggplotly() %>% layout(hoverlabel=list(bgcolor="white"))
  })
  
  datasetInput <- reactive({
    switch(input$dataset,
           "emissions data" = emissions_complete_data,
           "energy use data" = filtered_sector, 
           "energy generation data" = clean_power_generation_states)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)