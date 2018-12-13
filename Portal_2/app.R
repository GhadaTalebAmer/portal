library(shiny)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(shinythemes)
library(leaflet)
library(RColorBrewer)
library(leaflet.extras)
library(DT)
library(tidyverse)
library(ggplot2)
library(knitr)
library(formattable)
library(janitor)
library(readr)
library(dplyr)
library(shiny)
library(devtools)
library(leaflet)
library(RColorBrewer)
library(leaflet.extras)
library(kableExtra)


justification_2012 <- read_rds("justification_12.rds")
justification_2007 <- read_rds("justification_07.rds")
justification_2002 <- read_rds("justification_02.rds")
violence <- read_rds("violence.rds")
violence_2007 <- read_rds("violence_2007.rds")
new <- read_rds("new.rds")
bind <- read_rds("bind.rds")
bind_2007 <- read_rds("bind_2007.rds")
data_2012 <- read_rds("data_2012.rds")
data_2007 <- read_rds("data_2007.rds")
data_2002 <- read_rds("data_2002.rds")

sgbv <- read_csv("sgbv_services_institutions.csv")

sgbv_2 <- 
  sgbv %>% 
  select(-Longitude, -Latitude)



ui <- fluidPage(theme = shinytheme("cerulean"),
                
                
                titlePanel(h2("Sexual and Gender-Based Violence Portal"),
                           windowTitle = "SGBV Portal"),
                h4("Country Profile: Jordan"),
                
                ## Created a tabset panel to create a dashboard-like 
                ## appearance for the app. It consists of four 
                ## tabs. Each tab contains a side bar
                ## with descriptions/instructions and a main panel
                ## of relevant outputs. For text outputs, I chose to
                ## include text directly in main panel outputs to 
                ## make it easier to manipulate the appearance.
                
                tabsetPanel(
                  
                  tabPanel(
                    title = "Population and Family Health Survey (2012)",
                    sidebarPanel(width = 3,
                                 h5("Overview of Data"),
                                 p("")),
                    mainPanel(
                      tabsetPanel(
                        tabPanel(title = "Data Visualizations",
                                 tabsetPanel(
                                   tabPanel(title = "Wife Beating",
                                            h4("Justifications of Wife-Beating by Female-Identified Respondents in 2012"),
                                            p(""),
                                            plotOutput(outputId = "A_2012", width = 800, height = 400),
                                            br(),
                                            plotOutput(outputId = "B_2012", width = 800, height = 400),
                                            br(),
                                            plotOutput(outputId = "C_2012", width = 800, height = 400),
                                            br(),
                                            plotOutput(outputId = "E_2012", width = 800, height = 400)
                                   ),
                                   tabPanel(title = "Violence",
                                            h4(""),
                                            p(""),
                                            plotOutput(outputId = "violence_plot", width = 600, height = 400),
                                            hr(),
                                            plotOutput(outputId = "physical_violence_plot", width = 1000, height = 800),
                                            hr(),
                                            h5(""),
                                            tableOutput(outputId = "kable")))),
                        
                        tabPanel(title = "Dataset",
                                 dataTableOutput(outputId = "data_2012")),
                        tabPanel(title = "Publications")))),
                  
                  tabPanel(
                    title = "Population and Family Health Survey (2007)",
                    sidebarPanel(width = 3,
                                 h5("Overview of Data"),
                                 p("")),
                    mainPanel(
                      tabsetPanel(
                        tabPanel(title = "Data Visualizations",
                                 tabsetPanel(
                                   tabPanel(title = "Wife Beating",
                                            h4("Justifications of Wife-Beating by Female-Identified Respondents in 2007"),
                                            h6(""),
                                            plotOutput(outputId = "A_2007", width = 800, height = 400),
                                            br(),
                                            plotOutput(outputId = "B_2007", width = 800, height = 400),
                                            br(),
                                            plotOutput(outputId = "C_2007", width = 800, height = 400),
                                            br(),
                                            plotOutput(outputId = "E_2007", width = 800, height = 400)),
                                   tabPanel(title = "Violence",
                                            h4(""),
                                            p(""),
                                            plotOutput(outputId = "violence_plot_2007", width = 600, height = 400),
                                            hr(),
                                            tableOutput(outputId = "kable_2007")))),
                        tabPanel(title = "Dataset",
                                 dataTableOutput(outputId = "data_2007")),
                        tabPanel(title = "Publications")))),
                  
                  tabPanel(
                    title = "Population and Family Health Survey (2002)",
                    sidebarPanel(width = 3,
                                 h5("Overview of Data"),
                                 p("")),
                    mainPanel(
                      tabsetPanel(
                        tabPanel(title = "Data Visualizations",
                                 h4("Justifications of Wife-Beating by Female-Identified Respondents in 2002"),
                                 h6(""),
                                 plotOutput(outputId = "A_2002", width = 800, height = 400),
                                 br(),
                                 plotOutput(outputId = "B_2002", width = 800, height = 400),
                                 br(),
                                 plotOutput(outputId = "C_2002", width = 800, height = 400),
                                 br(),
                                 plotOutput(outputId = "E_2002", width = 800, height = 400)),
                        tabPanel(title = "Dataset",
                                 dataTableOutput(outputId = "data_2002")),
                        tabPanel(title = "Publications")))),
                  
                  tabPanel(
                    title = "UNICEF",
                    sidebarPanel(width = 3,
                                 h5("Overview of Data"),
                                 p("")),
                    mainPanel(
                      tabsetPanel(
                        tabPanel(title = "Data Visualizations",
                                 h4("Sexual Violence Children"),
                                 h6(""),
                                 leafletOutput(outputId = "unicef_map", width = 1000),
                                 hr(),
                                 tableOutput(outputId = "unicef_table")),
                        tabPanel(title = "Dataset"),
                        tabPanel(title = "Publications")))),
                  
                  
                  tabPanel(
                    title = "Resource Network",
                    sidebarPanel(width = 3,
                                 h5("Overview"),
                                 p("")),
                    mainPanel(
                      h5("Mapping of Sexual and Gender-Based Violence Institutions and Services in Jordan"),
                      br(),
                      leafletOutput(outputId = "leaflet_map_2", width = 1000),
                      hr(),
                      dataTableOutput(outputId = "institutions")))))






server <- function(input, output) {
  
  output$sample_data <- renderDataTable({
    
    datatable(sample_data, 
              caption = "Search for Individual Country Indicators", selection = "none")  %>%  
      formatStyle(c('Country','Age', 'Sex', 'X', 'Y', 'Percentage'),
                  backgroundColor = 'white')
    
    
  }) 
  
  output$sample_map <- renderLeaflet({
    
    
    
    leaflet(options = leafletOptions(minZoom = 2)) %>% 
      addProviderTiles("Wikimedia", 
                       group = "Coloured Political Boundaries Map") %>% 
      setMaxBounds(lng1 = 180 ,
                   lat1 = 90, 
                   lng2 = -180, 
                   lat2 = -90) %>% 
      addCircleMarkers(lng = sample_data$X, 
                       lat = sample_data$Y,
                       popup = paste0("As of 2016, the proportion of the population in ", sample_data$Country,
                                      " aged 18-29 years who experienced sexual violence by age 18 was ",
                                      sample_data$Percentage, "."),
                       label = sample_data$Country,
                       clusterOptions = markerClusterOptions(),
                       labelOptions = labelOptions(noHide = F, 
                                                   textsize = "10px", 
                                                   direction = "bottom")) %>% 
      addSearchOSM() %>%  
      addResetMapButton()
    
  })
  
  output$leaflet_map_2 <- renderLeaflet({
    
    research <- 
      sgbv %>% 
      filter(Services == "Research and Data Collection")
    
    support <- 
      sgbv %>% 
      filter(Services == "Support Services and Programs")
    
    legal <- 
      sgbv %>% 
      filter(Services == "Legal Services")
    
    services_palette <- 
      colorFactor(palette = "Set1", 
                  levels = c("Research and Data Collection", 
                             "Support Services and Programs",
                             "Legal Services"))
    
    
    
    map <-
      leaflet(options = leafletOptions(minZoom = 2)) %>% 
      addProviderTiles("Wikimedia", 
                       group = "Coloured Political Boundaries Map") %>% 
      addCircleMarkers(lng = research$Longitude, 
                       lat = research$Latitude,
                       group = "Research and Data Collection",
                       color = services_palette(research$Services),
                       radius = 6, 
                       popup = paste0("Location: ", research$City, ".",
                                      br(),
                                      "The institution is a: ", research$Type, ". ",
                                      br(),
                                      "Phone Number: ", research$Phone, " ",
                                      br(),
                                      "Email: ", research$Email, " ",
                                      br(),
                                      "Website: ", research$Website),
                       label = paste0(research$Name),
                       
                       clusterOptions = markerClusterOptions(),
                       labelOptions = labelOptions(noHide = F, 
                                                   textsize = "12px", 
                                                   direction = "bottom")) %>% 
      addCircleMarkers(lng = support$Longitude, 
                       lat = support$Latitude,
                       group = "Support Services and Programs",
                       color = services_palette(support$Services),
                       radius = 6, 
                       popup = paste0("Location: ", support$City, ".",
                                      br(),
                                      "The institution is a: ", support$Type, ". ",
                                      br(),
                                      "Phone Number: ", support$Phone, " ",
                                      br(),
                                      "Email: ", support$Email, " ",
                                      br(),
                                      "Website: ", support$Website),
                       label = paste0(support$Name),
                       
                       clusterOptions = markerClusterOptions(),
                       labelOptions = labelOptions(noHide = F, 
                                                   textsize = "12px", 
                                                   direction = "bottom")) %>% 
      addCircleMarkers(lng = legal$Longitude, 
                       lat = legal$Latitude,
                       group = "Legal Services",
                       color = services_palette(legal$Services),
                       radius = 6, 
                       popup = paste0("Location: ", legal$City, ".",
                                      br(),
                                      "The institution is a: ", legal$Type, ". ",
                                      br(),
                                      "Phone Number: ", legal$Phone, " ",
                                      br(),
                                      "Email: ", legal$Email, " ",
                                      br(),
                                      "Website: ", legal$Website),
                       label = paste0(legal$Name),
                       
                       clusterOptions = markerClusterOptions(),
                       labelOptions = labelOptions(noHide = F, 
                                                   textsize = "12px", 
                                                   direction = "bottom")) %>% 
      addLegend(pal = services_palette,
                values = c("Research and Data Collection", 
                           "Support Services and Programs",
                           "Legal Services"),
                opacity = 0.7,
                title = "Type of Services Provided",
                position = "bottomright") %>% 
      addSearchOSM() %>%  
      addResetMapButton()
    
    
    map %>% 
      addProviderTiles("Wikimedia", 
                       group = "Coloured Political Boundaries Map") %>% 
      addProviderTiles("Esri.WorldImagery",
                       group = "Coloured Physical Map")  %>% 
      addLayersControl(baseGroups = c( 
        "Coloured Political Boundaries Map",
        "Coloured Physical Map"),
        overlayGroups = 
          c("Research and Data Collection", 
            "Support Services and Programs",
            "Legal Services"))
    
  })
  output$institutions <- renderDataTable({
    
    datatable(sgbv_2, 
              caption = "List of Governmental and Non-Governmental Organizations involved in Sexual and Gender-Based Violence Work in Jordan", 
              selection = "none", 
              width = "1000px",
              style = "jqueryui")  
  })
  
  output$A_2012 <- renderPlot({
    
    
    justification_2012 %>% 
      filter(!A == "Don't Know") %>% 
      group_by(A, `Age Group`, `Education Level`) %>% 
      count() %>% 
      ggplot(aes(x = `Age Group`, y = n, fill = A)) + 
      geom_col() + 
      facet_wrap(~`Education Level`, scales = "fixed", shrink = TRUE, nrow = 2) +
      theme_light() + 
      labs(title = "Table 1.1 Number of women who justify wife-beating if a woman goes out without telling her husband",
           subtitle = "By age group and highest education level achieved",
           x = "Age Group",
           y = "Number of Respondents",
           fill = "Response")  +
      scale_fill_manual("Response", values = c("Yes" = "deepskyblue4", "No" = "skyblue2")) +
      theme(title = element_text(size = 13, colour = "steelblue4", face = "bold"),
            plot.subtitle = element_text(size = 14),
            legend.text = element_text(size = 10, colour = "grey55"),
            strip.text = element_text(size = 12, face = "bold"),
            panel.grid = element_line(colour = "lightgray"),
            legend.title = element_text(size = 12, colour = "steelblue4"),
            legend.justification = c("right", "top"),
            legend.position = "right",
            axis.title.x = element_text(size = 12, vjust = -0.4, colour = "steelblue4"), 
            axis.title.y = element_text(size = 12, vjust = 0.4, colour = "steelblue4"))
  })
  
  output$B_2012 <- renderPlot({
    
    justification_2012 %>% 
      filter(!B == "Don't Know") %>% 
      group_by(B, `Age Group`, `Education Level`) %>% 
      count() %>% 
      ggplot(aes(x = `Age Group`, y = n, fill = B)) + 
      geom_col() + 
      facet_wrap(~`Education Level`, scales = "fixed", shrink = TRUE, nrow = 2) +
      theme_light() + 
      labs(title = "Table 1.2 Number of women who justify wife-beating if a woman neglects her children",
           subtitle = "By age group and highest education level achieved",
           x = "Age Group",
           y = "Number of Respondents",
           fill = "Response")  +
      scale_fill_manual("Response", values = c("Yes" = "deepskyblue4", "No" = "skyblue2")) +
      theme(title = element_text(size = 14, colour = "steelblue4", face = "bold"),
            plot.subtitle = element_text(size = 14),
            legend.text = element_text(size = 10, colour = "grey55"),
            strip.text = element_text(size = 12, face = "bold"),
            panel.grid = element_line(colour = "lightgray"),
            legend.title = element_text(size = 12, colour = "steelblue4"),
            legend.justification = c("right", "top"),
            legend.position = "right",
            axis.title.x = element_text(size = 12, vjust = -0.4, colour = "steelblue4"), 
            axis.title.y = element_text(size = 12, vjust = 0.4, colour = "steelblue4"))
  })
  
  output$C_2012 <- renderPlot({
    
    justification_2012 %>% 
      filter(!C == "Don't Know") %>% 
      group_by(C, `Age Group`, `Education Level`) %>% 
      count() %>% 
      ggplot(aes(x = `Age Group`, y = n, fill = C)) + 
      geom_col() + 
      facet_wrap(~`Education Level`, scales = "fixed", shrink = TRUE, nrow = 2) +
      theme_light() + 
      labs(title = "Table 1.3 Number of women who justify wife-beating if a woman argues with her husband",
           subtitle = "By age group and highest education level achieved",
           x = "Age Group",
           y = "Number of Respondents",
           fill = "Response")  +
      scale_fill_manual("Response", values = c("Yes" = "deepskyblue4", "No" = "skyblue2")) +
      theme(title = element_text(size = 13, colour = "steelblue4", face = "bold"),
            plot.subtitle = element_text(size = 14),
            legend.text = element_text(size = 10, colour = "grey55"),
            strip.text = element_text(size = 12, face = "bold"),
            panel.grid = element_line(colour = "lightgray"),
            legend.title = element_text(size = 12, colour = "steelblue4"),
            legend.justification = c("right", "top"),
            legend.position = "right",
            axis.title.x = element_text(size = 12, vjust = -0.4, colour = "steelblue4"), 
            axis.title.y = element_text(size = 12, vjust = 0.4, colour = "steelblue4"))
  })
  
  
  output$E_2012 <- renderPlot({
    
    justification_2012 %>% 
      filter(!E == "Don't Know") %>% 
      group_by(E, `Age Group`, `Education Level`) %>% 
      count() %>% 
      ggplot(aes(x = `Age Group`, y = n, fill = E)) + 
      geom_col() + 
      facet_wrap(~`Education Level`, scales = "fixed", shrink = TRUE, nrow = 2) +
      theme_light() + 
      labs(title = "Table 1.4 Number of women who justify wife-beating if a woman burns the food",
           subtitle = "By age group and highest education level achieved",
           x = "Age Group",
           y = "Number of Respondents",
           fill = "Response")  +
      scale_fill_manual("Response", values = c("Yes" = "deepskyblue4", "No" = "skyblue2")) +
      theme(title = element_text(size = 14, colour = "steelblue4", face = "bold"),
            legend.text = element_text(size = 10, colour = "grey55"),
            plot.subtitle = element_text(size = 14),
            strip.text = element_text(size = 12, face = "bold"),
            panel.grid = element_line(colour = "lightgray"),
            legend.title = element_text(size = 12, colour = "steelblue4"),
            legend.justification = c("right", "top"),
            legend.position = "right",
            axis.title.x = element_text(size = 12, vjust = -0.4, colour = "steelblue4"), 
            axis.title.y = element_text(size = 12, vjust = 0.4, colour = "steelblue4"))
    
  })
  
  output$A_2007 <- renderPlot({
    
    justification_2007 %>% 
      filter(!A == "Don't Know") %>% 
      group_by(A, `Age Group`, `Education Level`) %>% 
      count() %>% 
      ggplot(aes(x = `Age Group`, y = n, fill = A)) + 
      geom_col() + 
      facet_wrap(~`Education Level`, scales = "fixed", shrink = TRUE, nrow = 2) +
      theme_light() + 
      labs(title = "Table 1.1 Number of women who justify wife-beating if a woman goes out without telling her husband",
           subtitle = "By age group and highest education level achieved",
           x = "Age Group",
           y = "Number of Respondents",
           fill = "Response")  +
      scale_fill_manual("Response", values = c("Yes" = "deepskyblue4", "No" = "skyblue2")) +
      theme(title = element_text(size = 13, colour = "steelblue4", face = "bold"),
            plot.subtitle = element_text(size = 14),
            legend.text = element_text(size = 10, colour = "grey55"),
            strip.text = element_text(size = 12, face = "bold"),
            panel.grid = element_line(colour = "lightgray"),
            legend.title = element_text(size = 12, colour = "steelblue4"),
            legend.justification = c("right", "top"),
            legend.position = "right",
            axis.title.x = element_text(size = 12, vjust = -0.4, colour = "steelblue4"), 
            axis.title.y = element_text(size = 12, vjust = 0.4, colour = "steelblue4"))
    
  })
  
  output$B_2007 <- renderPlot({
    
    justification_2007 %>% 
      filter(!B == "Don't Know") %>% 
      group_by(B, `Age Group`, `Education Level`) %>% 
      count() %>% 
      ggplot(aes(x = `Age Group`, y = n, fill = B)) + 
      geom_col() + 
      facet_wrap(~`Education Level`, scales = "fixed", shrink = TRUE, nrow = 2) +
      theme_light() + 
      labs(title = "Table 1.2 Number of women who justify wife-beating if a woman neglects her children",
           subtitle = "By age group and highest education level achieved",
           x = "Age Group",
           y = "Number of Respondents",
           fill = "Response")  +
      scale_fill_manual("Response", values = c("Yes" = "deepskyblue4", "No" = "skyblue2")) +
      theme(title = element_text(size = 14, colour = "steelblue4", face = "bold"),
            plot.subtitle = element_text(size = 14),
            legend.text = element_text(size = 10, colour = "grey55"),
            strip.text = element_text(size = 12, face = "bold"),
            panel.grid = element_line(colour = "lightgray"),
            legend.title = element_text(size = 12, colour = "steelblue4"),
            legend.justification = c("right", "top"),
            legend.position = "right",
            axis.title.x = element_text(size = 12, vjust = -0.4, colour = "steelblue4"), 
            axis.title.y = element_text(size = 12, vjust = 0.4, colour = "steelblue4"))
    
  })
  
  output$C_2007 <- renderPlot({
    
    justification_2007 %>% 
      filter(!C == "Don't Know") %>% 
      group_by(C, `Age Group`, `Education Level`) %>% 
      count() %>% 
      ggplot(aes(x = `Age Group`, y = n, fill = C)) + 
      geom_col() + 
      facet_wrap(~`Education Level`, scales = "fixed", shrink = TRUE, nrow = 2) +
      theme_light() + 
      labs(title = "Table 1.3 Number of women who justify wife-beating if a woman argues with her husband",
           subtitle = "By age group and highest education level achieved",
           x = "Age Group",
           y = "Number of Respondents",
           fill = "Response")  +
      scale_fill_manual("Response", values = c("Yes" = "deepskyblue4", "No" = "skyblue2")) +
      theme(title = element_text(size = 13, colour = "steelblue4", face = "bold"),
            plot.subtitle = element_text(size = 14),
            legend.text = element_text(size = 10, colour = "grey55"),
            strip.text = element_text(size = 12, face = "bold"),
            panel.grid = element_line(colour = "lightgray"),
            legend.title = element_text(size = 12, colour = "steelblue4"),
            legend.justification = c("right", "top"),
            legend.position = "right",
            axis.title.x = element_text(size = 12, vjust = -0.4, colour = "steelblue4"), 
            axis.title.y = element_text(size = 12, vjust = 0.4, colour = "steelblue4"))
    
  })
  
  output$E_2007 <- renderPlot({
    
    justification_2007 %>% 
      filter(!E == "Don't Know") %>% 
      group_by(E, `Age Group`, `Education Level`) %>% 
      count() %>% 
      ggplot(aes(x = `Age Group`, y = n, fill = E)) + 
      geom_col() + 
      facet_wrap(~`Education Level`, scales = "fixed", shrink = TRUE, nrow = 2) +
      theme_light() + 
      labs(title = "Table 1.4 Number of women who justify wife-beating if a woman burns the food",
           subtitle = "By age group and highest education level achieved",
           x = "Age Group",
           y = "Number of Respondents",
           fill = "Response")  +
      scale_fill_manual("Response", values = c("Yes" = "deepskyblue4", "No" = "skyblue2")) +
      theme(title = element_text(size = 14, colour = "steelblue4", face = "bold"),
            plot.subtitle = element_text(size = 14),
            legend.text = element_text(size = 10, colour = "grey55"),
            strip.text = element_text(size = 12, face = "bold"),
            panel.grid = element_line(colour = "lightgray"),
            legend.title = element_text(size = 12, colour = "steelblue4"),
            legend.justification = c("right", "top"),
            legend.position = "right",
            axis.title.x = element_text(size = 12, vjust = -0.4, colour = "steelblue4"), 
            axis.title.y = element_text(size = 12, vjust = 0.4, colour = "steelblue4"))
    
  })
  
  
  output$A_2002 <- renderPlot({
    
    justification_2002 %>% 
      filter(!A == "Don't Know") %>% 
      group_by(A, `Age Group`, `Education Level`) %>% 
      count() %>% 
      ggplot(aes(x = `Age Group`, y = n, fill = A)) + 
      geom_col() + 
      facet_wrap(~`Education Level`, scales = "fixed", shrink = TRUE, nrow = 2) +
      theme_light() + 
      labs(title = "Table 1.1 Number of women who justify wife-beating if a woman goes out without telling her husband",
           subtitle = "By age group and highest education level achieved",
           x = "Age Group",
           y = "Number of Respondents",
           fill = "Response")  +
      scale_fill_manual("Response", values = c("Yes" = "deepskyblue4", "No" = "skyblue2")) +
      theme(title = element_text(size = 13, colour = "steelblue4", face = "bold"),
            legend.text = element_text(size = 10, colour = "grey55"),
            plot.subtitle = element_text(size = 14),
            strip.text = element_text(size = 12, face = "bold"),
            panel.grid = element_line(colour = "lightgray"),
            legend.title = element_text(size = 12, colour = "steelblue4"),
            legend.justification = c("right", "top"),
            legend.position = "right",
            axis.title.x = element_text(size = 12, vjust = -0.4, colour = "steelblue4"), 
            axis.title.y = element_text(size = 12, vjust = 0.4, colour = "steelblue4"))
    
  })
  
  output$B_2002 <- renderPlot({
    
    justification_2002 %>% 
      filter(!B == "Don't Know") %>% 
      group_by(B, `Age Group`, `Education Level`) %>% 
      count() %>% 
      ggplot(aes(x = `Age Group`, y = n, fill = B)) + 
      geom_col() + 
      facet_wrap(~`Education Level`, scales = "fixed", shrink = TRUE, nrow = 2) +
      theme_light() + 
      labs(title = "Table 1.2 Number of women who justify wife-beating if a woman neglects her children",
           subtitle = "By age group and highest education level achieved",
           x = "Age Group",
           y = "Number of Respondents",
           fill = "Response")  +
      scale_fill_manual("Response", values = c("Yes" = "deepskyblue4", "No" = "skyblue2")) +
      theme(title = element_text(size = 14, colour = "steelblue4", face = "bold"),
            plot.subtitle = element_text(size = 14),
            legend.text = element_text(size = 10, colour = "grey55"),
            strip.text = element_text(size = 12, face = "bold"),
            panel.grid = element_line(colour = "lightgray"),
            legend.title = element_text(size = 12, colour = "steelblue4"),
            legend.justification = c("right", "top"),
            legend.position = "right",
            axis.title.x = element_text(size = 12, vjust = -0.4, colour = "steelblue4"), 
            axis.title.y = element_text(size = 12, vjust = 0.4, colour = "steelblue4"))
    
  })
  output$C_2002 <- renderPlot({
    
    justification_2002 %>% 
      filter(!C == "Don't Know") %>% 
      group_by(C, `Age Group`, `Education Level`) %>% 
      count() %>% 
      ggplot(aes(x = `Age Group`, y = n, fill = C)) + 
      geom_col() + 
      facet_wrap(~`Education Level`, scales = "fixed", shrink = TRUE, nrow = 2) +
      theme_light() + 
      labs(title = "Table 1.3 Number of women who justify wife-beating if a woman argues with her husband",
           subtitle = "By age group and highest education level achieved",
           x = "Age Group",
           y = "Number of Respondents",
           fill = "Response")  +
      scale_fill_manual("Response", values = c("Yes" = "deepskyblue4", "No" = "skyblue2")) +
      theme(title = element_text(size = 13, colour = "steelblue4", face = "bold"),
            legend.text = element_text(size = 10, colour = "grey55"),
            plot.subtitle = element_text(size = 14),
            strip.text = element_text(size = 12, face = "bold"),
            panel.grid = element_line(colour = "lightgray"),
            legend.title = element_text(size = 12, colour = "steelblue4"),
            legend.justification = c("right", "top"),
            legend.position = "right",
            axis.title.x = element_text(size = 12, vjust = -0.4, colour = "steelblue4"), 
            axis.title.y = element_text(size = 12, vjust = 0.4, colour = "steelblue4"))
    
  })
  output$E_2002 <- renderPlot({
    
    justification_2002 %>% 
      filter(!E == "Don't Know") %>% 
      group_by(E, `Age Group`, `Education Level`) %>% 
      count() %>% 
      ggplot(aes(x = `Age Group`, y = n, fill = E)) + 
      geom_col() + 
      facet_wrap(~`Education Level`, scales = "fixed", shrink = TRUE, nrow = 2) +
      theme_light() + 
      labs(title = "Table 1.4 Number of women who justify wife-beating if a woman burns the food",
           subtitle = "By age group and highest education level achieved",
           x = "Age Group",
           y = "Number of Respondents",
           fill = "Response")  +
      scale_fill_manual("Response", values = c("Yes" = "deepskyblue4", "No" = "skyblue2")) +
      theme(title = element_text(size = 14, colour = "steelblue4", face = "bold"),
            plot.subtitle = element_text(size = 14),
            legend.text = element_text(size = 10, colour = "grey55"),
            strip.text = element_text(size = 12, face = "bold"),
            panel.grid = element_line(colour = "lightgray"),
            legend.title = element_text(size = 12, colour = "steelblue4"),
            legend.justification = c("right", "top"),
            legend.position = "right",
            axis.title.x = element_text(size = 12, vjust = -0.4, colour = "steelblue4"), 
            axis.title.y = element_text(size = 12, vjust = 0.4, colour = "steelblue4"))
    
  })
  
  output$violence_plot <- renderPlot({
    
    violence %>% 
      ggplot(aes(x = Type, y = Percent, fill = Response)) +
      geom_col(width = 0.5) +
      theme_light() + 
      labs(title = 
             "Percentage of female-identified respondents who report 
           having experienced some form of violence by their husbands",
           subtitle = "By Type of Violence",
           x = "Type of Violence",
           y = "Percentage of Respondents (%)",
           fill = "Response")  +
      scale_fill_manual("Response", values = c("Yes" = "deepskyblue4", "No" = "skyblue2")) +
      theme(title = element_text(size = 14, colour = "steelblue4", face = "bold"),
            plot.subtitle = element_text(size = 14),
            legend.text = element_text(size = 10, colour = "grey55"),
            panel.grid = element_line(colour = "lightgray"),
            legend.title = element_text(size = 12, colour = "steelblue4"),
            legend.justification = c("right", "top"),
            legend.position = "right",
            axis.title.x = element_text(size = 13, vjust = -0.4, colour = "steelblue4"), 
            axis.title.y = element_text(size = 13, vjust = 0.4, colour = "steelblue4"))
    
  })
  
  output$physical_violence_plot <- renderPlot({
    
    new %>% 
      group_by(Type, `Age Group`, Response) %>% 
      count() %>% 
      ggplot(aes( x = `Age Group`, y = n, fill = Response)) +
      geom_bar(stat = "identity", position = 'dodge') + 
      facet_wrap(~Type, nrow = 4) +
      theme_light() + 
      labs(title = "Physical Violence",
           subtitle = "by ..",
           x = "Age Group",
           y = "Number of Respondents",
           fill = "Response")  +
      scale_fill_manual("Response", values = c("Never" = "slategray1", 
                                               "Often" = "lightblue1",
                                               "Sometimes" = "skyblue2",
                                               "Yes, but not in the last 12 months" = "deepskyblue4")) +
      theme(title = element_text(size = 14, colour = "steelblue4", face = "bold"),
            legend.text = element_text(size = 10, colour = "grey55"),
            strip.text = element_text(size = 13, face = "bold"),
            panel.grid = element_line(colour = "lightgray"),
            legend.title = element_text(size = 12, colour = "steelblue4"),
            legend.justification = c("right", "top"),
            legend.position = "right",
            axis.title.x = element_text(size = 15, vjust = -0.4, colour = "steelblue4"), 
            axis.title.y = element_text(size = 15, vjust = 0.4, colour = "steelblue4"))
  })
  
  output$kable <- function() {
    
    bind %>%  
      kable(
        align = c(rep('c', 1), rep("c", 9))) %>%
      kable_styling(bootstrap_options = "hover", 
                    position = "center", 
                    row_label_position = "c", 
                    font_size = 14, 
                    full_width = TRUE) %>% 
      row_spec(0, bold = F, color = "black", font_size = 13, align = "c") %>% 
      column_spec(1, width = "7em", bold = T) %>% 
      column_spec(2:9, width = "30em") %>% 
      group_rows("Age Group", 1, 7, 
                 label_row_css = "no-border-bottom; float:left; color: #37778f;") %>% 
      group_rows("Region", 8, 10, 
                 label_row_css = "no-border-bottom; float:left; padding-top: 20px; color: #37778f;") %>% 
      group_rows("Education", 11, 14, 
                 label_row_css = "no-border-bottom; float:left; padding-top: 20px;  color: #37778f;") 
    
    
  }
  
  output$violence_plot_2007 <- renderPlot({
    
    violence_2007 %>% 
      ggplot(aes(x = Type, y = Percent, fill = Response)) +
      geom_col(width = 0.5) +
      theme_light() + 
      labs(title = 
             "Percentage of female-identified respondents who report 
           having experienced some form of violence by their husbands",
           subtitle = "By Type of Violence",
           x = "Type of Violence",
           y = "Percentage of Respondents (%)",
           fill = "Response")  +
      scale_fill_manual("Response", values = c("Yes" = "deepskyblue4", "No" = "skyblue2")) +
      theme(title = element_text(size = 14, colour = "steelblue4", face = "bold"),
            plot.subtitle = element_text(size = 14),
            legend.text = element_text(size = 10, colour = "grey55"),
            panel.grid = element_line(colour = "lightgray"),
            legend.title = element_text(size = 12, colour = "steelblue4"),
            legend.justification = c("right", "top"),
            legend.position = "right",
            axis.title.x = element_text(size = 13, vjust = -0.4, colour = "steelblue4"), 
            axis.title.y = element_text(size = 13, vjust = 0.4, colour = "steelblue4"))
    
    
  })
  
  output$kable_2007 <- function() {
    
    bind_2007 %>%  
      kable(
        align = c(rep('c', 1), rep("c", 9))) %>%
      kable_styling(bootstrap_options = "hover", 
                    position = "center", 
                    row_label_position = "c", 
                    font_size = 14, 
                    full_width = TRUE) %>% 
      row_spec(0, bold = F, color = "black", font_size = 13, align = "c") %>% 
      column_spec(1, width = "7em", bold = T) %>% 
      column_spec(2:8, width = "30em") %>% 
      group_rows("Age Group", 1, 6, 
                 label_row_css = "no-border-bottom; float:left; color: #37778f;") %>% 
      group_rows("Region", 7, 9, 
                 label_row_css = "no-border-bottom; float:left; padding-top: 20px; color: #37778f;") %>% 
      group_rows("Education", 10, 13, 
                 label_row_css = "no-border-bottom; float:left; padding-top: 20px;  color: #37778f;") 
    
    
  }
  
  
  
  output$unicef_map <- renderLeaflet({
    
    
    
    leaflet(options = leafletOptions(minZoom = 2)) %>% 
      addProviderTiles("Wikimedia", 
                       group = "Coloured Political Boundaries Map") %>% 
      setMaxBounds(lng1 = 180 ,
                   lat1 = 90, 
                   lng2 = -180, 
                   lat2 = -90) %>% 
      addCircleMarkers(lng = unicef_clean$X, 
                       lat = unicef_clean$Y,
                       popup = paste0("As of ", unicef_clean$Year, ", the proportion of the ", unicef_clean$Sex, "-identified population in ", sample_data$Country,
                                      " aged 18-29 years that experienced any form of sexual violence by the age of 18 was ",
                                      unicef_clean$Percentage, "."),
                       label = unicef$Country,
                       clusterOptions = markerClusterOptions(),
                       labelOptions = labelOptions(noHide = F, 
                                                   textsize = "10px", 
                                                   direction = "bottom")) %>% 
      addSearchOSM() %>%  
      addResetMapButton()
    
  })
  
  output$unicef_table <- function() {
    
    unicef_table <-
      unicef_clean_2 %>% 
      group_by(Sex, Year) %>% 
      summarize(average = mean(Percentage)/100) %>% 
      spread(Sex, average) %>% 
      adorn_pct_formatting(digits = 2) %>% 
      select(Year, "Average Percentage of Female-Identified Respondents that Experienced Sexual Violence Before Age of 18" = "Female", "Average Percentage of Male-Identified Respondents that Experienced Sexual Violence Before Age of 18" = "Male")
    
    
    unicef_table %>% 
      kable(align = c(rep('c', 1), rep("c", 2))) %>%
      kable_styling(bootstrap_options = "hover", 
                    position = "center", 
                    row_label_position = "c", 
                    font_size = 14, 
                    full_width = FALSE) %>% 
      row_spec(0, bold = F, color = "#37778f", font_size = 15) %>% 
      column_spec(1, width = "7em", bold = T) %>% 
      column_spec(2:3, width = "20em") 
  }
}

# Run the application 
shinyApp(ui = ui, server = server) 



