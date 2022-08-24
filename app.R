#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#  title: "Visualize and check corrected Unispec Data"
#  author: "Jim Laundre"
#  date: "2022-07-07"

#  Followed Ruby An's shiny app for visualizing Unispec data and from
#  Mastering Shiny 10 Dynamic UI (https://mastering-shiny.org/action-dynamic.html)
#
#   
library(shiny,warn.conflicts = FALSE)
library(tidyverse,warn.conflicts = FALSE)
library(DT,warn.conflicts = FALSE)
library(plotly,warn.conflicts = FALSE)
library(lubridate,warn.conflicts = FALSE)
library("viridis",warn.conflicts = FALSE)
library(rstudioapi)
library(data.table)
source("R/helper.R",local = TRUE)

# ---------------------------------------------------------------------------------
# Before lunching the shiny app, read in data from past years and any newer 
#   index.rds files. Note: Past data have been cleaned and sites standardized
#  These data objects are scoped across all sessions

  past_indices_data <- readRDS("data/indices_2014-2021.rds")
  
#  Get newer index.rds files by searching for files in a selected folder (sub-folder included). 
#   The function "get_current_indices" is in R/helper.R 
  # data_path <- selectDirectory(caption = "Select a folder to search for index.rds files")
  current_indices_data <- read_rds("data/2022_indices.rds")
    # get_current_indices(data_path) %>%
    # dplyr::filter(!Treatment %in% c("DARK", "REF")) %>%
    # mutate(collection_year = "current",Year = lubridate::year(Date),
    #        DOY = lubridate::yday(Date)) %>%
    # select(Site,Year,Date,DOY,Treatment,NDVI,collection_year) %>%
    # group_by(Site, Year, DOY, Date, Treatment, collection_year)

# Define UI for application----------------------------------------------------------
ui <- (fluidPage(
  navbarPage(
    "Arctic LTER Spectral Reflectance Data",
    
       tabPanel("1. Plot Comparison",
             sidebarLayout(
               sidebarPanel(
                 # Note fileInput creates a data frame (name, size, and temporary file path of the files uploaded)
                 
                 helpText("Select an indices rds file"),
                 
                 fileInput("processed_spectra",
                   "Select an ...index.rds file to plot.",
                   multiple = FALSE,
                   accept = c(".rds")
                 ),
                 
                 # Input: Checkboxes ----
                 checkboxGroupInput("choice_site", "Site",
                                    choices= NULL),
                 checkboxGroupInput("choice_treatment", "Treatment",
                                    choices= NULL),
                 checkboxGroupInput("choice_block", "Block",
                                    choices= NULL)
               
               ),
               mainPanel(
                 tabsetPanel(
                 tabPanel("Plot Graphs",
                        plotlyOutput("plot_reflec"),
                        plotlyOutput("plot_indices")),
                 tabPanel("Compare to Past Years",
                        plotlyOutput("plot_past_years"))
                 )
              )
          )
       )
  )
))

# Define server logic 
server <- function(input, output, session) {
  
# Read the RDS file 
  processed_data <- reactive ({
    readRDS(input$processed_spectra$datapath) %>%
    mutate(across(where(is.factor), as.character)) %>%  # Convert factors so we can filter
    dplyr::filter(!Treatment %in% c("DARK", "REF")) %>%
    mutate(Block = as.numeric(str_extract(Block, "\\d"))) # convert "B1", etc  to numeric
  })
  
# And unnest the spectra
  processed_spectra <- reactive({
    req(input$processed_spectra)
    processed_data() %>%
   unnest(cols = c(Spectra)) %>%
   mutate(across(where(is.factor), as.character)) 
    })
  
# And unnest the indices 
  processed_indices <- reactive({
    req(input$processed_spectra)
    processed_data() %>%
    unnest(cols = c(Indices)) %>%
      dplyr::filter(Index %in% c("NDVI"))%>%
    rename(NDVI = Value)
  })
#* Select box updates.  When a file is loaded, get the sites and blocks ----
#* and select the first ones
  observeEvent(input$processed_spectra,{

    selected_b <-unique(processed_spectra() %>% select(Block))$Block
    updateCheckboxGroupInput(session, inputId = "choice_block", choices = selected_b,
                             selected = selected_b[1])

    selected_s <-unique(processed_spectra() %>% select(Site))$Site
    updateCheckboxGroupInput(session, inputId = "choice_site", choices = selected_s,
                             selected = selected_s[1])
    
  })

#  When a site is selected update the treatment and block choices 
  
  observeEvent(input$choice_site,{
    
    before_selected <-input$choice_treatment
    selected_t <-unique(processed_spectra() %>% 
                          filter(Site == input$choice_site) %>%
                          select(Treatment))$Treatment %>% factor() %>% levels()
    updateCheckboxGroupInput(session, inputId = "choice_treatment", choices = selected_t,
                             selected = before_selected)
    
    before_selected <-input$choice_block
    selected_b <-unique(processed_spectra() %>% 
                          filter(Site == input$choice_site) %>%
                          select(Block))$Block %>% factor() %>% levels()
    updateCheckboxGroupInput(session, inputId = "choice_block", choices = selected_b,
                             selected = before_selected)    
  })

#  Create the reactive data for each plots based on the selected choices
#
  sub_data_reflec <- reactive({
     req(input$processed_spectra, input$choice_site, input$choice_block,
         input$choice_treatment )
     
    processed_spectra() %>%
    filter(Site %in% input$choice_site,Block %in% input$choice_block,Treatment %in% input$choice_treatment) %>%
    group_by(Date, Site, Block, Treatment) %>%
       mutate(Replicate = as.character(Replicate), Block = as.factor(Block))
   })
   
   sub_data_indices <- reactive({
     req(input$processed_spectra, input$choice_site, input$choice_block, input$choice_treatment )
     
     processed_indices() %>%
       filter(Site %in% input$choice_site,Block %in% input$choice_block,Treatment %in% input$choice_treatment) %>%
       group_by(Date, Site, Block, Treatment)%>%
       mutate(Replicate = as.character(Replicate))
   })
   
  sub_data_all <- reactive({
    req(processed_indices(),input$choice_site,input$choice_treatment)
    plot_data <- current_indices_data %>%
      full_join(past_indices_data,by = c("Year", "Date", "Site", "Treatment", "NDVI",
                                      "DOY","collection_year")) %>%
     filter(Site %in% input$choice_site,Treatment %in% input$choice_treatment)%>%
      summarise(sd = sd(NDVI,na.rm = T),
                NDVI = mean(NDVI, na.rm = T))
    return(plot_data)
  })
  
  # Plot Output -------------------------------------------------------------
   #  TODO use plotly to plot instead of just converting plot
  output$plot_reflec <- renderPlotly({
    req(sub_data_reflec()) 
    ### Plot
    plotly::ggplotly(
      ggplot(sub_data_reflec(),mapping = aes(x = Wavelength, y = Reflectance )) + 
      geom_line(aes(color = Replicate, linetype = Block)) +
      facet_grid(Site ~ Treatment) +
      theme_light()+
      scale_color_viridis(discrete = TRUE, option = "D")+
      theme(legend.position = "left"))
  })
  output$plot_indices <- renderPlotly({
     req(sub_data_indices()) 
     ### Plot
     plotly::ggplotly(
       ggplot(sub_data_indices(),mapping = aes(x = Block, y = NDVI )) + 
       geom_point(aes(color = Replicate)) +
       facet_grid(Site ~ Treatment) +
       theme_light()+
       scale_color_viridis(discrete = TRUE, option = "D")+
       theme(legend.position = "left"))
   })
  output$plot_past_years <- renderPlotly({
    req(sub_data_all())
    # Select all the blocks since it is an average of all the blocks. TODO hide blocks choices
    selected_b <-unique(processed_spectra() %>% select(Block))$Block
    updateCheckboxGroupInput(session, inputId = "choice_block", choices = selected_b,
                             selected = selected_b)
    plotly::ggplotly(
      ggplot(data = sub_data_all(), aes(x = DOY,y = NDVI, color = factor(Year))) +
        geom_point(aes(alpha = 0.5)) +
        geom_line() +
        facet_grid(Site ~ Treatment) +
        #formatting
        theme_minimal() +
        scale_color_viridis(discrete = TRUE, option = "D")
       # scale_color_manual(values = c("red", "grey"))
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
