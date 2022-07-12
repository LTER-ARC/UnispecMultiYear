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


library(shiny)
library(tidyverse,warn.conflicts = FALSE)
library(DT,warn.conflicts = FALSE)
library(plotly,warn.conflicts = FALSE)


# Define UI for application----------------------------------------------------------
ui <- (fluidPage(
  navbarPage(
    "Arctic LTER Spectral Reflectance Data",
    
       tabPanel("1. Plot Comparison",
             sidebarLayout(
               sidebarPanel(
                 # Note fileInput creates a data frame (name, size, and temporary file path of the files uploaded)
                 
                 helpText("Load a processed spu indices rds file"),
                 
                 fileInput("processed_spectra",
                   "Select a processed ...index.rds file",
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
               mainPanel("Plots",
                        plotlyOutput("plot_reflec"),
                        plotlyOutput("plot_indices"))
             ))
  )
))

# Define server logic 
server <- function(input, output, session) {
  
# Read the RDS file 
  processed_data <- reactive (readRDS(input$processed_spectra$datapath))
  
# And unnest the spectra
  processed_spectra <- reactive({
    req(input$processed_spectra)
    processed_data() %>%
      unnest(cols = c(Spectra)) %>%
    dplyr::filter(!Treatment %in% c("DARK", "REF"))})
  
# And unnest the indices 
  processed_indices <- reactive({
    req(input$processed_spectra)
    processed_data() %>%
      unnest(cols = c(Indices)) %>%
      dplyr::filter(!Treatment %in% c("DARK", "REF"), Index %in% c("NDVI"))%>%
    rename(NDVI = Value)})
  
#* Select box updates.  When a file is loaded get the sites and blocks
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
       mutate(Replicate = as.character(Replicate))
   })
   
   sub_data_indices <- reactive({
     req(input$processed_spectra, input$choice_site, input$choice_block, input$choice_treatment )
     
     processed_indices() %>%
       filter(Site %in% input$choice_site,Block %in% input$choice_block,Treatment %in% input$choice_treatment) %>%
       group_by(Date, Site, Block, Treatment)%>%
       mutate(Replicate = as.character(Replicate))
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
       theme(legend.position = "left"))
   })   
   
}

# Run the application 
shinyApp(ui = ui, server = server)
