

ui <- fluidPage(
  
  # App title ----
  titlePanel("Tabsets"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select the random distribution type ----
      radioButtons("fam", "Family:",
                   c("Fagaceae" = "Fagaceae",
                     "Phrymaceae" = "Phrymaceae",
                     "Lamiaceae" = "Lamiaceae")),
      
      br(),
      
      autocomplete_input("famAuto", value='',label = 'Family',options=unique(MatchDataObj$data$family))
      
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("DendroGram", plotOutput("PlotGGTree"))
                  ,
                  tabPanel("Table", dataTableOutput("FilteredTable"))
      )
      
    )
  )
)