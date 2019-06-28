

ui <- fluidPage(
  
  # App title ----
  titlePanel("Tabsets"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select the random distribution type ----
      selectInput("famAuto","Family",choices=unique(MatchDataObj$data$family), selected='', multiple=TRUE),
      #autocomplete_input("famAuto", value='',label = 'Family',options=unique(MatchDataObj$data$family)),
      br(),
      selectInput("countyAuto", 'County',choices=c("",unique(CountyCodes$CountyCode)),selected=''),
      br(),
      selectInput("instInput","Primary Seed Bank:",c(
        "Select" = "",
        "Rancho Santa Ana" = "RSA",
        "San Diego Zoo" = "SDZG",
        "Santa Barabara BG" = "SBBG",
        "UC Santa Cruz" = "UCSC",
        "UC Berkeley BG" = "UCB",
        "Rae Selling Berry" = "BERR"
      )
      ),
      br(),
      textOutput("FilterText")
      
      
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