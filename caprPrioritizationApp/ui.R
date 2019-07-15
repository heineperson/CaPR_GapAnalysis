
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  # App title ----
  titlePanel(""),
  navbarPage("Explorer Tools",  
      tabPanel("Phylogeny",
      
      # Sidebar layout with input and output definitions ----
      sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
          
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
            "Rae Selling Berry" = "BERR")),
          br(),
          textOutput("FilterText")
          ,textOutput("PhyloDSummarySeed")
        ),
    
    # Main panel for displaying outputs ----
      mainPanel(
        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(type = "tabs",
                    tabPanel("DendroGram", plotOutput("PlotGGTree")),
                    tabPanel("Table", dataTableOutput("FilteredTable")))
                )
           )
          #)
      ),

  tabPanel("Collection Types",
           sidebarLayout(
             
             # Sidebar panel for inputs ----
             sidebarPanel(
               selectInput("rareRank","Rare Plant Rank:",choices=c(
                 "All Species" = "",
                 "1B" = "1B",
                 "2B" = "2B",
                 "3" = "3",
                 "4" = "3",
                 "1B.1" = "1B.1"),multiple=TRUE)
               
             ),
             mainPanel(
               tabsetPanel(type = "tabs",
                           tabPanel("Venn Diagram", plotOutput("VennDiagram")),
                           tabPanel("Table", p("x"))
             )   
           )
    
  )
)))
