
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  # App title ----
  titlePanel(""),
  navbarPage("Explorer Tools",  
      tabPanel("Phylogeny",
               h2("Phylogenetic Analyses of Seed Collections"),
               p("Katie created a phylogeny of the California plant list using V.Phylomaker (Jin & Qian 2019) which uses the 
most recent seed plant megatree available from Smith & Brown 2018 phylogeny, which used the Open Tree of Life
project to generate a tree with 79,881 plant taxa. Species that were not in this phylogeny I added to the closest genus or species nodes."),
               p("I am working on how people can view the tree more easily. I want to be able to zoom in, but the ggplot trees that allow you to do that are very slow. For now, I am letting people toggle the label sizes. 
"),

               
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
          p("Number of observations:"),textOutput("FilterText"),
          actionButton("calculateSignal", "Calculate Phylogenetic Signal in Seed Collections")
         ,textOutput("PhyloDSummarySeed"),
         actionButton("calculateLivingSignal", "Calculate Phylogenetic Signal in Living Collections")
         ,textOutput("PhyloDSummaryLiving")
        ),
    
    # Main panel for displaying outputs ----
      mainPanel(
        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(type = "tabs",
                    tabPanel("DendroGram", 
                             plotOutput("PlotGGTree"
                                        # ,dblclick = "plottree_dblclick",
                                        #               brush = brushOpts(
                                        #                 id = "plottree_brush",
                                        #                 resetOnNew = TRUE)
                                        )
                             ),
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
                 "4" = "4",
                 "1B.1" = "1B.1"),multiple=TRUE),
               tags$div(
                  HTML("<p>Tier01 = Maternal Lines Seed Collections (and Specific Living Collections) of known wild origin</p>
<p>Tier02 = Bulked (or unknown) Seed Collections of known wild origin</p>
<p>Tier03 = Any seed collections of unknown origin</p>
<p>Tier04 = Living Collections of known wild origin (possibly few individuals)</p>
<p>Tier05 = Living Collection of unknown origin</p>
<p></p>"))
               
             ),
             mainPanel(
               tabsetPanel(type = "tabs",
                           tabPanel("Venn Diagram", plotOutput("VennDiagram")),
                           tabPanel("Table", dataTableOutput("VennTable"))
             )   
           )
    
  )
)))
