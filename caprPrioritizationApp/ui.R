
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  # App title ----
  titlePanel(""),
  navbarPage("Explorer Tools",  
      tabPanel("Phylogeny",
               h2("Phylogenetic Analyses of Seed Collections"),
               h3("How was the tree made?"),
               p("Katie created a phylogeny of the California plant list using V.Phylomaker (Jin & Qian 2019) which uses the 
most recent seed plant megatree available from Smith & Brown 2018 phylogeny, which used the Open Tree of Life
project to generate a tree with 79,881 plant taxa. Species that were not in this phylogeny I added to the closest genus or species nodes."),
               h3("Phylogenetic Signal"),
               p("I calculated phylogenetic signal for presence/absence of seed and living collections of the phylogeny using the phylo.d function in the caper package. For this metric, 0=no signal, 1 = completely determined by phylogency. Somewhat surprisingly, there is greater phylogency in seed collections (0.83) compared to living collections (0.56), perhaps reflecting phylogenetic signal in 
                 non-orthodox seeds. You can use the tool below to calculate signal for pruned trees"),
               h3("Evolutionary Distinctiveness "),
               p("I used the picante package to calculate phylogenetic distinctiveness for each species. The values can be seen in the table tab. These can ultimately be used in our prioritization"),
               h3("What's next?"),
               p("I am working on how people can view the tree more easily. I want to be able to zoom in, but the ggplot trees that allow you to do that are very slow. For now, I am letting people toggle the label sizes."),
               
               
      # Sidebar layout with input and output definitions ----
      sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
          sliderInput("sliderTextSize","Select Size of Labels",min=0.1,max=1.5, step=0.1,value=0.3),
          selectInput("famAuto","Family",choices=unique(MatchDataObj$data$family), selected='', multiple=TRUE),
          #autocomplete_input("famAuto", value='',label = 'Family',options=unique(MatchDataObj$data$family)),
          br(),
          selectInput("countyAuto", 'County (rare plants only):',choices=c("",unique(CountyCodes$CountyCode)),selected='',multiple=TRUE),
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
         br(),
         actionButton("calculateLivingSignal", "Calculate Phylogenetic Signal in Living Collections"),
         textOutput("PhyloDSummaryLiving")
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
