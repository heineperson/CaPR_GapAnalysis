
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  # App title ----
  titlePanel(""),
  navbarPage("Explorer Tools",  
      
tabPanel("Prioritization Tool",
         h2("How to prioritize collections"),
         p("The goal was to create something similar to the PIECES tool. Currently, the filters work but the value rankings don't. 
           I wasn't sure if the value rankings for rarity and collection status make sense for us because might be bound by fundign to go for uncollected 1Bs"),
         sidebarLayout(
           
           # Sidebar panel for inputs ----
           sidebarPanel(
             h2("Collector Specific Inputs"),
             h3("Collecting Radius"),
             textInput("yourLocation","Enter California City"),
             textInput("milesFrom","Miles you are willing to travel from Above City"),
             h3("Geographic Filters"),
             selectInput("countySample", 'Enter Counties You Prefer to Collect: ',choices=c("",sort(unique(EOdata$KEYCOUNTY))),selected='',multiple=TRUE),
             selectInput("ecoRegion", 'Enter EcoRegions You Prefer to Collect: ',choices=c("",sort(unique(capr$JEP_REG))),selected='',multiple=TRUE),
             h3("Ownership Filters"),
             selectInput("landownBroad", 'Land Ownership Type (BLM, USFS): ',choices=c("",sort(unique(EOdata$BroadestOwnership))),selected='',multiple=TRUE),
             #selectInput("nationalForest", 'National Forest (from federal lands spatial layer): ',choices=c("",unique(EOdata$forest)),selected='',multiple=TRUE),
             selectInput("specificLands","Specific Landownership (from CNDDB): ", choices=c("",specificLandVec),selected='',multiple=TRUE),
             h3("Collection & Rarity Filters"),
             selectInput("rarityRank","CNPS Ranking ", choices=c("",sort(unique(EOdata$RPLANTRANK))),selected='',multiple=TRUE),
             radioButtons("collectionSpp","Species Already in Seed Collection: ", choices=c("Any"=NULL,"Yes"="Yes","No"="No")),
             
             h2("Value Rankings (NOT WORKING)"),
             textInput("rarityRank","Importance of Rarity"),
             textInput("locationRank","Importance of Nearness to Your Location"),
             textInput("evoRank","Importance of Evolutionary Distinctness"),
             textInput("collectionRank","Importance that Species Is Not Yet collected")
             
           ),
           mainPanel(
             h2("Tables Filtered By Inputs"),
             tabsetPanel(type="tabs",
                         tabPanel("occurrences",
               dataTableOutput("occurrencesPriority")),
               tabPanel("species",
                        dataTableOutput("sppTablePriority")
              
             ))
           )
         )
         ),
tabPanel("Phylogeny",
         h2("Phylogenetic Analyses of Seed Collections"),
         h3("How was the tree made?"),
         p("Katie created a phylogeny of the California plant list using V.Phylomaker (Jin & Qian 2019) which uses the 
           most recent seed plant megatree available from Smith & Brown 2018 phylogeny, which used the Open Tree of Life
           project to generate a tree with 79,881 plant taxa. Species that were not in this phylogeny I added to the closest genus or species nodes."),
         h3("Phylogenetic Signal"),
         p("I calculated phylogenetic signal for presence/absence of seed and living collections of the phylogeny using the phylo.d function in the caper package. For this metric, 0=no signal, 1 = completely determined by phylogency. Somewhat surprisingly, there is greater phylogency in seed collections (0.83) compared to living collections (0.62), perhaps reflecting phylogenetic signal in 
           non-orthodox seeds. You can use the tool below to calculate signal for pruned trees"),
         h3("Evolutionary Distinctiveness "),
         p("I used the picante package to calculate phylogenetic distinctiveness for each species. The values can be seen in the table tab. These can ultimately be used in our prioritization"),
         h3("What's next?"),
         p("Filter by vascular plants. Ask phylogenetics team at SBBG if this is a good approach. I am working on how people can view the tree more easily. I want to be able to zoom in, but the ggplot trees that allow you to do that are very slow. For now, I am letting people toggle the label sizes."),
         
         
         # Sidebar layout with input and output definitions ----
         sidebarLayout(
           
           # Sidebar panel for inputs ----
           sidebarPanel(
             sliderInput("sliderTextSize","Select Size of Labels",min=0.1,max=1.5, step=0.1,value=0.3),
             selectInput("famAuto","Family",choices=unique(MatchDataObj$data$family), selected='', multiple=TRUE),
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
         h2("Collection Categories"),
         tags$div(
           HTML("<p>I created the following categories for accessions:&nbsp;</p>
                <ul>
                <li><strong>Conservation Collections: Maternal Lines &amp; Wild</strong>: Seed accessions with available provenance data that have been collected by maternal lines. I added a few living accessions of conservation quality from RSA.</li>
                <li><strong>Seed: Bulked or grown Ex-Situ&nbsp;</strong>Seed accessions with available provenance data that were collected in bulk (or do not specify). This includes seed grown ex-situ from known wild source</li>
                <li><strong>Seed: Data deficient:&nbsp;</strong>Seed accessions without available wild locality information.</li>
                <li><strong>Living: Wild</strong>: Living accessions with known wild provenance</li>
                <li><strong>Living: Unknown</strong>: Living accessions with unknown or garden provenance</li>
                </ul>")
           ),
         
         h3("What do I need from RSA?"),
         p("Provance code update: the original template I was using didn't make a ton of sense - I think you probably have this info"),
         h3("What do I need from SBBG?"),
         p("Maternal Lines vs. Bulked for some collections, living collectiosn that qualify for conservation collections"),
         h3("What do I need from UCB?"),
         p("Seed Counts, Preparations (bulked vs. maternal lines), provenance (wild vs. cultivated), georeferencing info where possible, living collectiosn that qualify for conservation collections"),
         h3("What do I need from UCSC?"),
         p("Living collections if possible, preparations (bulked vs. maternal lines), seed counts"),
         h3("What do I need from RPBG?"),
         p("I added their species list for living collections but I need their actual accessions list"),
         
         
         
         tabsetPanel(type = "tabs",
                     tabPanel("Progress Graph", 
                              plotOutput("ProgressPlot",height=800,width=800),
                              dataTableOutput("VennTable")
                     ),
                     tabPanel("Venn Diagram",
                              sidebarLayout(
                                
                                # Sidebar panel for inputs ----
                                sidebarPanel(
                                  selectInput("rareRank","Rare Plant Rank:",choices=c(
                                    "All Species" = "",
                                    "1B" = "1B",
                                    "2B" = "2B",
                                    "3" = "3",
                                    "4" = "4"),multiple=TRUE)
                                ),
                                mainPanel(plotOutput("VennDiagram",height=800,width=800)))),
                     tabPanel("Gold Star Species", 
                              h2("Gold Star Species: >= 5 maternal line pops & > 3000 seeds"),
                              dataTableOutput("GoldStar"),
                              h3("Should we rank within our seed collections on a finer scale?"),
                              tags$div(
                                HTML(
                                  "<ul>
<li><strong>Seed Counts</strong></li>
<li><strong>Accession Age</strong></li>
<li><strong>Number of Maternal Lines</strong></li>
<li><strong>Germination Tests</strong></li>
<li><strong>Number of populations</strong></li>
</ul>"
                                )
                              )
                     ),
                     tabPanel("Data Deficient Accessions",
                              h2("Accession missing info needed to categorize"),
                              dataTableOutput("dataDeficient"))
                     
         )),

tabPanel("Maps",
         h2("Coming soon!"),
         p("I have extracted Jepson Regions for all the occurrences and accessions - need to visualize this"))


))
