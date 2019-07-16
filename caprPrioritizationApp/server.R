server <- function(input, output) {
  
# Defining Reactive Filters
  Filters <- reactive({
    
    if(is.null(input$famAuto)){
      famFilter <- seq(1,length(MatchDataObj$data$family))
    }else{
      famFilter <- which(MatchDataObj$data$family%in%input$famAuto)
    }
    
    if(is.null(input$countyAuto)){
      countyFilter <- seq(1,length(MatchDataObj$data$family))
      }else{
     countyFilter <- which(trimws(unlist(strsplit(as.character(MatchDataObj$data$Counties), ",")))%in% trimws(unlist(strsplit(input$countyAuto, ","))))
     }

    if(is.null(input$instInput)){
      instFilter <- seq(1,length(MatchDataObj$data$institutions))
    }else{
      instFilter <- which(grepl(input$instInput,as.character(MatchDataObj$data$institutions)))
    }

    filters <- Reduce(intersect, list(famFilter,countyFilter,instFilter))
     return(filters)
  })

# Creating Tree for Plotting that Filters by Family  
  TreeFilter <- reactive({
    pruned.tree<-drop.tip(MatchDataObj$phy,MatchDataObj$phy$tip.label[-Filters()])
    return(pruned.tree)
  })  
  
# Creating Table that Filters by Family  
   SppTableFilter <- reactive({
     pruned.data<-(MatchDataObj$data)[Filters(),]
     return(pruned.data) 
   })
  
  
# Plotting Tree
   
  # ranges <- reactiveValues(x = NULL, y = NULL)
   
  output$PlotGGTree <- renderPlot(
    # Expression
    {
      phy2 <- TreeFilter()
      dat <- MatchDataObj$data[MatchDataObj$data$nameOnPhylogeny%in%phy2$tip.label,]
      p <- trait.plot(collapse.singles(phy2), dat, cols = list(InSeedCollection = c("pink", "red"), 
                                                               InLivingCollection = c("lightblue", "blue")),
                                                    str = list(c("No","Yes"),c("No","Yes")),
                                              cex.lab=input$sliderTextSize,cex.legend=2)
      
      #  
      # treeTibble <- as_tibble(phy2)
      # dataTibble <- as_tibble(dat)
      # dataTibble$nameOnPhylogeny = as.character(dataTibble$nameOnPhylogeny)
      # y <- full_join(treeTibble, dataTibble, by  = c("label" = "nameOnPhylogeny"))
      # treeObj <- as.treedata(y)
      # 
###########
     # p <- ggtree(treeObj,aes(colour=InSeedCollection),layout='circular') + geom_tiplab2(hjust = -.1,align=T)
      #p <- p +geom_tippoint(aes(x=x+13), size=dim(dat)[1]^(1/4),na.rm=T,colour="purple")
      #p <- gheatmapKT(p, CastCountObj_mat, color="black",low=c("yellow"), high = c("purple"),width=1,offset = 2, font.size=3,colnames_position="top")
      #p <- p + annotate("text",x=22,size=2.5,y=length(inputData$Count)+1.5,label=input$PollinatorInputPhy2)
      #p <- p + guides(fill=guide_legend(title="Proportion of Pollinator Observations"))
      #p <- p + theme(legend.title.align=0.5)

      return(p)
    }
,
    height = 1000,
    width = 800

  )    
  # 
  # observeEvent(input$plottree_dblclick, {
  #   brush <- input$plottree_brush
  #   if (!is.null(brush)) {
  #     ranges$x <- c(brush$xmin, brush$xmax)
  #     ranges$y <- c(brush$ymin, brush$ymax)
  #     
  #   } else {
  #     ranges$x <- NULL
  #     ranges$y <- NULL
  #   }
  # })
  
  

# Table of Species in Collections Output
  output$FilteredTable <- renderDataTable(
    as.data.table(SppTableFilter())[,.(nameOnPhylogeny,CRPR, evolDist,InSeedCollection, InLivingCollection)]
  )

# Text of the Number of Obserations in a Filter
  output$FilterText <- renderText(
    length(Filters())
  )
  
  
  observeEvent(input$calculateSignal, {
    
# Create reactive model of seed phylogenetic signal
   SeedPhyloModel <- reactive({
      phylo.d(TreeFilter(),SppTableFilter(), names.col=nameOnPhylogeny, binvar=InSeedCollection, permut = 100, rnd.bias=NULL)
   })

# Writing the phylogenetic signal statistic
  output$PhyloDSummarySeed <- renderText(
    paste("Phylogenetic Signal in Seed Collections:", round(SeedPhyloModel()$DEstimate,digits=2)))
})
  
  observeEvent(input$calculateLivingSignal, {
    
    # Create reactive model of seed phylogenetic signal
   LivingPhyloModel <- reactive({
      phylo.d(TreeFilter(),SppTableFilter(), names.col=nameOnPhylogeny, binvar=InLivingCollection, permut = 100, rnd.bias=NULL)
    })
    
    # Writing the phylogenetic signal statistic
    output$PhyloDSummaryLiving <- renderText(
      paste("Phylogenetic Signal in Living Collections:", round(LivingPhyloModel()$DEstimate,digits=2), "P Value (Brownian motion):", ))
  })
  
  
  
  FiltersVenn <- reactive({
    if(is.null(input$rareRank)){
      rareFilter <- seq(1,length(caprSppTable$taxonID))
    }else{
      rareFilter <- which(caprSppTable$CRPR_simple%in%input$rareRank)
    }
    return(rareFilter)
  })
    
    SppVennFilter <- reactive({
      data<-as.data.frame(caprSppTable)[FiltersVenn(),]
      data <- as.data.table(data)
      return(data) 
    })  
    
 # Table of Species in Venn Diagram
    output$VennTable <- renderDataTable(
      SppVennFilter()
    )
    
    
# Venn diagram of species collection type
  output$VennDiagram <- renderPlot(  {
    VenDat <- SppVennFilter()[,.(Count=.N),by="collectionTypes"]
    VenDat[,collectionTypes:=gsub(", ","&",collectionTypes,fixed=T)] 
    VenVec <- VenDat$Count
    names(VenVec) <-VenDat$collectionTypes
    p <- plot(euler(VenVec),quantities=TRUE)
    
    return(p)}
    ,
    height=1000,
    width=800
  )
  

  
  }

