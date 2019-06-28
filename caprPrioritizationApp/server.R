server <- function(input, output) {
  
# Defining Reactive Filters
  Filters <- reactive({
    
    # Filter table
    #FilterTable <- as.data.table(MatchDataObj$data)

     #famFilter <- which(MatchDataObj$data$family%in%input$famAuto)
    
    if(is.null(input$famAuto)){
      famFilter <- seq(1,length(MatchDataObj$data$family))
    }else{
      famFilter <- which(MatchDataObj$data$family%in%input$famAuto)
    }
    
    if(is.null(input$countyAuto)){
      countyFilter <- seq(1,length(MatchDataObj$data$family))
      }else{
     countyFilter <- which(grepl(input$countyAuto,as.character(MatchDataObj$data$Counties)))
     }

    if(is.null(input$instInput)){
      instFilter <- seq(1,length(MatchDataObj$data$institutions))
    }else{
      instFilter <- which(grepl(input$instInput,as.character(MatchDataObj$data$institutions)))
    }
    
    
    filters <- Reduce(intersect, list(famFilter,countyFilter,instFilter))
    #filters <- Reduce(intersect, list(famFilter,instFilter))
    #filters <- famFilter
    #filters <- instFilter
    
     return(filters)
  })
  
# Creating Tree for Plotting that Filters by Family  
  TreeFilter <- reactive({
    pruned.tree<-drop.tip(MatchDataObj$phy,MatchDataObj$phy$tip.label[-Filters()])
    return(pruned.tree)
  })  
  
# Creating Table that Filters by Family  
   SppTableFilter <- reactive({
     pruned.data<-MatchDataObj$data[Filters(),]
     return(pruned.data) 
   })
  
  
# Plotting Tree
  output$PlotGGTree <- renderPlot(
    # Expression
    {
      phy2 <- TreeFilter()
      dat <- MatchDataObj$data[MatchDataObj$data$nameOnPhylogeny%in%phy2$tip.label,]
      
      p <- trait.plot(collapse.singles(phy2), dat, cols = list(SeedCollYN = c("pink", "red"), 
                                                                   AnyCollYN = c("lightblue", "blue")),cex.lab=0.3)
      
      
      
      #p <- ggtree(phy2,layout='rectangular') + geom_tiplab( size=3, color="black")
      #p <- p +geom_tippoint(aes(x=x+13), size=inputData$Count^(1/4),na.rm=T,colour="purple")
      #p <- gheatmapKT(p, CastCountObj_mat, color="black",low=c("yellow"), high = c("purple"),width=1,offset = 2, font.size=3,colnames_position="top") 
      #p <- p + annotate("text",x=22,size=2.5,y=length(inputData$Count)+1.5,label=input$PollinatorInputPhy2)
      #p <- p + guides(fill=guide_legend(title="Proportion of Pollinator Observations"))
      #p <- p + theme(legend.title.align=0.5)
      return(p)
    },
    height = 1000,
    width = 800
  )    

# Table output
  output$FilteredTable <- renderDataTable(
    SppTableFilter()
  )

# Text Output
  output$FilterText <- renderText(
    length(Filters())
  )
  
  }

