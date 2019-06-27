server <- function(input, output) {
  
# Creating Tree for Plotting that Filters by Family  
  FamTree <- reactive({
    pruned.tree<-drop.tip(MatchDataObj$phy,MatchDataObj$phy$tip.label[-which(grepl(input$fam,as.character(MatchDataObj$data$family)))])
    return(pruned.tree) 
  })  
  
# Creating Table that Filters by Family  
   FamTable <- reactive({
     pruned.data<-subset(MatchDataObj$data, family==input$fam)
     return(pruned.data) 
   })
  
  
# Plotting Tree
  output$PlotGGTree <- renderPlot(
    # Expression
    {
      phy2 <- FamTree()
      
      p <- ggtree(phy2,layout='rectangular') + geom_tiplab( size=2, color="black")
      #p <- p +geom_tippoint(aes(x=x+13), size=inputData$Count^(1/4),na.rm=T,colour="purple")
      #p <- gheatmapKT(p, CastCountObj_mat, color="black",low=c("yellow"), high = c("purple"),width=1,offset = 2, font.size=3,colnames_position="top") 
      #p <- p + annotate("text",x=22,size=2.5,y=length(inputData$Count)+1.5,label=input$PollinatorInputPhy2)
      #p <- p + guides(fill=guide_legend(title="Proportion of Pollinator Observations"))
      p <- p + theme(legend.title.align=0.5)
      return(p)
    },
    height = 1000,
    width = 600
  )    

# Table output
  output$FilteredTable <- renderDataTable(
    FamTable()
    # {
    # table <- FamTable()
    # return(table)}
    )
    
}

