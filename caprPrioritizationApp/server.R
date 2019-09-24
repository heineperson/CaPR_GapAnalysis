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
      phylo.d(TreeFilter(),SppTableFilter()[,c(1,13,14)], names.col=nameOnPhylogeny, binvar=InSeedCollection, permut = 100, rnd.bias=NULL)
   })

# Writing the phylogenetic signal statistic
  output$PhyloDSummarySeed <- renderText(
    paste("Phylogenetic Signal in Seed Collections:", round(SeedPhyloModel()$DEstimate,digits=2),"P Value (Brownian motion):",round(SeedPhyloModel()$Pval1,digits=2) ))
})
  
  observeEvent(input$calculateLivingSignal, {
    
    # Create reactive model of seed phylogenetic signal
   LivingPhyloModel <- reactive({
      phylo.d(TreeFilter(),SppTableFilter()[,c(1,13,14)], names.col=nameOnPhylogeny, binvar=InLivingCollection, permut = 100, rnd.bias=NULL)
    })
    
    # Writing the phylogenetic signal statistic
    output$PhyloDSummaryLiving <- renderText(
      paste("Phylogenetic Signal in Living Collections:", round(LivingPhyloModel()$DEstimate,digits=2), "P Value (Brownian motion):",round(LivingPhyloModel()$Pval1,digits=2) ))
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
    
# Collection Progress Barchar
    
    output$ProgressPlot <- renderPlot({
      collectionTable <- caprSppTable[,.(Count=.N),by=c("topCollectionTypes","CRPR_simple")][order(CRPR_simple,topCollectionTypes)]
      collectionTable[is.na(CRPR_simple),CRPR_simple:="Unranked"]
      collectionTable$topCollectionTypes <- factor(collectionTable$topCollectionTypes, levels = c("06-Not Collected","05-Living Unknown", "04-Living Wild", "03-Seed Data Deficient", "02-Seed Bulked", "01-Maternal Lines And Wild"))
      
      p <- ggplot(data=collectionTable[CRPR_simple!="Unranked"],aes(x=CRPR_simple,y=Count,fill=topCollectionTypes))+geom_bar(stat="identity",colour="black")
      p <- p + theme_bw()
      p <- p +   scale_fill_manual(values=c("grey", "orchid2", "orchid4", "palegreen1", "palegreen3","palegreen4")) 
      p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      p <- p +  theme(axis.text.x=element_text(size=14,face="bold",angle=25,hjust=1,colour="black"),axis.text.y=element_text(size=14),
                      axis.title=element_text(size=16,face="bold"),  plot.title = element_text(hjust = 0.5,size=18))
      p <- p + ylab("# of Species")+ylim(0,1300)
      p <- p + xlab("CNPS Rank")
      p <- p + ggtitle("Progress Toward Our Goal")
      return(p) 
    }
     )
    
  
 # Table of Species in Bar Chart
    output$VennTable <- renderDataTable(
      collectionTable <- caprSppTable[,.(Count=.N),by=c("topCollectionTypes","CRPR_simple")][order(CRPR_simple,topCollectionTypes)]
      
    )
    
  # Gold star species that meet the CPC guideliens
    output$GoldStar <- renderDataTable(
      caprSppTable[SppRank=="MeetsCPCGoal",.(name_minus_authors,CRPR,matLinesSeedCollections,aggregateSeedCount,aggregateMaternalLineCount)]
    )    

# Data deficient accessions
    output$dataDeficient <- renderDataTable(
     capr[conservationClassification=="Seed: Data Deficient",.(scientificNameOriginal,institutionCode,tgermplasmIdentifier,biologicalStatus,preparations, decimalLatitude,cnddbEOIndex)]
    )    
    
# Venn diagram of species collection type
  output$VennDiagram <- renderPlot(  {
    VenDat <- SppVennFilter()[collectionTypesSeed!="Uncollected",.(Count=.N),by="collectionTypesSeed"]
    VenDat[,collectionTypesSeed:=gsub(", ","&",collectionTypesSeed,fixed=T)] 
    VenVec <- VenDat$Count
    names(VenVec) <-VenDat$collectionTypesSeed
    p <- plot(euler(VenVec),quantities=TRUE,main="Distribution of Species Among Seed Collection Types")
    
    return(p)}
  )
  
#############  
### Prioritization Tab
##########

  # Reactive Filters Priotiziation

# Writing a function that gives list for each filter
filterFunc <- function(InPuT,Data,column){
  if((is.null(InPuT) )){
    filterOut <- seq(1,dim(Data)[1])
  }else{
    filterOut <- which(Data[,get(column)]%in%InPuT)
  }
  return(filterOut)
}
    
  # Defining Reactive Filters
  FiltersOccPrior <- reactive({
    
    landOwnBroad <- filterFunc(input$landownBroad,EOdata, "BroadestOwnership")
    landOwnForest <- filterFunc(input$nationalForest,EOdata, "forest")
    county <- filterFunc(input$countySample,EOdata, "KEYCOUNTY")
    ecoRegion <- filterFunc(input$ecoRegion,EOdata, "JEP_REG")
    specificLands <- filterFunc(input$specificLands,EOdata, "OWNERMGT")
    collectionSpp <- filterFunc(input$collectionSpp,EOdata, "SppYesNo")
    crpr <- filterFunc(input$rarityRank,EOdata, "RPLANTRANK")
    
    filters <- Reduce(intersect, list(landOwnBroad,landOwnForest,county,ecoRegion,specificLands,collectionSpp,crpr))
    return(filters)
  })
  

  # Occurrence table with reactive filters & location
  reactiveOccDT <- reactive({
    if(is.null(input$yourLocation)|input$yourLocation=="" | is.null(input$milesFrom)|input$milesFrom==""){
      dat <- EOdata[,.(SNAME, CRPR=RPLANTRANK, EO=OCCNUMBER, COUNTY=KEYCOUNTY, LOCATION,LOCDETAILS,`Owner(GIS)`=BroadestOwnership,`Owner(cnddb)`=OWNERMGT,`EO Collected`=CollectedYesNo,`Spp Collected`=SppYesNo)][FiltersOccPrior()][order(SNAME)]
    }else{
      loc <- geocode(input$yourLocation)
      dat <- EOdata[,.(SNAME, CRPR=RPLANTRANK, EO=OCCNUMBER, COUNTY=KEYCOUNTY, LOCATION,LOCDETAILS,`Owner(GIS)`=BroadestOwnership,`Owner(cnddb)`=OWNERMGT,`EO Collected`=CollectedYesNo,`Spp Collected`=SppYesNo,
                       milesAway=round(distHaversine(matrix(c(decimalLongPolyCent, decimalLatPolyCent), ncol = 2),
                                               matrix(c(loc[[1]], loc[[2]]), ncol = 2))/6000))][FiltersOccPrior()]
      dat <- dat[milesAway <= as.numeric(input$milesFrom)][order(milesAway)]
    }
    return(dat)
  })
  
  # Species table with reactive filters & location
  reactiveSppDT <- reactive({
    if(is.null(input$yourLocation)|input$yourLocation=="" | is.null(input$milesFrom)|input$milesFrom==""){
      #dat <- EOdata[,.(SNAME, CRPR=RPLANTRANK, EO=OCCNUMBER, COUNTY=KEYCOUNTY, LOCATION,LOCDETAILS,`Owner(GIS)`=BroadestOwnership,`Owner(cnddb)`=OWNERMGT,`EO Collected`=CollectedYesNo,`Spp Collected`=SppYesNo)][FiltersOccPrior()][order(SNAME)]
      datlim <- EOdata[][FiltersOccPrior()]
      sppdat <- datlim[,.(CRPR=RPLANTRANK[1],FEDLIST=FEDLIST[1],evolDist=round(evolDist[1]),`Collected Anywhere`=SppYesNo[1],`Collections in Filter`=sum(1*CollectedYN==1), milesAway=NA),by=SNAME][order(SNAME)]
    }else{
      loc <- geocode(input$yourLocation)
      datlim <- EOdata[][FiltersOccPrior()]
      sppdat <- datlim[,.(CRPR=RPLANTRANK[1],FEDLIST=FEDLIST[1],evolDist=round(evolDist[1]),`Collected Anywhere`=SppYesNo[1],`Collections in Filter`=sum(1*CollectedYN==1),
                       milesAway=min(round(distHaversine(matrix(c(decimalLongPolyCent, decimalLatPolyCent), ncol = 2),
                                                     matrix(c(loc[[1]], loc[[2]]), ncol = 2))/6000,1),na.rm=T)),by=SNAME]
      sppdat <- sppdat[milesAway <= as.numeric(input$milesFrom)][order(milesAway)]
    }
    
    # Prioritity Ranks
    sppdat[CRPR%in%c("1B.1","1A"),RareScore:=100]
    sppdat[CRPR=="1B.2",RareScore:=95]
    sppdat[CRPR=="1B.3",RareScore:=90]
    sppdat[CRPR%in%c("2A","2B.1"),RareScore:=75]
    sppdat[CRPR%in%c("2B.2"),RareScore:=65]
    sppdat[CRPR%in%c("2B.3"),RareScore:=60]
    sppdat[CRPR%in%c("3","3.1"),RareScore:=50]
    sppdat[CRPR%in%c("3.2"),RareScore:=45]
    sppdat[CRPR%in%c("3.3"),RareScore:=40]
    sppdat[CRPR%in%c("4.1"),RareScore:=25]
    sppdat[CRPR%in%c("4.2"),RareScore:=20]
    sppdat[CRPR%in%c("4.3"),RareScore:=15]
    sppdat[is.na(RareScore),RareScore:=0]
    sppdat[,RareScore:=RareScore/100]

    # Collection Status
    sppdat[,CollectScore:=ifelse(`Collected Anywhere`=="Yes",0,1)]

    # Nearness
    sppdat[,DistScore:=ifelse(is.na(milesAway),0,1-milesAway/max(milesAway,na.rm=T))]

    # Evolutionary Dist
    sppdat[is.na(evolDist)|evolDist=="",evolDist:=1]
    sppdat[,EvolScore:=evolDist/max(evolDist,na.rm=T)]

    # TotalScore
    print(input$rarityRank1)
    sppdat[,RawScore:=RareScore+DistScore+EvolScore+CollectScore]
    sppdat[,AdjScore:=RareScore*(5-(input$rarityRank1))+EvolScore*(5-input$evoRank)+CollectScore*(5-input$collectionRank)+DistScore*(5-input$locationRank)]
    sppdat1=sppdat[,.(SNAME,CRPR,evolDist,`Collected Anywhere`,milesAway,RareScore,CollectScore,DistScore,EvolScore,AdjScore)][order(-AdjScore,milesAway,SNAME)]

    return(sppdat1)
  })
  
  
  
   # output$occurrencesPriority <- renderDataTable(
   #   reactiveOccDT()
   # )
  output$occurrencesPriority <- renderDataTable(

    datatable(
      cbind(' ' = '&oplus;',     as.data.frame(reactiveOccDT())),
      escape = -2,
      options = list(
        columnDefs = list(
          list(visible = FALSE, targets = c(5,6,7)),
          list(orderable = FALSE, className = 'details-control', targets = 1)
        )
      ),
      callback = JS("
  table.column(1).nodes().to$().css({cursor: 'pointer'});
  var format = function(d) {
    return '<div style=\"background-color:#eee; padding: .5em;\"> Location: ' +
            d[6] + ', More Details: ' + d[7] + ', County: ' + d[5] + '</div>';
  };
  table.on('click', 'td.details-control', function() {
    var td = $(this), row = table.row(td.closest('tr'));
    if (row.child.isShown()) {
      row.child.hide();
      td.html('&oplus;');
    } else {
      row.child(format(row.data())).show();
      td.html('&CircleMinus;');
    }
  });"
      ))
      )
  
  output$sppTablePriority <- DT::renderDataTable(
    DT::datatable(reactiveSppDT(),options=list(pageLength = 25))
  )
  
  output$downloadOcc <- downloadHandler(
    filename = function(){"OccurrencesCnddbCaPRFiltered.csv"}, 
    content = function(fname){
      write.csv(reactiveOccDT(), fname)
    }
  )
  
  output$downloadSpp <- downloadHandler(
    filename = function(){"SppCnddbCaPRFiltered.csv"}, 
    content = function(fname){
      write.csv(    reactiveSppDT(), fname)
    }
  )
  
  
  
  }

