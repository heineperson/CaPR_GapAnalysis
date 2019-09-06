library(ggmap)
library(data.table)
library(raster)
library(rgdal)

source("caprPrioritizationApp/Tokens/GoogleAPI.R")

# Bringing in cnddb occurrences
targetcnddb <- fread("caprPrioritizationApp/AppData/OccurrencesCnddbCaPRFiltered.csv")

# Reading in cnddb to make 
cnddb_shape <- readOGR(dsn="Data/CNDDBLayer",layer="cnddb")
cnddb_shape<- spTransform(cnddb_shape,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
points_dat <- lapply(1:length(cnddb_shape@polygons), 
                     function(x) as.data.table(t(cnddb_shape@polygons[[x]]@labpt)))
points_dat <- rbindlist(points_dat, fill=TRUE)
points_dat <- cbind( as.data.table(cnddb_shape@data),points_dat)
points_dat<- points_dat[TAXONGROUP%in%c("Dicots","Monocots","Ferns","Bryophytes","Gymnosperms","Herbaceous")]
#coordinates(points_dat) <-  ~V1 + V2

# Finding targets
points_dat_CNF <- subset(points_dat,(grepl("USFS-CLEVELAND",OWNERMGT) & SNAME%in%unique(targetcnddb$SNAME)))


merge(targetcnddb,cnddbAll[,.(SNAME,OCCNUMBER,)])

# Brinding in federal lands data
 federal_lands <- readOGR(dsn="Data/FederalLands",layer="Federal_Lands_California")

# Subsetting to just Cleveland National Forest
federal_lands_no19NA <- federal_lands[!is.na(federal_lands@data$ilmcaPub19),]
clevelandNF <- federal_lands_no19NA[federal_lands_no19NA@data$ilmcaPub19=="Cleveland National Forest",]

# Plot 
clevelandNF <- fortify(clevelandNF)

# Making up species stuff
points_dat_CNF[SNAME%in%unique(points_dat_CNF$SNAME)[1:4],pch:=1]
points_dat_CNF[SNAME%in%unique(points_dat_CNF$SNAME)[5:8],pch:=2]
points_dat_CNF[SNAME%in%unique(points_dat_CNF$SNAME)[9:12],pch:=3]
points_dat_CNF[SNAME%in%unique(points_dat_CNF$SNAME)[13:16],pch:=4]
points_dat_CNF[SNAME%in%unique(points_dat_CNF$SNAME)[c(1,5,9,13)],col:="red"]
points_dat_CNF[SNAME%in%unique(points_dat_CNF$SNAME)[c(2,6,10,14)],col:="blue"]
points_dat_CNF[SNAME%in%unique(points_dat_CNF$SNAME)[c(3,7,11,15)],col:="yellow"]
points_dat_CNF[SNAME%in%unique(points_dat_CNF$SNAME)[c(4,8,12,16)],col:="orange"]


map <- get_map(location = "escondido, ca",
               zoom = 9, maptype="terrain",source="google")
p <- ggmap(map) + geom_polygon(aes(x=long, y=lat, group=group), fill='purple', size=0.5,color='black', data=clevelandNF, alpha=0.5)
p <- p + geom_point(data=points_dat_CNF, aes(x=V1,y=V2,colour=SNAME,linetype=as.character(pch)))
#p <- p + scale_linetype_manual("", values=as.character(points_dat_CNF$pch)) 
p


mapNorth <- get_map(location = c(-117.5,33.65),
               zoom = 10, maptype="terrain",source="google")
p <- ggmap(mapNorth) + geom_polygon(aes(x=long, y=lat, group=group), fill='purple', size=0.5,color='black', data=clevelandNF, alpha=0.5)
p <- p + geom_point(data=points_dat_CNF[V2>33.4 & OWNERMGT=="USFS-CLEVELAND NF"], aes(x=V1,y=V2,fill=SNAME),colour="black",pch=21,size=3)
p <-p +  xlim(c(-117.75,-117.25)) + xlab("")
p <-p +  ylim(c(33.4,33.85)) + ylab("") + ggtitle("Trabuco Ranger District")
p <- p + guides(fill=guide_legend(title="Target Species"))
p
ggsave("Figures/Trabuco.png",width=8,height=5)



mapMiddle <- get_map(location = c(-116.8,33.35),
                    zoom = 10, maptype="terrain",source="google")
p <- ggmap(mapMiddle) + geom_polygon(aes(x=long, y=lat, group=group), fill='purple', size=0.5,color='black', data=clevelandNF, alpha=0.5)
p <- p + geom_point(data=points_dat_CNF[SNAME%in%c("Phacelia keckii","Symphyotrichum defoliatum") & OWNERMGT=="USFS-CLEVELAND NF"], aes(x=V1,y=V2,fill=SNAME),colour="black",pch=21,size=3)
p <-p +  xlim(c(-117.2,-116.5)) + xlab("")
p <-p +  ylim(c(33.1,33.5)) + ylab("") + ggtitle("Palomar Ranger District")
p <- p + guides(fill=guide_legend(title="Target Species"))
p
ggsave("Figures/Palomar.png",width=8,height=5)



mapSouth <- get_map(location = c(-116.6,32.75),
                     zoom = 10, maptype="terrain",source="google")
p <- ggmap(mapSouth) + geom_polygon(aes(x=long, y=lat, group=group), fill='purple', size=0.5,color='black', data=clevelandNF, alpha=0.5)
p <- p + geom_point(data=points_dat_CNF[V2< 33.2 & OWNERMGT=="USFS-CLEVELAND NF"], aes(x=V1,y=V2,fill=SNAME),colour="black",pch=21,size=3)
p <-p +  xlim(c(-116.9,-116.3)) + xlab("")
p <-p +  ylim(c(32.65,33.1)) + ylab("") + ggtitle("Descanso Ranger District")
p <- p + guides(fill=guide_legend(title="Target Species"))
p
ggsave("Figures/Descanso.png",width=8,height=5)


