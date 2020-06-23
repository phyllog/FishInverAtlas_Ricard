## plot strata boundaries from DFO SUMMER survey
## shapefile obtained from Jamie Emberley at SABS
## Daniel Ricard, 2012-07-30

require(PBSmapping)
data(worldLLhigh)

SS.strata <- importShapefile("C:/ATLAS_poissons_SS/Mapping/ScotianShelfStrataUTM83F.shp", readDBF=TRUE, projection="UTM", zone=19)

SS.strata.LL <- convUL(SS.strata, km=FALSE)
SS.strata.LL$X <- SS.strata.LL$X + 360

pdf("SUMMER-strata-SABS.pdf", width=11, height=11/1.6)
plotMap(worldLLhigh, xlim=c(291.5,303.5), ylim=c(41.5,48), col=grey(0.9),plt=c(0.1,0.9,0.1,0.9),border=grey(0.5),axes=FALSE,tckLab=FALSE,xlab="",ylab="")
box()
addPolys(SS.strata.LL)
dev.off()
