## maps of IDW interpolated abundance

figure21.fct <- function(dat.in, cex.in, pos.ylabel=c(0,0)) {
# dat.in <- read.csv(file.path(path.ATLAS, "/Data/SS2526_catch.csv"), header=TRUE)

logic.abundant <- quantile(subset(dat.in, totno.corr != 0)$totno.corr, probs=c(0.95))>50

my.levels <- if(logic.abundant) c(0,0.1,5,20,50,100,500) else c(0,0.05,0.1,0.5,1,5,10)
my.legend <- ifelse(logic.abundant, "abundant","rare")

my.xlim <- c(291.25,303.5)
my.ylim <- c(41,47.5)

xx.lon<-pretty(my.xlim,n=5)
yy.lat<-pretty(my.ylim, n=4)

mat.layout2 <- matrix(c(0,2,0,0,1,3,5,0,0,4,6,8,0,0,7,0),nrow=4, ncol=4)
ll <- layout(mat.layout2, widths=3*c(2,6.5,6.5,2), heights=3*c(2,4.8,4.8,2), respect=TRUE)


yrs.labels <- c("1999-2002","2003-2006","2007-2010","2011-2013")

my.cols.palette <- c('white','#FEF0D9', '#FDCC8A', '#FC8D59', '#E34A33', '#B30000')

## layout and such, to allow for the axes
# top left longitude axis
par(mar=c(0,0,3,0), las=1)
plot(xx.lon,rep(max(yy.lat), length(xx.lon)),axes=FALSE, type='n', xlab="",ylab="")
axis(side=3, at=xx.lon, line=-5.5, labels=paste(360-xx.lon,"\u{B0}W",sep=""))


# top left latitude
par(mar=c(0,3,0,0),las=2)
plot(rep(min(xx.lon),length(yy.lat)),yy.lat,axes=FALSE,type='n')
axis(side=2,at=yy.lat, line=-5.5, labels=paste(yy.lat,"\u{B0}N",sep=""))


## loop over -year periods
for (i in 1:4) {
yy <- strsplit(yrs.labels[i],"-")
tt <- subset(dat.in, YEAR >=as.numeric(yy[[1]][1]) & YEAR <= as.numeric(yy[[1]][2]) )

tt$occ <- ifelse(tt$totno.corr==0,0,1)
pr.occ <- round(mean(tt$occ), digits=3)

my.df <- data.frame(x=tt$lon+360, y=tt$lat, z=tt$totno.corr)
#my.df <- data.frame(x=tt$lon+360, y=tt$lat, z=tt$totwgt.corr)

# define grid for IDW interpolation
g.len <- 200
D <- data.frame(	x=rep(seq(my.xlim[1],my.xlim[2],length.out=g.len), time=g.len),
					y=rep(seq(my.ylim[1],my.ylim[2],length.out=g.len), each=g.len),
					z=rep(0, g.len*g.len))
DD <- data.frame(	x=seq(my.xlim[1],my.xlim[2],length.out=g.len),
					y=seq(my.ylim[1],my.ylim[2],length.out=g.len),
					z=matrix(0, g.len, g.len))

# set coordinates system
coordinates(my.df)=~x+y
coordinates(D)=~x+y

# IDW
my.power<-10
totno.idw <- idw(z~1, my.df, D, idp=my.power)

# reset interpolation grid
D <- data.frame(	x=rep(seq(my.xlim[1],my.xlim[2],length.out=g.len), time=g.len),
					y=rep(seq(my.ylim[1],my.ylim[2],length.out=g.len), each=g.len),
					z=rep(0, g.len*g.len))

W <- owin(my.xlim, my.ylim)
D <- as.ppp(D, W=W)
D <- as(D, "SpatialPoints")

## remove points that are outside the mask
M=PolySet2SpatialPolygons(SS.strata.mask.LL)

PtsNotInMask <-  which(!is.na(overlay(D, M)))

totno.idw$var1.pred[-PtsNotInMask]=0
DD$z=matrix(as.vector(totno.idw$var1.pred), ncol=g.len, nrow=g.len)


C <- contourLines(	x=seq(my.xlim[1],my.xlim[2], length.out=g.len), 
					y=seq(my.ylim[1],my.ylim[2], length.out=g.len),
					z=matrix(DD$z,ncol=g.len, nrow=g.len), 
					levels= my.levels) #c(0,0.1,5,20,50,100,500)) 

res= convCP(C, projection = "LL")
res=PolySet2SpatialPolygons(res$PolySet)
res <- lapply(getSpPpolygonsSlot(res), checkPolygonsHoles)

R <- as.SpatialPolygons.PolygonsList(res)
	
plotMap(worldLLhigh, my.xlim, my.ylim, col=grey(0.9),plt=c(0.0,1.0,0.0,1.0),border=grey(0.7),axes=FALSE,tckLab=FALSE,xlab="",ylab="")
text(293.2,46.4,yrs.labels[i],bg='white',cex=0.95)
text(293.2,45.9,paste("P(occ) = ",pr.occ,sep=""),cex=0.75)
#addPolys(SUMMER.strata.mask)

plot(R, border=my.cols.palette, col = my.cols.palette, add=TRUE, axes=FALSE)
#points(360+tt$lon,tt$lat,pch=3,cex=0.05)
box()
#addPolys(worldLLhigh)
addPolys(SS.strata.mask.LL)
#plotMap(worldLLhigh, my.xlim, my.ylim, col=grey(0.9),plt=c(0.0,1.0,0.0,1.0),border=grey(0.7),axes=FALSE,tckLab=FALSE,xlab="",ylab="")

#addPolys(combinePolys(fixBound(SUMMER.strata.mask,tol=0.01)))

if(i==1 | i==4) {
	if(my.legend=="abundant"){
							text(299.5,42,"n/tow",cex=0.75)
							text(299.45,41.6,"n/trait",cex=0.75)
							legend('bottomright', c("0","<5","<20","<50","<100",">=100"), col='black', fill=my.cols.palette, bg='white',cex=0.95)
							}
	if(my.legend=="rare"){
							text(299.5,42,"n/tow",cex=0.75)
							text(299.45,41.6,"n/trait",cex=0.75)
							legend('bottomright', c("0","<0.1","<0.5","<1","<5",">=5"), col='black', fill=my.cols.palette, bg='white',cex=0.95)
						}
	}

} # end loop over 3-year periods

# bottom right latitude
par(mar=c(0,0,0,2),las=2)
plot(rep(max(xx.lon),length(yy.lat)),yy.lat,axes=FALSE,type='n')
axis(side=4,at=yy.lat, line=-6.5, labels=paste(yy.lat,"\u{B0}N",sep=""))
# bottom right longitude axis
par(mar=c(2,0,0,0),las=1)
plot(xx.lon,rep(min(yy.lat), length(xx.lon)),axes=FALSE, type='n', xlab="",ylab="")
axis(side=1, at=xx.lon, line=-6.5, labels=paste(360-xx.lon,"\u{B0}O",sep=""))

} # end function
