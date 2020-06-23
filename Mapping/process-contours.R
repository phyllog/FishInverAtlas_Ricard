##
## 
con.0 <- read.table("contour_0_0.xyz", sep="\t")
names(con.0) <- c("X","Y","Z")
con.1 <- read.table("contour_0_1.xyz", sep="\t")
names(con.1) <- c("X","Y","Z")
con.2 <- read.table("contour_0_2.xyz", sep="\t")
names(con.2) <- c("X","Y","Z")
con.3 <- read.table("contour_0_3.xyz", sep="\t")
names(con.3) <- c("X","Y","Z")
con.4 <- read.table("contour_0_4.xyz", sep="\t")
names(con.4) <- c("X","Y","Z")
con.5 <- read.table("contour_0_5.xyz", sep="\t")
names(con.5) <- c("X","Y","Z")
con.6i <- read.table("contour_0_6_i.xyz", sep="\t")
names(con.6i) <- c("X","Y","Z")
con.7i <- read.table("contour_0_7_i.xyz", sep="\t")
names(con.7i) <- c("X","Y","Z")
con.8 <- read.table("contour_0_8.xyz", sep="\t")
names(con.8) <- c("X","Y","Z")
con.9 <- read.table("contour_0_9.xyz", sep="\t")
names(con.9) <- c("X","Y","Z")
con.10i <- read.table("contour_0_10_i.xyz", sep="\t")
names(con.10i) <- c("X","Y","Z")
con.11i <- read.table("contour_0_11_i.xyz", sep="\t")
names(con.11i) <- c("X","Y","Z")
con.12i <- read.table("contour_0_12_i.xyz", sep="\t")
names(con.12i) <- c("X","Y","Z")
con.13i <- read.table("contour_0_13_i.xyz", sep="\t")
names(con.13i) <- c("X","Y","Z")
con.14 <- read.table("contour_0_14.xyz", sep="\t")
names(con.14) <- c("X","Y","Z")
con.15 <- read.table("contour_0_15.xyz", sep="\t")
names(con.15) <- c("X","Y","Z")
con.16 <- read.table("contour_0_16.xyz", sep="\t")
names(con.16) <- c("X","Y","Z")
con.17 <- read.table("contour_0_17.xyz", sep="\t")
names(con.17) <- c("X","Y","Z")
con.18 <- read.table("contour_0_18.xyz", sep="\t")
names(con.18) <- c("X","Y","Z")
con.19 <- read.table("contour_0_19.xyz", sep="\t")
names(con.19) <- c("X","Y","Z")
con.20 <- read.table("contour_0_20.xyz", sep="\t")
names(con.20) <- c("X","Y","Z")
con.21i <- read.table("contour_0_21_i.xyz", sep="\t")
names(con.21i) <- c("X","Y","Z")
con.22i <- read.table("contour_0_22_i.xyz", sep="\t")
names(con.22i) <- c("X","Y","Z")
con.23i <- read.table("contour_0_23_i.xyz", sep="\t")
names(con.23i) <- c("X","Y","Z")
con.24i <- read.table("contour_0_24_i.xyz", sep="\t")
names(con.24i) <- c("X","Y","Z")
con.25i <- read.table("contour_0_25_i.xyz", sep="\t")
names(con.25i) <- c("X","Y","Z")
con.26 <- read.table("contour_0_26.xyz", sep="\t")
names(con.26) <- c("X","Y","Z")
con.27i <- read.table("contour_0_27_i.xyz", sep="\t")
names(con.27i) <- c("X","Y","Z")
con.28i <- read.table("contour_0_28_i.xyz", sep="\t")
names(con.28i) <- c("X","Y","Z")
con.29i <- read.table("contour_0_29_i.xyz", sep="\t")
names(con.29i) <- c("X","Y","Z")
con.30i <- read.table("contour_0_30_i.xyz", sep="\t")
names(con.30i) <- c("X","Y","Z")
con.31i <- read.table("contour_0_31_i.xyz", sep="\t")
names(con.31i) <- c("X","Y","Z")


con.0$PID <- 1
con.0$POS <- seq(1,length(con.0$X))
con.1$PID <- 2
con.1$POS <- seq(1,length(con.1$X))
con.2$PID <- 3
con.2$POS <- seq(1,length(con.2$X))
con.3$PID <- 4
con.3$POS <- seq(1,length(con.3$X))
con.4$PID <- 5
con.4$POS <- seq(1,length(con.4$X))
con.5$PID <- 6
con.5$POS <- seq(1,length(con.5$X))
con.6i$PID <- 7
con.6i$POS <- seq(1,length(con.6i$X))
con.7i$PID <- 8
con.7i$POS <- seq(1,length(con.7i$X))
con.8$PID <- 9
con.8$POS <- seq(1,length(con.8$X))
con.9$PID <- 10
con.9$POS <- seq(1,length(con.9$X))
con.10i$PID <- 11
con.10i$POS <- seq(1,length(con.10i$X))
con.11i$PID <- 12
con.11i$POS <- seq(1,length(con.11i$X))
con.12i$PID <- 13
con.12i$POS <- seq(1,length(con.12i$X))
con.13i$PID <- 14
con.13i$POS <- seq(1,length(con.13i$X))
con.14$PID <- 15
con.14$POS <- seq(1,length(con.14$X))
con.15$PID <- 16
con.15$POS <- seq(1,length(con.15$X))
con.16$PID <- 17
con.16$POS <- seq(1,length(con.16$X))
con.17$PID <- 18
con.17$POS <- seq(1,length(con.17$X))
con.18$PID <- 19
con.18$POS <- seq(1,length(con.18$X))
con.19$PID <- 20
con.19$POS <- seq(1,length(con.19$X))
con.20$PID <- 21
con.20$POS <- seq(1,length(con.20$X))
con.21i$PID <- 22
con.21i$POS <- seq(1,length(con.21i$X))
con.22i$PID <- 23
con.22i$POS <- seq(1,length(con.22i$X))
con.23i$PID <- 24
con.23i$POS <- seq(1,length(con.23i$X))
con.24i$PID <- 25
con.24i$POS <- seq(1,length(con.24i$X))
con.25i$PID <- 26
con.25i$POS <- seq(1,length(con.25i$X))
con.26$PID <- 27
con.26$POS <- seq(1,length(con.26$X))
con.27i$PID <- 28
con.27i$POS <- seq(1,length(con.27i$X))
con.28i$PID <- 29
con.28i$POS <- seq(1,length(con.28i$X))
con.29i$PID <- 30
con.29i$POS <- seq(1,length(con.29i$X))
con.30i$PID <- 31
con.30i$POS <- seq(1,length(con.30i$X))
con.31i$PID <- 32
con.31i$POS <- seq(1,length(con.31i$X))


		
all.con <- 
	rbind(
		con.0[,c(4,5,1,2)],
		con.27i[,c(4,5,1,2)],
		con.1[,c(4,5,1,2)],
		con.2[,c(4,5,1,2)],
		con.3[,c(4,5,1,2)],
		con.4[,c(4,5,1,2)],
		con.5[,c(4,5,1,2)],
		con.6i[,c(4,5,1,2)],
		con.7i[,c(4,5,1,2)],
		con.8[,c(4,5,1,2)],
		con.9[,c(4,5,1,2)],
		con.11i[,c(4,5,1,2)],
		con.12i[,c(4,5,1,2)],
		con.13i[,c(4,5,1,2)],
		con.14[,c(4,5,1,2)],
		con.15[,c(4,5,1,2)],
		con.16[,c(4,5,1,2)],
		con.17[,c(4,5,1,2)],
		con.18[,c(4,5,1,2)],
		con.19[,c(4,5,1,2)],
		con.20[,c(4,5,1,2)],
		con.21i[,c(4,5,1,2)],
		con.23i[,c(4,5,1,2)],
		con.24i[,c(4,5,1,2)],
		con.25i[,c(4,5,1,2)],
		con.26[,c(4,5,1,2)],
		con.28i[,c(4,5,1,2)],
		con.29i[,c(4,5,1,2)],
		con.30i[,c(4,5,1,2)]
		)
		


mask.poly <- as.PolySet(all.con)
mask.poly$X <- mask.poly$X + 360
labs <- calcCentroid(mask.poly)
labs$label <- labs$PID

plotMap(worldLLhigh, my.xlim, my.ylim, col=grey(0.9),plt=c(0.0,1.0,0.0,1.0),border=grey(0.7),axes=FALSE,tckLab=FALSE,xlab="",ylab="")
addPolys(mask.poly)
addLabels(labs)
box()

