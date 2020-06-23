## stratified random estimate of abudance and biomass
## 3x3 IDW maps
## cod 4VsW
## - exclude strata 440, 441 and 442, include strata 443 to 469, exclude strata above 470

my.path <- file.path("C:/ATLAS_poissons_SS/Shackell-R-resources")
cod.catch.df <- extract.catch.fct(10,'4VsW')
strat.list <- stratified.fct(cod.catch.df, DDHS=FALSE)
stratified.df <- strat.list[[1]]

## generate the figure 
	# name of graphics file
	devName <- "Stratified-4VsW"
	# open graphics device
	devOpen(devName, saveDir=my.path, fileName=paste("Shackell-request-20120927-","SS", 10, "_", devName, sep=""), saveFormat='pdf', width=12, height=4)
	# graphics parameters
	par(mfrow=c(1,2), omi=c(0,0,0,0), mai=c(0.35, 0.5, 0.1, 0.1), ps=8, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
	# text parameters
	cexF=list(title=1, axis=0.8,labels=0.9,legend=1, unit="in")
	# call appropriate figure functions
	figure13.fct(stratified.df, cexF)
	title("Cod - 4VsW - Summer - Abundance")
	figure14.fct(stratified.df, cexF)
	title("Cod - 4VsW - Summer - Biomass")
	
	# save the graphic device
	devSave(devName)
