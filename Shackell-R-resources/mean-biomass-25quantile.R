## for Nancy, generate a csv file that includes the average biomass during the top 25% years
## started: 2012-09-21


get.mean.biomass.top25 <- function(spec.num){
fn <- paste("../Data/SS",spec.num,"_stratified.csv",sep="")
dat.in <- read.csv(fn, header=TRUE)

return(mean(subset(dat.in, b >= quantile(dat.in$b, prob=c(0.75)))$b))
} # end function



  #setwd("C:/ATLAS_poissons_SS/Shackell-R-resources")
  #setwd("R:/Shared/Shackell_Nancy/ATLAS_poissons_SS/Shackell-R-resources")
  
  spec.list <- read.csv("../Report/species-list.csv",header=FALSE)
  species.L <- spec.list[spec.list$V5=='L',]$V1 # long timeseries

my.df <- data.frame(species= species.L, mean.biomass.top25 = sapply(species.L, function(i){get.mean.biomass.top25(spec.num=i)}))

write.csv(my.df, "mean-biomass-top25.csv")


