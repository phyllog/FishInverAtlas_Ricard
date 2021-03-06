#--------------------------------------------------------------------------#
## main R script to generate the figures required in the DFO Maritimes groundfish survey atlas
## Daniel Ricard, started 2012-07-20 relying extensively on Hugo Bourdages' work on the NGSL atlas
##
options(echo=FALSE)

print(paste("Script Atlas.R started: ", Sys.time()))

# required libraries
  necessary <- c("PBSmapping","spatstat","zoo","classInt","RColorBrewer","gstat","maptools",
                  "foreign","fields","spam","rgeos", "RODBC", "xtable", "MASS", "SSOAP")
  installed <- necessary %in% installed.packages()[, 'Package']
  if (length(necessary[!installed]) >=1)
    install.packages(necessary[!installed], repos='http://mirror.its.dal.ca/cran/')

	# lapply(necessary, function(i){citation(i)})
# set the correct path
  #path.ATLAS=file.path("C:/ATLAS_poissons_SS")
#  path.ATLAS=file.path("C:/Documents and Settings/RicardD/My Documents/Dropbox/ATLAS_poissons_SS") ## at BIO
#  path.ATLAS=file.path("D:/Dropbox/ATLAS_poissons_SS") ## on my Acer notebook
#  path.ATLAS=file.path("C:/Users/shackelln/Dropbox/ATLAS_poissons_SS") ## at BIO
  path.ATLAS=file.path("C:/RProjects/FishInverAtlas_Ricard")
## open ODBC connection to Oracle database
	require(RODBC, quietly=TRUE, warn.conflicts = FALSE)
  source("C:/RProjects/FishInverAtlas_Ricard/FunctionsR/chan.R")
  # chan <- odbcConnect(dsn='biobank', uid='', pwd='')
	# source the code that defines the data extraction functions
	source(file.path(path.ATLAS, "FunctionsR/data-extract.R"))

# source the code that defines the function for each figure
  source(file.path(path.ATLAS, "FunctionsR/figures.R"))

# source the code that defines the function for habitat suitability
  source(file.path(path.ATLAS, "FunctionsR/habitat-suitability.R"))

# source the code that sets all the mapping functions and data requirements such as polygons and masks, this file also creates a survey map with strata polygons and NAFO divisions
	source(file.path(path.ATLAS, "Mapping/SUMMER-strata.R"))
  
# source the code that computes the survey summaries (e.g. number of sets per stratum per year, etc.) and generates the summary tables to appear at the front of the atlas
#	The following line updates the file Report/species-list-final.csv
	source(file.path(path.ATLAS, "FunctionsR/summaries.R"))

## actual function calls for species-level analyses
## first call is to generate the data files for each species
## second call is to generate the figures for each species
  
  # 2012-09-20: I'm turning the species list into the authoritative list by which the data extraction and the generation of figures in R is done
  spec.list <- read.csv(file.path(path.ATLAS, "/Report/species-list-final.csv"),header=FALSE) # this list is itself generated from the above "summaries.R", which requires database connection and connection to WORMS to get AphiaID
  
  species.L <- spec.list[spec.list$V9=='L',]$V4 # long timeseries
  species.S <- spec.list[spec.list$V9=='S',]$V4 # short timeseries
  species.LR <- spec.list[spec.list$V9=='LR',]$V4 # long timeseries rare
  species.SR <- spec.list[spec.list$V9=='SR',]$V4 # short timeseries rare
  species.I <- spec.list[spec.list$V9=='I',]$V4 # intermediate species

  # GROUP : well identified species with no DDHS fitting problems
  species.numbers <- species.L # c(10,11,12,13,14,16,23,40,41,42,43,60,300,4511,320,220,640,400,200,201,202,203,204,50,30,304,62,160,70,304,112,15,31,241) #,114) 
  # extract all the data and plot all the figures for GROUP
	print(paste("Starting data extracts, L species: ", Sys.time()))
	sapply(species.numbers, function(i){data.extract(extract.num=c(1,2,3,5,6,7), spec.num=i)})
	print(paste("End data extract, starting figures, L species: ", Sys.time()))
	sapply(species.numbers, function(i){figures(spec.num=i, fig=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,17,18,19,20))})
	#sapply(species.L, function(i){figures(spec.num=i, fig=c(15,20))})

	# sapply(species.numbers, function(i){figures(spec.num=i, fig=c(19))})
	print(paste("End figures, L species: ", Sys.time()))
  
  # GROUP : well identified species with DDHS fitting problems
  #species.numbers <- c(610,143)
  #sapply(species.numbers, function(i){data.extract(extract.num=c(1,2,3,4,6,7), spec.num=i)})
  #sapply(species.numbers, function(i){figures(spec.num=i, fig=c(2,3,4,5,6,7,8,9,10,11,12,13,14,17,18,19,20))})
    
  # GROUP : recorded since 1999, invertebrates
  species.numbers <- species.S # c(2526,2550,2511,2211,2527,2521,2513,4321,340,2523)
	print(paste("Starting data extracts, S species: ", Sys.time()))
	sapply(species.numbers, function(i){data.extract(extract.num=c(8,9,10), spec.num=i)}) # extract only catch data
    print(paste("End data extract, starting figures, S species: ", Sys.time()))
	sapply(species.numbers, function(i){figures(spec.num=i, fig=c(13,14,16,5,18,17,19,21,22))}) # 
    print(paste("End figures, S species: ", Sys.time()))
	
  # GROUP : rare species
  species.numbers <- c(species.LR, species.SR) # c(341,742,637,816,741,240,159,52,4514,4512,414,604,520,500,621,412,51,303,626,4320,704,505,720,512,158,221,307,603,503,17,149,646,351,314,63,142,620,122,156,323,619,641,301,630,150,200,712,19,502,501,625,642,143) #,114, 2532,
	print(paste("Starting data extracts, LR species: ", Sys.time()))
	sapply(species.numbers, function(i){data.extract(extract.num=c(1), spec.num=i)}) # extract only catch data
	print(paste("End data extract, starting figures, LR species: ", Sys.time()))
	sapply(species.numbers, function(i){figures(spec.num=i, fig=c(16))}) # plot only tow locations
	print(paste("End figures, LR species: ", Sys.time()))
  
  # GROUP : intermediate species where abundance should be used instead of biomass
  species.numbers <- species.I # c(114, 647, 64, 410, 123, 623, 622, 61, 306, 701, 350, 880, 44)
	print(paste("Starting data extracts, I species: ", Sys.time()))
	sapply(species.numbers, function(i){data.extract(extract.num=c(1,3,4), spec.num=i)}) # extract catch data and generate distribution indices
	print(paste("End data extract, starting figures, I species: ", Sys.time()))
	sapply(species.numbers, function(i){figures(spec.num=i, fig=c(5,6,10,13,14,17,19))}) 
	print(paste("End figures, I species: ", Sys.time()))

  #sapply(species.numbers, function(i){figures(spec.num=i, fig=c(13,14))})
  #sapply(species.numbers, function(i){figures(spec.num=i, fig=c(5))})
  #
  
	## close ODBC connection
	odbcClose(chan)
	
print(paste("Script Atlas.R finished: ", Sys.time()))
q("no")

	## now run analyses for multiple species and trophic groups
	## identification of "core habitats"
	# habitat.suitability(species.num=species.numbers)

	## blah blah
