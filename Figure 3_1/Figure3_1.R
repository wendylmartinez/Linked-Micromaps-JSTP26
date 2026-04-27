# This script file creates Figure 3.1 in the article ...

# "Using Linked Micromaps to Explore Complex Structures in Official Statistics" 
# by Randall Powers, Darcy Steeg Morris, John L. Eltinge and Wendy L. Martinez 

# The article is accepted to the Journal of Statistical Theory and Practice.
# It is also posted on arXiv

# Figure 3.1 in the article 


################################
#  Create L-Micromap - Police 
################################

library(micromapST)

##########################
#   Set up colormap
##########################
# These were established as color safe.

# Part of Base R
Poi <- palette.colors(9, palette = "Okabe-Ito")

# Pick the colors we want for micromapST
# Call that function with colors set to this vector
Pcb <- Poi[c(2,3,5,6,7,9,1,4,4,8,9,9)]
Pcb[3] = "#FFFF00"     # Brightens the yellow
# Make it lighter gray
Pcb[c(6,11,12)] <- "#D3D3D3" 

##########################
#  Load Data
##########################

# Load police data
Statepolice <- read.csv("Statepolice.csv", row.names = 1)
MSApolice <- read.csv("MSAPolice.csv")
BOSpolice <- read.csv("BOSPolice.csv")

# Get state names for later assignment to aggregated data
rown <- row.names(Statepolice)

# See if any states are missing
tmpBOS <- unique(BOSpolice$STATE)   # 51 of these
tmpMSA <- unique(MSApolice$STATE)   # 51 of these

# Need to aggregate - use Low and High - Wages - Hourly
BOSmin <- aggregate(H_MEAN ~ STATE, data = BOSpolice, FUN = min)
MSAmin <- aggregate(H_MEAN ~ STATE, data = MSApolice, FUN = min)

BOSmax <- aggregate(H_MEAN ~ STATE, data = BOSpolice, FUN = max)
MSAmax <- aggregate(H_MEAN ~ STATE, data = MSApolice, FUN = max)

# Need to get mean wage for MSA and BOS
BOSmean <- aggregate(H_MEAN ~ STATE, data = BOSpolice, FUN = mean)
MSAmean <- aggregate(H_MEAN ~ STATE, data = MSApolice, FUN = mean)

# See if it looks reasonable
tmpBOSb <- cbind(BOSmin,BOSmax)  # Looks ok. Has col with state IDs
tmpMSAb <- cbind(MSAmin,MSAmax)

# Get the left and right CI values for hourly mean wage

HwageLo <- Statepolice$H_MEAN - (1.645 * Statepolice$MEAN_PRSE/100 * Statepolice$H_MEAN)
HwageHi <- Statepolice$H_MEAN + (1.645 * Statepolice$MEAN_PRSE/100 * Statepolice$H_MEAN)

Statepolice$HwageLo <- HwageLo
Statepolice$HwageHi <- HwageHi


# Need a data set with the required data columns

PolData <- data.frame(
  LQ = Statepolice$LOC_QUOTIENT,
  StMean = Statepolice$H_MEAN,
  Mmin = MSAmin$H_MEAN,
  Mmax = MSAmax$H_MEAN,
  Bmin = BOSmin$H_MEAN,
  Bmax = BOSmax$H_MEAN,
  Bmean = BOSmean$H_MEAN,
  Mmean = MSAmean$H_MEAN,
  WageLo = Statepolice$HwageLo,
  WageHi = Statepolice$HwageHi
)

rownames(PolData) = rown

############################################
#  Create Police Wages Micromap
############################################

# Set up the layout for the linked micromap
panelDesc <- data.frame(
  type=c("maptail","id","scatdot","dot","arrow","arrow"),    
  lab1=c("","","LQ vs Wage","Hourly Wage","Hourly Wage","Hourly Wage"), 
  lab2=c("","","(Sort by Wage)","State (Sort)","Range - MSA","Range - BOS"),
  lab3=c("","","Dollars","Dollars","Dollars","Dollars"),
  lab4=c("","","LQ","","",""),
  refVals = c(NA,NA,NA,36.80,36.80,36.80),
  refTexts=c(NA,NA,NA,NA,"National Mean",NA),
  col1=c(NA,NA,"StMean","StMean","Mmin","Bmin"),
  col2=c(NA,NA,"LQ",NA,"Mmax","Bmax")
) 

# Add Loess to the scatdot column.
# Must have the latest version of micromapST - 3.1.1 - on CRAN

wparm <- list(NA,
              NA,
              list(line="LOWESS",f=.1,line.col="black",line.lwd=1,line.lty="solid"),
              NA,
              NA,
              NA)

panelDesc$parm <- wparm

# Following looks good!

ExtTitle <- c("Police & Sheriff Patrol Officers",
              "Occupational Employment & Wage Statistics 2023")

# Color safe palette
grDevices::png(file="Figure3_1_colorsafe.png",
               width=8.5,height=10, units = "in", res = 600)

# Sort variable is Mean Wage for State
micromapST(PolData, panelDesc, sortVar=2, ascend=FALSE,
           colors = Pcb,
           title = ExtTitle
)  

x <- grDevices::dev.off()

# Grayscale palette
grDevices::png(file="Figure3_1_grayscale.png",
               width=8.5,height=10, units = "in", res = 600)

# Sort variable is Mean Wage for State
micromapST(PolData, panelDesc, sortVar=2, ascend=FALSE,
           colors = "greys",
           title = ExtTitle
)  

x <- grDevices::dev.off()

# Grayscale palette
grDevices::png(file="Figure3_1_def.png",
               width=8.5,height=10, units = "in", res = 600)

# Sort variable is Mean Wage for State
micromapST(PolData, panelDesc, sortVar=2, ascend=FALSE,
           title = ExtTitle
)  

x <- grDevices::dev.off()

