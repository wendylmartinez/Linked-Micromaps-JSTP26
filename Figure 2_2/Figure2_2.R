# This script file creates a plot in:

# "Using Linked Micromaps to Explore Complex Structures in Official Statistics"
# by Powers, Steeg-Morris, Eltinge, and Martinez (2026)
# Accepted for publication in the J. of Statistical Theory & Practice
# It is also posted on arXiv

# Figure 2.2 in the article 


################################
#  Create L-Micromap - Police 
################################

library(micromapST)

##########################
#   Set up colormap
##########################
# These were explored and established in ColorExperiments.R

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
# Left endpoint = 90% interval

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

# Linked Micromap Layout
# Map/sort - State Hourly Wage
# Column 1 - State Hourly Wage
# Column 2 - MSA Min-Max Hourly Wage - as arrow 
# Column 3 - BOS Min-Max Hourly Wage - as arrow
# Column 4 - Scatterplot MSA vs BOS

panelDesc <- data.frame(
  type=c("maptail","id","dot","arrow","arrow","scatdot"),    
  lab1=c("","","Hourly Wage State","Hourly Wage","Hourly Wage","Hourly Wage"), 
  lab2=c("","","(Sort)","Range - MSA","Range - BOS","MSA vs BOS"),
  lab3=c("","","Dollars","Dollars","Dollars","BOS"),
  lab4=c("","","","","","MSA"),
  refVals = c(NA,NA,36.80,36.80,36.80,NA),
  refTexts=c(NA,NA,NA,"National Mean Wage $36.80",NA,NA),
  col1=c(NA,NA,"StMean","Mmin","Bmin","Bmean"),  
  col2=c(NA,NA,NA,"Mmax","Bmax","Mmean")
) 

# Following looks good!

# This produces a plot using the color safe palette
ExtTitle <- c("Police & Sheriff Patrol Officers",
              "Occupational Employment & Wage Statistics 2023")

grDevices::png(file="Figure2_2_colorsafe.png",
               width=8.5,height=10, units = "in", res = 600)

# Sort variable is Mean Wage for State
micromapST(PolData, panelDesc, sortVar=2, ascend=FALSE,
           colors = Pcb,
           title = ExtTitle
)  

x <- grDevices::dev.off()

# This produces a plot using the grayscale palette

grDevices::png(file="Figure2_2_gray.png",
               width=8.5,height=10, units = "in", res = 600)

# Sort variable is Mean Wage for State
micromapST(PolData, panelDesc, sortVar=2, ascend=FALSE,
           colors = "greys",
           title = ExtTitle
)  

x <- grDevices::dev.off()

# This produces a plot using the default palette

grDevices::png(file="Figure2_2_def.png",
               width=8.5,height=10, units = "in", res = 600)

# Sort variable is Mean Wage for State
micromapST(PolData, panelDesc, sortVar=2, ascend=FALSE,
           title = ExtTitle
)  

x <- grDevices::dev.off()

