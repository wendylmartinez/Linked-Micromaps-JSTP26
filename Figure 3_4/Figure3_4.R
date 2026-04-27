# This script file creates a plot in:

# "Using Linked Micromaps to Explore Complex Structures in Official Statistics"
# by Powers, Steeg-Morris, Eltinge, and Martinez (2026)
# Accepted for publication in the J. of Statistical Theory & Practice

# Figure 3.4 in the article 

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

###################################################
#  Load Software for State - Software Developers
###################################################

# Only load state data
Statesoft <- read.csv("Statesoft.csv", row.names = 1)

##########################
#  Construct intervals
##########################

# FROM:  https://www.bls.gov/ecec/factsheets/ecec-relative-standard-errors.htm
# Example 1: building an interval
# 
# Retirement and savings costs for professional and business services workers = $0.17
# Percent standard error = 39.0%
# 90% confidence interval = $0.17+/- (1.645 x .39 x $0.17) = [$0.06, $0.28]

# Get the left and right CI values for hourly mean wage
# Left endpoint = 90% interval

HwageLo <- Statesoft$H_MEAN - (1.645 * Statesoft$MEAN_PRSE/100 * Statesoft$H_MEAN)
HwageHi <- Statesoft$H_MEAN + (1.645 * Statesoft$MEAN_PRSE/100 * Statesoft$H_MEAN)

Statesoft$HwageLo <- HwageLo
Statesoft$HwageHi <- HwageHi

################################
#  Create L-Micromap - Software
################################

##################################################
#  Try the boxplots using the quantiles explicitly
##################################################

# use the percentiles to build a boxplot.

# I need to stack the different columns and State Abr
# Note that in micromapST the boxplots are created using something
# other than the main data object, so it will be a separate Tmp matrix

# Get the boxplot data for the percentiles into one data frame
TmpDat <- data.frame(t(Statesoft[,8:12]))
TmpStack <- stack(TmpDat)  # Has the right number of elements

# Change the column names.
colnames(TmpStack) <- c("Value", "State")

# Create a boxplot and save the results for micromapST
boxlist <- boxplot(split(TmpStack$Value,TmpStack$State),
                   plot = TRUE,
                   outpch = 20,
                   outcex = .5)

Tmp <- boxlist$stats

# Reset the Stats component to the actual percentiles.

boxlist$stats <- as.matrix(TmpDat)

panelDesc <- data.frame(
  type=c("maptail","id","dot","dotconf","boxplot"),    
  lab1=c("","","Location Quotient","Hourly Wage","Hourly Wage"), 
  lab2=c("","","Employment","w/ 90% CI","Percentiles"),
  lab3=c("","","LQ","Dollars","Dollar"),
  refVals = c(NA,NA,1,66.40,63.59),
  refTexts=c(NA,NA,NA,"Nat Mean $66","Nat Median $64"),
  col1=c(NA,NA,"LOC_QUOTIENT","H_MEAN",NA),
  col2=c(NA,NA,NA,"HwageLo",NA),
  col3=c(NA,NA,NA,"HwageHi",NA),
  panelData=c("","","","","boxlist")
) 

# Following looks good!

# This is the color safe palette
ExtTitle <- c("Software Developers - Employment & Wages",
              "Occupational Employment & Wage Statistics 2023")

grDevices::png(file="Figure3_4_colorsafe.png",
               width=8.5,height=10, units = "in", res = 600)

micromapST(Statesoft, panelDesc, sortVar=4, ascend=FALSE,
           colors = Pcb,
           title = ExtTitle
)  

x <- grDevices::dev.off()

# This is the gray scale palette
grDevices::png(file="Figure3_4_gray.png",
               width=8.5,height=10, units = "in", res = 600)

micromapST(Statesoft, panelDesc, sortVar=4, ascend=FALSE,
           colors = "greys",
           title = ExtTitle
)  

x <- grDevices::dev.off()


# This is the default palette
grDevices::png(file="Figure3_4_def.png",
               width=8.5,height=10, units = "in", res = 600)

micromapST(Statesoft, panelDesc, sortVar=4, ascend=FALSE,
           title = ExtTitle
)  

x <- grDevices::dev.off()

