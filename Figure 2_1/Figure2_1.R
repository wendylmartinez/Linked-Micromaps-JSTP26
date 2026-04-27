# This script file creates Figure 2.1 in the article ...

# "Using Linked Micromaps to Explore Complex Structures in Official Statistics" 
# by Randall Powers, Darcy Steeg Morris, John L. Eltinge and Wendy L. Martinez 

# The article is accepted to the Journal of Statistical Theory and Practice.
# It is also posted on arXiv


##################################
#   Metadata
##################################


# Figure 2.1 in the article 
#
# Used 2020 Q1 through 2022 Q2 in this example. 
#
# It uses the data from the QCEW, found here.
# https://data.bls.gov/maps/cew/us 
#
# Data values/columns were extracted from the QCEW site and placed in .csv files
# so we did not have to load the entire data set.

# Created separate spreadsheets for the Time Series Data to build
# the 3-D array in R. Saved each tab as a CSV file that are read in.

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

###########################
# Load data for  TS arrays
###########################

# We first need to create the 3-D array for the time series in micromapST.

# Read in the years/quarters for the horizontal axis of the series.
# For each state.

#####################
## Changed by DSM  ##
#####################
TS_x_orig <- as.matrix(read.csv("QCEW_emp_X.csv", row.names = 1))

# Make sure it's interpreted as dates
library(zoo)
TS_x <- matrix(as.yearqtr(as.character(TS_x_orig), format = "%Y.%q"), ncol=ncol(TS_x_orig), nrow=nrow(TS_x_orig))
rownames(TS_x) <- rownames(TS_x_orig)
attr(array, "xIsDate") <- TRUE
#####################

# Same for Y, which is the Change Emp rate for each industry.
TS_emp_LH <- as.matrix(read.csv("QCEW_emp_Y_LH.csv", row.names = 1))
# Now for construction
TS_emp_Const <- as.matrix(read.csv("QCEW_emp_Y_Const.csv", row.names = 1))

# Get the row names for the data frame input to micromapST
rown <- row.names(TS_x)

###############################
#  Create TS Arrays
###############################

# Put TS data into the 3-D array required by micromapST

# First create a 3-D array of zeros that we fill in with values.
# We have 51 rows (states), 9 years, and 2 pages (see below)

TSd_LH <- array(0,c(51,9,2))
rownames(TSd_LH) <- rown   # Assign row names, required by micromapST

TSd_Const <- array(0,c(51,9,2))
rownames(TSd_Const) <- rown   # Assign row names, required by micromapST

# Fill in with the data.
# TSd_*[,,1] is a page containing a matrix of years, in our example
# TSd_*[,,2] is a page containing a matrix of change emp rates

# Use 2020 Q1 to 2022 Q1 (9 quarters)
TSd_LH[,,1] <- TS_x    # Horizontal values of time series
TSd_LH[,,2] <- TS_emp_LH     # Vertical values of time series

TSd_Const[,,1] <- TS_x    # Horizontal values of time series
TSd_Const[,,2] <- TS_emp_Const     # Vertical values of time series


# Examine TSd - just so you can see what is going on and if it looks right.
dim(TSd_LH)
TSd_LH[1,,]
head(TSd_LH[,,1])
head(TSd_LH[,,2])
str(TSd_LH)

dim(TSd_Const)
TSd_Const[1,,]
head(TSd_Const[,,1])
head(TSd_Const[,,2])
str(TSd_Const)

# Above arrays look ok.

#############################
# Create the Linked Micromap
#############################

# Columns for Linked Micromaps
# Sort variable is Total Employment in 2021 Q1
# Do basic micromaps - no cumulative of states or sub-regions
#   First data col: Time Series of LH - TSd_LH
#   Second data col: Time Series of Const - TSd_Const

# Note that panelData contains the name of the data object
# containing the values for the time series.
# It must be a 3-D array.

# Load Total Employment 2021 Q2 from QCEW site
# Needed for the sort variable in this example
# Data for the time series are in TSd_LH and TSd_Const

Tot_Emp <- read.csv("TotEmpAll2021Q2.csv", row.names = 2)

panelDesc <- data.frame(
  type=c("map","id","ts","ts"),
  lab1=c("","","Leisure & Hospitality","Construction"),
  lab2=c("","","2020 Q1 to 2022 Q1","2020 Q1 to 2022 Q1"),
  lab3=c("", "","Time","Time"),
  lab4=c("", "","% Change","% Change"),
  col1=c(NA,NA,NA,NA),
  panelData = c(NA, NA,"TSd_LH","TSd_Const")
)


ExTitle <- c("QCEW Over-the-Year Change in Employment",
             "Sorted by Total Employment All Industries, 2021 Q2")

# This creates the plot using color safe palette.
grDevices::png(file="Figure2_1colorsafe.png",
               width=7.5,height=10, units = "in", res = 600)

# Order by Total Employment All Industries in the recovery - 2021 Q2
micromapST(Tot_Emp, panelDesc, sortVar=1, ascend=FALSE,
           colors = Pcb,
           title=ExTitle
)

x <- grDevices::dev.off()

# Now for one in grayscale

# This creates the plot using gray scale palette.
grDevices::png(file="Figure2_1gray.png",
               width=7.5,height=10, units = "in", res = 600)

# Order by Total Employment All Industries in the recovery - 2021 Q2
micromapST(Tot_Emp, panelDesc, sortVar=1, ascend=FALSE,
           colors = "greys",
           title=ExTitle
)

x <- grDevices::dev.off()

# This creates the plot using default palette.
grDevices::png(file="Figure2_1def.png",
               width=7.5,height=10, units = "in", res = 600)

# Order by Total Employment All Industries in the recovery - 2021 Q2
micromapST(Tot_Emp, panelDesc, sortVar=1, ascend=FALSE,
           title=ExTitle
)

x <- grDevices::dev.off()

