# This script file creates Figure 1.1 in the article ...
# "Using Linked Micromaps to Explore Complex Structures in Official Statistics" 
# by Randall Powers, Darcy Steeg Morris, John L. Eltinge and Wendy L. Martinez 

# The article is accepted to the Journal of Statistical Theory and Practice.
# It is also posted on arXiv

##################################
#   Metadata
##################################

# Sort variable will be the number of establishments in Q1 2025. 

# It uses the data from the QCEW, found here.
# https://data.bls.gov/maps/cew/us 
#
# Data values/columns were extracted from the QCEW site and placed in .csv files
# so we did not have to load the entire data set.

# Created separate spreadsheets for the Time Series Data to build
# the 3-D array in R. Saved each tab as a CSV file that are read in.

# These data sets are available in the github repo for this article.

library(micromapST)

##########################
#   Set up colormap
##########################
# The default colormap in the micromapST package is not safe for those
# having trouble visualizing colors. 

# The following is a color-safe palette. We include code for three
# versions of a linked micromap: color safe, default, and grayscale.

# Part of Base R
Poi <- palette.colors(9, palette = "Okabe-Ito")

# Pick the colors we want for micromapST
# Call that function with colors set to this vector
Pcb <- Poi[c(2,3,5,6,7,9,1,4,4,8,9,9)]
Pcb[3] = "#FFFF00"     # Brightens the yellow
# Make it lighter gray
Pcb[c(6,11,12)] <- "#D3D3D3"


##################################
#  Upload Data for 2025 Q1
#        All Industries
##################################

# Note that this has two columns where the units for Empl and Wages
# are in Millions (M) and Thousands (K) respectively.

# Users should change the directory in read.csv to reflect the location of the
# data files.
IndAll_25q1 <- as.matrix(read.csv("IndAll_25Q1.csv", row.names = 1))


###################################
#   Upload Data for Time Series
###################################

# We first need to create the 3-D array for the time series in micromapST.

# Read in the years/quarters for the horizontal axis of the series.
# For each state.

#####################
## Changed by DSM ###
#####################
TS_x_orig <- as.matrix(read.csv("QCEW25_X.csv", row.names = 1))

# Make sure it's interpreted as dates

library(zoo)

# Make sure R is interpreting the data as time.
TS_x <- matrix(as.yearqtr(as.character(TS_x_orig), format = "%y.%q"), ncol=ncol(TS_x_orig), nrow=nrow(TS_x_orig))
rownames(TS_x) <- rownames(TS_x_orig)
attr(array, "xIsDate") <- TRUE

#####################

# Same for Y, which is the Tot Emp for All industries 2020Q1 thru 2025Q1.
TS_emp <- as.matrix(read.csv("QCEW25_emp_Y.csv", row.names = 1))

# Convert to Millions
TS_empM <- TS_emp/1000000

# Now for Y, which is the Avg Weekly wages for All industries same time period
TS_wages <- as.matrix(read.csv("QCEW25_wage_Y.csv", row.names = 1))
# Convert to Thousands
# TS_wages <- TS_wages

# Get the row names for the data frame input to micromapST
rown <- row.names(TS_x)

###############  Time Series #########################

# Put TS data into the 3-D array required by micromapST

# First create a 3-D array of zeros that we fill in with values.
# We have 51 rows (states), 21 years, and 2 pages (see below)

TSd_EmpM <- array(0,c(51,21,2))
rownames(TSd_EmpM) <- rown   # Assign row names, required by micromapST

TSd_Wages <- array(0,c(51,21,2))
rownames(TSd_Wages) <- rown   # Assign row names, required by micromapST

# Fill in with the data.
# TSd_*[,,1] is a page containing a matrix of years, in our example
# TSd_*[,,2] is a page containing a matrix of Tot Emp, for example

# Use all columns
TSd_EmpM[,,1] <- TS_x    # Horizontal values of time series
TSd_EmpM[,,2] <- TS_empM     # Vertical values of time series

TSd_Wages[,,1] <- TS_x    # Horizontal values of time series
TSd_Wages[,,2] <- TS_wages     # Vertical values of time series


# Examine TSd - just so you can see what is going on and if it looks right.
head(TSd_EmpM[,,1])  # Header of first page, time values
head(TSd_EmpM[,,2])  # Header of second page, Emp values

#################################
#  Create the LMM
################################

panelDesc <- data.frame(
  type=c("map","id","ts","ts"),
  lab1=c("","","Total Employment (M)","Average Weekly Wage"),
  lab2=c("","","2020 Q1 to 2025 Q1","2020 Q1 to 2025 Q1"),
  lab3=c("", "","Time","Time"),
  lab4=c("", "","Employment (M)","Wage"),
  col1=c(NA,NA,NA,NA),
  panelData = c(NA, NA,"TSd_EmpM","TSd_Wages")
)


ExTitle <- c("QCEW All Industries",
             "Sorted by Total Establishments 2025 Q1")

grDevices::png(file="QCEW25tsB.png",
               width=7.5,height=10, units = "in", res = 600)

# One sort order
micromapST(data.frame(IndAll_25q1), panelDesc, sortVar= "Establishments", ascend=FALSE,
           colors = Pcb,
           title=ExTitle
)

x <- grDevices::dev.off()

#############################################################################
# This has two columns - dots and time series.
# This one is not in the article.
#############################################################################

panelDesc <- data.frame(
  type=c("map","id","dot","ts"),
  lab1=c("","","Total Establishments (M)","Average Weekly Wage"),
  lab2=c("","","2025 Q1","2020 Q1 to 2025 Q1"),
  lab3=c("", "","Time","Time"),
  lab4=c("", "","","Wage"),
  col1=c(NA,NA,"Establishments",NA),
  panelData = c(NA, NA,NA,"TSd_Wages")
)


ExTitle <- c("QCEW All Industries",
             "Sorted by Total Establishments 2025 Q1")

# Open the graphics device or file where the plot will appear.
grDevices::png(file="QCEW25ts_dotB.png",
               width=7.5,height=10, units = "in", res = 600)

# One sort order
# This sets the colormap to the color safe one.
micromapST(data.frame(IndAll_25q1), panelDesc, sortVar= "Establishments", ascend=FALSE,
           colors = Pcb,
           title=ExTitle
)

x <- grDevices::dev.off()

#########################################
#  Add scatterplot as the last column
#  This is in the article as Figure 1.1
#  This is the COLOR SAFE version
#########################################

panelDesc <- data.frame(
  type=c("map","id","dot","ts","scatdot"),
  lab1=c("","","Total Establishments (M)","Average Weekly Wage","%Chng Wage vs %Chng Emp"),
  lab2=c("","","2025 Q1","2020 Q1 to 2025 Q1","2025 Q1"),
  lab3=c("", "","","Time","% Chng Emp"),
  lab4=c("", "","","Wage","% Chng Wage"),
  col1=c(NA,NA,"EstablishmentsM",NA,"Employment_Delta"),
  panelData = c(NA, NA,NA,"TSd_Wages",NA),
  col2=c(NA,NA,NA,NA,"Wage_Delta")
)

# Make sure this is a data frame
IndAll_25q1 = data.frame(IndAll_25q1)
# Change Establishments to millions
IndAll_25q1$EstablishmentsM = IndAll_25q1$Establishments/1000000

ExTitle <- c("QCEW All Industries - 2025 Q1",
             "Sorted by Total Establishments")

grDevices::png(file="Figure_1_1_colorsafe.png",
               width=7.5,height=10, units = "in", res = 600)

# One sort order
# Still the color safe colormap
micromapST(IndAll_25q1, panelDesc, sortVar= "EstablishmentsM", ascend=FALSE,
           colors = Pcb,
           title=ExTitle
)

x <- grDevices::dev.off()

#########################################
#  Add scatterplot as the last column
#  This is in the article as Figure 1.1
#  This is the GRAYSCALE version
#########################################

grDevices::png(file="Figure_1_1_gray.png",
               width=7.5,height=10, units = "in", res = 600)

# One sort order
# Still the color safe colormap
micromapST(IndAll_25q1, panelDesc, sortVar= "EstablishmentsM", ascend=FALSE,
           colors =  "greys",
           title=ExTitle
)

x <- grDevices::dev.off()

#############################################
#  Add scatterplot as the last column
#  This is in the article as Figure 1.1
#  This is the DEFAULT color palette version
#############################################

grDevices::png(file="Figure_1_1_def.png",
               width=7.5,height=10, units = "in", res = 600)

# One sort order
# Still the color safe colormap
micromapST(IndAll_25q1, panelDesc, sortVar= "EstablishmentsM", ascend=FALSE,
           title=ExTitle
)

x <- grDevices::dev.off()


