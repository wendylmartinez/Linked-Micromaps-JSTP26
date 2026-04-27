# This script file creates Figure 2.3 in the article ...

# "Using Linked Micromaps to Explore Complex Structures in Official Statistics" 
# by Randall Powers, Darcy Steeg Morris, John L. Eltinge and Wendy L. Martinez 

# The article is accepted to the Journal of Statistical Theory and Practice.
# It is also posted on arXiv

# Figure 2.3 in the article 

#######################################
#   New York QCEW Manufacturing 2024 Q3
#######################################

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


# Get pop counts from Census
# July 2024: https://www.census.gov/data/tables/time-series/demo/popest/2020s-counties-total.html

# Added a column of zeros for the starting point of the arrows.

# NOTE: Richmond County had missing data.There is an option to ignore NAs.

# THIS WORKED - although we did get some warnings!!!

# Load state data
NYmfr <- read.csv("QCEW_mfr_Pop_NA.csv", header = TRUE)

panelDesc <- data.frame(
  type=c("map","id","arrow", "arrow")
  ,lab1=c("", "", "Percent Change in Employment", "Percent Change in Wages")
  ,lab2=c("", "", "", "")
  ,lab3=c("", "", "Employment", "Wages")
  ,col1=c(NA, NA, "Start", "Start")
  ,col2=c(NA, NA, "CngEmpPercent", "CngWeekWagesPercent")
  ,refVals=c(NA,NA,0,0)
)

ExTitle <- c("New York Manufacturing 2024 Q4 - Sort by Population (July 2024)",
             "Over-the-year-change in Employment and Wages")

grDevices::png(file="Figure2_3_colorsafe.png",
               width=8.5,height=10, units = "in", res = 700)

micromapST(NYmfr, panelDesc,
           sortVar="PopJuly24", ascend=FALSE,
           title=ExTitle,
           ignoreNoMatches = TRUE,
           color = Pcb,
           rowNames="full",rowNamesCol="Area",
           bordGrp="NewYorkBG"
)

x <- grDevices::dev.off()


#  Now grayscale
grDevices::png(file="Figure2_3_grayscale.png",
               width=8.5,height=10, units = "in", res = 700)

micromapST(NYmfr, panelDesc,
           sortVar="PopJuly24", ascend=FALSE,
           title=ExTitle,
           ignoreNoMatches = TRUE,
           color = "greys",
           rowNames="full",rowNamesCol="Area",
           bordGrp="NewYorkBG"
)

x <- grDevices::dev.off()


#  Now default colors
grDevices::png(file="Figure2_3_def.png",
               width=8.5,height=10, units = "in", res = 700)

micromapST(NYmfr, panelDesc,
           sortVar="PopJuly24", ascend=FALSE,
           title=ExTitle,
           ignoreNoMatches = TRUE,
           rowNames="full",rowNamesCol="Area",
           bordGrp="NewYorkBG"
)

x <- grDevices::dev.off()
