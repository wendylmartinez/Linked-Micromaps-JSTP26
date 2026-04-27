# This script file creates a plot in:

# "Using Linked Micromaps to Explore Complex Structures in Official Statistics"
# by Powers, Steeg-Morris, Eltinge, and Martinez (2026)
# Accepted for publication in the J. of Statistical Theory & Practice

# Figure 3.3 in the article 


##################################################
#   Teachers
##################################################

library(micromapST)

# Part of Base R
Poi <- palette.colors(9, palette = "Okabe-Ito")

# Pick the colors we want for micromapST
# Call that function with colors set to this vector
Pcb <- Poi[c(2,3,5,6,7,9,1,4,4,8,9,9)]
Pcb[3] = "#FFFF00"     # Brightens the yellow
# Make it lighter gray
Pcb[c(6,11,12)] <- "#D3D3D3" 


###################################
#  Elem - Special Needs
###################################

# Let's look at Special Ed - Elementary Level
# That level does not have career

# Special Education Teachers, Kindergarten and Elementary School
# 25-2052

##############################
#  Load Data Elementary
##############################

# Load state data
Elem <- read.csv("StateElem.csv", row.names = 1)
ElemSpEd <- read.csv("StateElemSpEd.csv",row.names = 1)

rown <- row.names(Elem)

# Linked Micromap - Layout
#    Sort - LQ - SE
#    Scatter - LQ_Elem vs LQ_SE
#    Scatter - Wage_Elem vs Wage_SE

# Special Ed - Mean Wage 71,770
# Elementary - Mean Wage 70,740

# Need to get data object with correct stuff in it

MMdat <- data.frame(
  LQ_SE = ElemSpEd$LOC_QUOTIENT,
  LQ_EL = Elem$LOC_QUOTIENT,
  ElemWage = Elem$A_MEAN,
  SEWage = ElemSpEd$A_MEAN,
  row.names = rown)

panelDesc <- data.frame(
  type=c("maptail","id","dot","scatdot","scatdot"),    
  lab1=c("","","Location Quotient (LQ)","LQ General Ed","Annual Wage ($)"), 
  lab2=c("","","Spec Ed (sort)","vs LQ Spec Ed","General Ed vs Special Ed"),
  lab3=c("","","LQ_SpEd (Occupation)","LQ_SpEd (Occupation)","Special Ed"),  # X vals
  lab4=c("","","","LQ_Gen","Gen Ed"),   # y vals
  refVals=c(NA,NA,1,NA,NA),
  col1=c(NA,NA,"LQ_SE","LQ_SE","SEWage"),
  col2=c(NA,NA,NA,"LQ_EL","ElemWage")
) 

ExtTitle <- c("Elementary: General Ed Compared to Special Ed Teachers",
              "Occupational Employment & Wage Statistics 2023")

grDevices::png(file="TeacherSpEd.png",
               width=8.5,height=10, units = "in", res = 700)

# Sort variable is Mean Wage for State
micromapST(MMdat, panelDesc, sortVar=1, ascend=FALSE,
           colors = Pcb,
           title = ExtTitle
)  

x <- grDevices::dev.off()

#####################################
#  Add Loess
#####################################

# Add Loess to the scatdot column.
# Must have the latest version of micromapST - 3.1.1 - on CRAN

# Specify parameters for Loess
wparm <- list(NA,NA,NA,list(line="LOWESS",f=2/3,line.col="black",line.lwd=1,line.lty="solid"),
              NA)

# Add to panel description
panelDesc$parm <- wparm

ExtTitle <- c("Elementary: General Ed Compared to Special Ed Teachers",
              "Occupational Employment & Wage Statistics 2023")

# This uses the color safe palette
grDevices::png(file="Figure3_3_colorsafe.png",
               width=8.5,height=10, units = "in", res = 700)

# Sort variable is Mean Wage for State
micromapST(MMdat, panelDesc, sortVar=1, ascend=FALSE,
           colors = Pcb,
           title = ExtTitle
)  

x <- grDevices::dev.off()

# This uses the gray scale palette
grDevices::png(file="Figure3_3_grayscale.png",
               width=8.5,height=10, units = "in", res = 700)

# Sort variable is Mean Wage for State
micromapST(MMdat, panelDesc, sortVar=1, ascend=FALSE,
           colors = "greys",
           title = ExtTitle
)  

x <- grDevices::dev.off()

# This uses the default palette
grDevices::png(file="Figure3_3_def.png",
               width=8.5,height=10, units = "in", res = 700)

# Sort variable is Mean Wage for State
micromapST(MMdat, panelDesc, sortVar=1, ascend=FALSE,
           title = ExtTitle
)  

x <- grDevices::dev.off()

##############################################
#  Explore scatterplot with different smooths
#      Outside of micromapST
##############################################

# Note that the micromapST package uses the lowess.
# ggplot2 default is loess

# Explore other spans for the smooth to explore other
# structures and relationships.

# Create separate scatterplot with smooth using ggplot
# Annotates the outlying points to compare with 
# linked regions in micromapST

# This is the span used in Figure 3.3

library(ggplot2)

# This shows more structure - peaks and troughs.
# Uses the same span as Figure 3.3, but it's a 
# different smooth
ggplot(MMdat, aes(LQ_SE, LQ_EL)) +
  geom_point() +
  geom_smooth(span = 2/3) +
  labs(
    x = "Location Quotient - Special Education",
    y = "Location Quotient - General Education"
  ) 

# Try different values of the span.

# Smaller span, so more wiggles
ggplot(MMdat, aes(LQ_SE, LQ_EL)) +
  geom_point() +
  geom_smooth(span = 0.4) +
  labs(
    x = "Location Quotient - Special Education",
    y = "Location Quotient - General Education"
  ) 


#######################################################
#   lowess function in base R to match micromapST
#                Figure 3.3
#######################################################

# Try the lowess function in base R. That fits micromapST.

# Did some plotting with different spans. 

plot(MMdat$LQ_SE, MMdat$LQ_EL, 
     main = "Elementary School Teachers",
     xlab = "Location Quotient Special Ed",
     ylab = "Location Quotient General Ed")

# Add the Lowess Smooths with different spans.
lines(lowess(MMdat$LQ_SE, MMdat$LQ_EL, f = 2/3), col = 'black',lwd = 2)
lines(lowess(MMdat$LQ_SE, MMdat$LQ_EL, f = .3), col = 'red',lty=2,lwd = 2)

# When copying to clipboard in Plot Window Export menu, used 500 by 375.








