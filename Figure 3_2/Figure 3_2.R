# This script file creates Figure 3.2 in the article ...

# "Using Linked Micromaps to Explore Complex Structures in Official Statistics" 
# by Randall Powers, Darcy Steeg Morris, John L. Eltinge and Wendy L. Martinez 

# The article is accepted to the Journal of Statistical Theory and Practice.
# It is also posted on arXiv

# Figure 3.2 in the article 

############################################
#  Create Police Wages - Explore with ggplot
############################################

library(ggplot2)

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

##################################################
#   Explore other scatterplot smooths
#      OUTSIDE of micromapST
##################################################

# Note that the micromapST package uses the lowess.
# ggplot2 default is loess

# Explore other spans for the smooth to explore other
# structures and relationships.

# Create separate scatterplot with smooth using ggplot
# Annotates the outlying points to compare with 
# linked regions in micromapST

# This is the span used in Figure 3.1

ggplot(PolData, aes(StMean, LQ)) +
  geom_point() +
  geom_smooth(span = 2/3) +
  labs(
    x = "Average Wage - Each State",
    y = "Location Quotient - Employment"
  ) +
  annotate("text", x = 40.82, y = 1.68, label = "DC", size=4) +
  annotate("text", x = 48.15, y = 0.58, label = "WA", size=4) +
  annotate("text", x = 41.01, y = 0.58, label = "OR", size=4)

# Try different values of the span.
# And do the local linear version

# slightly different span
ggplot(PolData, aes(StMean, LQ)) +
  geom_point() +
  geom_smooth(span = 0.75) +
  labs(
    x = "Average Wage - Each State",
    y = "Location Quotient - Employment"
  ) +
  annotate("text", x = 40.82, y = 1.68, label = "DC", size=4) +
  annotate("text", x = 48.15, y = 0.58, label = "WA", size=4) +
  annotate("text", x = 41.01, y = 0.58, label = "OR", size=4)

# Smaller span, so more wiggles
ggplot(PolData, aes(StMean, LQ)) +
  geom_point() +
  geom_smooth(span = 0.5) +
  labs(
    x = "Average Wage - Each State",
    y = "Location Quotient - Employment"
  ) +
  annotate("text", x = 40.82, y = 1.68, label = "DC", size=4) +
  annotate("text", x = 48.15, y = 0.58, label = "WA", size=4) +
  annotate("text", x = 41.01, y = 0.58, label = "OR", size=4)

#######################################################
#   lowess function in base R to match micromapST
#                Figure 3.2
#######################################################

# Try the lowess function in base R. That fits micromapST.

# Did some plotting with different spans. 

plot(PolData$StMean, PolData$LQ, 
     main = "Police & Sheriff Officers",
     xlab = "Average Hourly Wages (Dollars)",
     ylab = "Employment Location Quotient")

# Add the Lowess Smooths with different spans.
lines(lowess(PolData$StMean, PolData$LQ, f = 2/3), col = 'black',lwd = 2)
lines(lowess(PolData$StMean, PolData$LQ, f = .3), col = 'red',lty=2,lwd = 2)

# When copying to clipboard in Plot Window Export menu, used 500 by 375.

# Add a vertical line where the national mean is. 
# Find the y axis limits.
par("usr")
# [1] 19.2952 55.0648  0.5360  1.7240
# Third and fourth are the y axis limits.
lines(c(36.8, 36.8),c(0.5360, 1.7240), col = "green",lty=3, lwd = 2)
