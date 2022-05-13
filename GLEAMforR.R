library("mvnormtest")
library("car")
library("ggplot2")
library("stats")
library("dplyr")
library("rcompanion")
library("car")


str(GLEAMorigWKBK$`Postfarm, CO2 (kg CO2e)`)

# Transforming out string data into integers.Region, Animal Species, Commodity, and 
# Production are the columns to be changed. 

GLEAMorigWKBK$RegionR <- NA
GLEAMorigWKBK$'RegionR'[GLEAMorigWKBK$Region == 'Global'] <- 0
GLEAMorigWKBK$'RegionR'[GLEAMorigWKBK$Region =='East Asia and Southeast Asia'] <- 1
GLEAMorigWKBK$'RegionR'[GLEAMorigWKBK$Region =='Eastern Europe'] <- 2
GLEAMorigWKBK$'RegionR'[GLEAMorigWKBK$Region =='Latin America and the Caribbean'] <- 3
GLEAMorigWKBK$'RegionR'[GLEAMorigWKBK$Region =='Near East and North Africa'] <- 4
GLEAMorigWKBK$'RegionR'[GLEAMorigWKBK$Region =='North America'] <- 5
GLEAMorigWKBK$'RegionR'[GLEAMorigWKBK$Region =='Oceania'] <- 6
GLEAMorigWKBK$'RegionR'[GLEAMorigWKBK$Region =='Russian Federation'] <- 7
GLEAMorigWKBK$'RegionR'[GLEAMorigWKBK$Region =='South Asia'] <- 8
GLEAMorigWKBK$'RegionR'[GLEAMorigWKBK$Region =='Sub-Saharan Africa'] <- 9
GLEAMorigWKBK$'RegionR'[GLEAMorigWKBK$Region =='Western Europe'] <- 10

GLEAMorigWKBK$`Animal speciesR` <- NA
GLEAMorigWKBK$`Animal speciesR`[GLEAMorigWKBK$`Animal species` =='Cattle'] <- 0
GLEAMorigWKBK$`Animal speciesR`[GLEAMorigWKBK$`Animal species` =='Chicken'] <- 1
GLEAMorigWKBK$`Animal speciesR`[GLEAMorigWKBK$`Animal species` =='Buffaloes'] <- 2
GLEAMorigWKBK$`Animal speciesR`[GLEAMorigWKBK$`Animal species` =='Sheep'] <- 3
GLEAMorigWKBK$`Animal speciesR`[GLEAMorigWKBK$`Animal species` =='Goats'] <- 4
GLEAMorigWKBK$`Animal speciesR`[GLEAMorigWKBK$`Animal species` =='Pigs'] <- 5

GLEAMorigWKBK$'CommodityR' <- NA
GLEAMorigWKBK$'CommodityR'[GLEAMorigWKBK$'Commodity' =='Meat'] <-0
GLEAMorigWKBK$'CommodityR'[GLEAMorigWKBK$'Commodity' =='Aggregated'] <-1
GLEAMorigWKBK$'CommodityR'[GLEAMorigWKBK$'Commodity' =='Milk'] <-2
GLEAMorigWKBK$'CommodityR'[GLEAMorigWKBK$'Commodity' =='Eggs'] <-3

GLEAMorigWKBK$`Production systemR` <- NA
GLEAMorigWKBK$`Production systemR`[GLEAMorigWKBK$`Production system` == 'Aggregated'] <-0
GLEAMorigWKBK$`Production systemR`[GLEAMorigWKBK$`Production system` == 'Grassland systems'] <-1
GLEAMorigWKBK$`Production systemR`[GLEAMorigWKBK$`Production system` == 'Mixed systems'] <-2
GLEAMorigWKBK$`Production systemR`[GLEAMorigWKBK$`Production system` == 'Backyard systems'] <-3
GLEAMorigWKBK$`Production systemR`[GLEAMorigWKBK$`Production system` == 'Layers'] <-4
GLEAMorigWKBK$`Production systemR`[GLEAMorigWKBK$`Production system` == 'Feedlots'] <-5
GLEAMorigWKBK$`Production systemR`[GLEAMorigWKBK$`Production system` == 'Broilers'] <-6
GLEAMorigWKBK$`Production systemR`[GLEAMorigWKBK$`Production system` == 'Intermediate systems'] <-7
GLEAMorigWKBK$`Production systemR`[GLEAMorigWKBK$`Production system` == 'Industrial systems'] <-8

# Our new columns are at the end of the original columns and have usable integers                                     
                                    
# Running a t.test on Species and Postfarm CO2.
SpeciesT <- t.test(GLEAMorigWKBK$`Animal speciesR`, GLEAMorigWKBK$`Postfarm, CO2 (kg CO2e)`, paired = TRUE)
SpeciesT
# Our p value is much less than 0.05. We have no correlation between these two. 

# Testing to see what else may have a correlation. 
GLEAMkeep <- c("Animal speciesR", "Postfarm, CO2 (kg CO2e)")
GLEAM1 <- GLEAMorigWKBK[GLEAMkeep]
GLEAM2 <- GLEAM1[1:5000]
GLEAM2 <- as.matrix(GLEAM1)

GLEAM3 <- na.omit(GLEAM2)

mshapiro.test(t(GLEAM3))
# Error in solve.default received. Our test is not useful

GLEAMKeep2 <- c('Postfarm, CO2 (kg CO2e)', "CommodityR")
GLEAM4 <- GLEAMorigWKBK[GLEAMKeep2]
GLEAM5 <- GLEAM4[1:5000]
GLEAM6 <- as.matrix(GLEAM4)
GLEAM7 <- na.omit(GLEAM4)
mshapiro.test(t(GLEAM7))
str(GLEAM4)
# Same story for this test

GLEAMKeep3 <- c(CommodityR,`Emission Intensity (kg CO2e per kg protein)` )
GLEAM8 <- GLEAMorigWKBK[GLEAMKeep3]
GLEAM9 <- as.matrix(GLEAM8)
GLEAM10 <- na.omit(GLEAM9)
mshapiro.test(t(GLEAM10))
# And once again, error. 

t.test(GLEAMorigWKBK$`Production (kg protein)`, GLEAMorigWKBK$`Postfarm, CO2 (kg CO2e)`)
# We have a p-value far smaller than 0.05 again. Also the alternative hypothesis line is 
# not equal to 0 

t.test(GLEAMorigWKBK$`Production (kg protein)`, GLEAMorigWKBK$`Total GHG emissions (kg CO2e)`)
# More of the same with an even smaller p-value. 

GleamPlot <- ggplot(GLEAMorigWKBK, aes(x = `Animal species`, y =`Postfarm, CO2 (kg CO2e)` ))
GleamPlot + geom_point() + ggtitle("Differences between Species (Post-farm Emissions)") +
xlab("Species of Livestock") + ylab("Postfarm emissions (CO2)")
# Cattle seems to have the highest emissions that come from post farm movement of 
# the commodity 

GleamPlot1 <- ggplot(GLEAMorigWKBK, aes(x = Commodity, y = `Feed, CO2 (kg CO2e)` ))
GleamPlot1 + geom_point() + ggtitle("CO2 emissions by Commodity") +
xlab("Commodity") + ylab("CO2 emissions")
# Aggregated commodity data shows that it had the highest CO2 emissions. Meat is 
# next highest with milk and eggs following, respectively. Only one animal
# producing eggs does make sense with the data. 

GleamPlot2 <- ggplot(GLEAMorigWKBK, aes(x = `Total CO2 emissions (kg CO2e)`, y = `Production system` ))
GleamPlot2 + geom_point() + ggtitle("Total emissions by Production system") +
xlab("Total CO2 Emissions") + ylab("Production")
# The production system was put on the y-axis so it is easy to read. 

ggplot(GLEAMorigWKBK, aes(x=`Postfarm, CO2 (kg CO2e)`)) + geom_histogram(bins = 100)
# Taking a look at our Postfarm CO2 column, adjusted. We can see some outliars at
# the far right. There appears to be a nice curve in the data with this histogram

plotNormalHistogram(GLEAMorigWKBK$CommodityR)
# We have a positive skew to our commodity. Let's see how the square root of commodity
# lends itself to distribution

GLEAMorigWKBK$CommodityRsqrt <- sqrt(GLEAMorigWKBK$CommodityR)
plotNormalHistogram(GLEAMorigWKBK$CommodityRsqrt)
# We have a very nice curve. Let's try log of commodity just to be thorough.

GLEAMorigWKBK$CommodityRlog <- log(GLEAMorigWKBK$CommodityR)
plotNormalHistogram(GLEAMorigWKBK$CommodityRlog)
# No help from log of commodity.
GLEAMorigWKBK$RegionR
  
GleamCarbon <- na.omit(GLEAMorigWKBK %>% filter('Production system' %in% c("Aggregated", 
"Grassland systems", "Mixed Systems", "Backyard systems", "Feedlots", "Intermediate systems", "Industrial systems")))

apps <- na.omit(NEWgoogleplaystore %>% filter(Category %in% c("BEAUTY",
                                    "FOOD_AND_DRINK", "PHOTOGRAPHY")))

plotNormalHistogram(GLEAMorigWKBK$`Production system`)
bartlett.test(`Postfarm, CO2 (kg CO2e)` ~ `Production system`, data=GLEAMorigWKBK )
bartlett.test(`Postfarm, CO2 (kg CO2e)` ~ `Commodity`, data=GLEAMorigWKBK )
bartlett.test(`Postfarm, CO2 (kg CO2e)` ~ `Animal species`, data=GLEAMorigWKBK )
bartlett.test(`Postfarm, CO2 (kg CO2e)` ~ RegionR, data=GLEAMorigWKBK )
# All of our categorical columns have violated the assumption of homogeneity

fligner.test(`Postfarm, CO2 (kg CO2e)` ~ `Production system`, data=GLEAMorigWKBK )
fligner.test(`Postfarm, CO2 (kg CO2e)` ~ `Commodity`, data=GLEAMorigWKBK )
fligner.test(`Postfarm, CO2 (kg CO2e)` ~ `Animal species`, data=GLEAMorigWKBK )
fligner.test(`Postfarm, CO2 (kg CO2e)` ~ RegionR, data=GLEAMorigWKBK )
# Again, all columns have violated the assumption of homogeneity

# Time for ANOVAs. Hopefully find something significant. 
GLEAMnova <- lm(`Postfarm, CO2 (kg CO2e)` ~ `Production system`, data=GLEAMorigWKBK)
Anova(GLEAMnova, Type="II", white.adjust=TRUE)
# We have a significance code. There are significant differences between the 
# productions systems and post-farm CO2 emission. However, lets do some correction
# to double check our finding. 

pairwise.t.test(GLEAMorigWKBK$`Postfarm, CO2 (kg CO2e)`, GLEAMorigWKBK$`Production system`, p.adjust="bonferroni", pool.sd = FALSE)
# We have some significant differences with the pairs meat and milk as well as meat and aggregated. Lets get the means of the
# matrix. 

ProdMeans <- GLEAMorigWKBK %>% group_by(`Production system`) %>% summarize(MEANS = mean(`Postfarm, CO2 (kg CO2e)`))
CommodityMeans <- GLEAMorigWKBK %>% group_by('Animal Species') %>% summarize(MEANs = mean(`Postfarm, CO2 (kg CO2e)`))
