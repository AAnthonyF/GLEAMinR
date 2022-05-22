# Libraries to load. 

library("mvnormtest")
library("car")
library("ggplot2")
library("stats")
library("dplyr")
library("rcompanion")
library("car")
library("effects")
library("multcomp")

# Taking a look at the data
GLEAMorigWKBK

# Taking a look at our data type. 
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
                                    
# Running a t.test on Species and Post farm CO2.
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
#plotNormalHistogram(GLEAMorigWKBK$CommodityRsqrt)
# We have a very nice curve. Let's try log of commodity just to be thorough.

GleamCarbon <- na.omit(GLEAMorigWKBK %>% filter('Production system' %in% c("Aggregated", 
"Grassland systems", "Mixed Systems", "Backyard systems", "Feedlots", "Intermediate systems", "Industrial systems")))

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

MEATnova <- lm(`Postfarm, CO2 (kg CO2e)` ~ `Commodity`, data=GLEAMorigWKBK )
Anova(MEATnova, Type="II", white.adjust=TRUE)
# Another significance code using Commodity and post farm CO2. 

CH4SpecNova <- lm(`Total CH4 emissions (kg CO2e)` ~ `Animal species`, data=GLEAMorigWKBK)
Anova(CH4SpecNova, type="II", white.adjust=TRUE)
# Strong significance code with methane gas total emissions with animal species. 

CH4Specplot <- ggplot(GLEAMorigWKBK, aes(x = `Animal species`, y = `Total CH4 emissions (kg CO2e)`))
CH4Specplot+ geom_boxplot() + ggtitle("Total Methane Emissions by Species") +
xlab("Animal Species") + ylab("Total Methane Emissions")
# Cattle is much higher levels of Methane emissions than the other animal species. 

DirectSpecPlot <- ggplot(GLEAMorigWKBK, aes(x = `Animal species`, y = `Direct energy, CO2 (kg CO2e)`))
DirectSpecPlot+ geom_boxplot() + ggtitle("Direct Energy Emissions by Species") +
xlab("Animal Species") + ylab("Direct energy CO2 emissions")
# Here we see higher emissions with chicken and cattle not so far behind when using
# direct energy emissions of CO2

# Lots of hisograms to check out our data visually. 
plotNormalHistogram(GLEAMorigWKBK$CommodityR)
plotNormalHistogram(GLEAMorigWKBK$`Feed, CO2 (kg CO2e)`)
plotNormalHistogram(GLEAMorigWKBK$`Postfarm, CO2 (kg CO2e)`)
plotNormalHistogram(GLEAMorigWKBK$`Total GHG emissions (kg CO2e)`)
plotNormalHistogram(GLEAMorigWKBK$`Total CO2 emissions (kg CO2e)`)
plotNormalHistogram(GLEAMorigWKBK$`Total N2O emissions (kg CO2e)`)
plotNormalHistogram(GLEAMorigWKBK$`Feed, CH4 (kg CO2e)`)
plotNormalHistogram(GLEAMorigWKBK$`Feed: fertilizer & crop residues, N2O (kg CO2e)`)
plotNormalHistogram(GLEAMorigWKBK$`Feed: applied & deposited manure, N2O (kg CO2e)`)
plotNormalHistogram(GLEAMorigWKBK$`Enteric fermentation, CH4 (kg CO2e)`)
plotNormalHistogram(GLEAMorigWKBK$`Manure management, CH4 (kg CO2e)`)
plotNormalHistogram(GLEAMorigWKBK$`Manure management, N2O (kg CO2e)`)
# The distribution is quite similar between all of the variables

SPECIESnova <- lm(`Postfarm, CO2 (kg CO2e)` ~ `Animal species`, data=GLEAMorigWKBK)
Anova(SPECIESnova, Type="II", white.adjust=TRUE)
# We have a very significant outcome with the post farm CO2 and species columns! 

SPECIESplot <- ggplot(GLEAMorigWKBK, aes(x =`Animal species` , y = `Postfarm, CO2 (kg CO2e)` ))
SPECIESplot+ geom_boxplot() + ggtitle("Post farm CO2 by Animal Species") +
xlab("Animal Species") + ylab("Post Farm CO2 Emissions")
# Cattle show to have the highest level of post farm CO2 emissions. Let's
# check out other emissions against animal species.

COMMODITYnova <- lm(`Postfarm, CO2 (kg CO2e)` ~ Commodity, data=GLEAMorigWKBK)
Anova(COMMODITYnova, Type="II", white.adjust=TRUE)
# We have a significance code. This is not as significant as Post farm CO2 with
# animal species. 

CommodityPlot <- ggplot(GLEAMorigWKBK, aes(x = Commodity , y = `Postfarm, CO2 (kg CO2e)`))
CommodityPlot+ geom_boxplot() + ggtitle("Post farm CO2 by Commodity") +
xlab("Commodity Type") + ylab("Post Farm CO2 Emissions")

CommodityPlot2 <- ggplot(GLEAMorigWKBK, aes(x = Commodity , y = `Postfarm, CO2 (kg CO2e)`))
CommodityPlot2+ geom_col() + ggtitle("Post farm CO2 by Commodity") +
xlab("Commodity Type") + ylab("Post Farm CO2 Emissions")

# Does the commodity affect the pst farm and direct energy CO2 emissions?
plot(GLEAMorigWKBK$`Postfarm, CO2 (kg CO2e)` ~ GLEAMorigWKBK$`Production (kg protein)`, data=GLEAMorigWKBK)
plot(GLEAMorigWKBK$`Direct energy, CO2 (kg CO2e)` ~ GLEAMorigWKBK$`Production (kg protein)`, data=GLEAMorigWKBK)

PostbyProtein.lm <- lm(`Postfarm, CO2 (kg CO2e)` ~ `Production (kg protein)`, data=GLEAMorigWKBK)
summary(PostbyProtein.lm)

DirectbyProtein.lm <- lm(`Direct energy, CO2 (kg CO2e)` ~ `Production (kg protein)`, data=GLEAMorigWKBK)
summary(DirectbyProtein.lm)

SpecProEm.lm<- lm(`Total GHG emissions (kg CO2e)` ~ `Animal speciesR` + `Production (kg protein)`, data = GLEAMorigWKBK )
summary(SpecProEm.lm)
# There are significant correlations with the multiple variables. 

SpecProEm2.lm<- lm(`Total GHG emissions (kg CO2e)` ~ `Animal species` + `Production (kg protein)`, data = GLEAMorigWKBK )
summary(SpecProEm2.lm)

pairwise.t.test(GLEAMorigWKBK$`Production (kg protein)`, GLEAMorigWKBK$`Postfarm, CO2 (kg CO2e)`)

leveneTest(`Direct energy, CO2 (kg CO2e)`~Commodity, data=GLEAMorigWKBK)

# Aggregated commodity was left out of this new column as it is redundant information. 
GLEAMorigWKBK$'CleanComm' <- NA
GLEAMorigWKBK$'CleanComm'[GLEAMorigWKBK$'Commodity' =='Meat'] <-0
GLEAMorigWKBK$'CleanComm'[GLEAMorigWKBK$'Commodity' =='Milk'] <-1
GLEAMorigWKBK$'CleanComm'[GLEAMorigWKBK$'Commodity' =='Eggs'] <-2

CommMan <- manova(cbind(GLEAMorigWKBK$`Postfarm, CO2 (kg CO2e)`, GLEAMorigWKBK$`Direct energy, CO2 (kg CO2e)` ) ~ 
CleanComm, data = GLEAMorigWKBK)
summary(CommMan)
summary.aov(CommMan)
summary.aov(CommMan, test ="wilks")