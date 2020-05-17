


################################  Libraries ################################# 

library(dplyr)
library(tibble)
library(CORElearn)
library(ggplot2)










################################  Read Dataset ################################# 

whiteWineURL <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
whiteCleanData <- read.csv(whiteWineURL, header = TRUE, sep = ";")


redWineURL <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
redCleanData <- read.csv(redWineURL, header = TRUE, sep = ";")


# Add color attribute to distinguish datasets
redCleanData['color'] <- 'red.wine'
whiteCleanData['color'] <- 'white.wine'


redData <- redCleanData
whiteData <- whiteCleanData


nrow(redCleanData)
ncol(redCleanData)

nrow(whiteCleanData)
ncol(whiteCleanData)





################################  Data Preprocessing ################################# 


# Check if there are duplicated rows
any(duplicated(whiteCleanData))
any(duplicated(redCleanData))

# Remove duplicated rows considering all columns
whiteCleanData <- whiteCleanData[!duplicated(whiteCleanData),]
redCleanData <- redCleanData[!duplicated(redCleanData),]



# Overview
nrow(whiteCleanData)
nrow(redCleanData)


# Check for unknown variables
any(is.na(whiteCleanData))
any(is.na(redCleanData))


# Merge wine datasets
mergedCleanData <- rbind(redCleanData, whiteCleanData)

colnames(mergedCleanData)

# Show the types of data
sapply(mergedCleanData, class)



mergedData <- mergedCleanData
sapply(mergedData, class)


# Cut function needs numeric value, therefore cast quality as numeric
mergedData$quality <- as.numeric(mergedData$quality)

# Label wine quality 
mergedData$quality <- cut(mergedData$quality, breaks=c(3, 5, 7, Inf), labels=c("Poor", "Good", "Excellent"), include.lowest = TRUE)




#################################  Explotary Data Analysis ################################# 







# If Standart Deviation and IQR gives different values 
# it indicates thjere may be outliers exist

# Standart Deviation
mergedData %>% 
  select(fixed.acidity : alcohol) %>% 
  summarise_all(list(sd = sd))

# IQR
mergedData %>% 
  select(fixed.acidity : alcohol) %>% 
  summarise_all(list(IQR = IQR))


# Mean
mergedData %>% 
  select(fixed.acidity : alcohol) %>% 
  summarise_all(list(mn = mean))


# Median
mergedData %>% 
  select(fixed.acidity : alcohol) %>% 
  summarise_all(list(md = median)) 







# Quality 0 is very bad, 10 is very excellent
ggplot(data = mergedCleanData, aes(x = quality)) +
  geom_bar(fill="steelblue", width=0.5) +
  ggtitle("Number Of Samples According to Quality") +
  labs(x = "Quality", y = "Count") + 
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2)) +
  facet_wrap( ~color) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = unique(mergedCleanData$quality))

histogramPlot <- function(data, attribute, binWidth, xLabel, title) 
{
  
  ggplot(data, aes(x=attribute)) + 
    geom_histogram(binwidth = binWidth, color="black", fill="steelblue", linetype="dashed") +
    ggtitle(title) +
    labs(x = xLabel, y = "Count") + 
    facet_wrap( ~color) + 
    theme(plot.title = element_text(hjust = 0.5))
}


histogramByLogScalePlot <- function(data, attribute, binWidth, xLabel, title) 
{
  
  ggplot(data, aes(x=attribute)) + 
    geom_histogram(binwidth = binWidth, color="black", fill="steelblue", linetype="dashed") +
    scale_x_log10(lim = c(min(attribute), 1.00370),  breaks = seq(min(data$density), 1.00370, 0.002))+
    ggtitle(title) +
    labs(x = xLabel, y = "Count") + 
    facet_wrap( ~color) + 
    theme(plot.title = element_text(hjust = 0.5))
  
}
histogramPlotLog(mergedCleanData, mergedCleanData$density, 0.0002, "Density", "Histogram of Density")


histogramPlot(mergedCleanData, mergedCleanData$fixed.acidity, 0.5, "Fixed Acidity", "Histogram of Fixed Acidity")
histogramPlot(mergedCleanData, mergedCleanData$volatile.acidity, 0.05, "Volatile Acidity", "Histogram of Volatile Acidity")
histogramPlot(mergedCleanData, mergedCleanData$citric.acid, 0.05, "Citric Acidity", "Histogram of Citric Acidity")
histogramPlot(mergedCleanData, mergedCleanData$residual.sugar, 2.5, "Residual Sugar","Histogram of Residual Sugar")
histogramPlot(mergedCleanData, mergedCleanData$chlorides, 0.025, "Chlorides", "Histogram of Chlorides")
histogramPlot(mergedCleanData, mergedCleanData$free.sulfur.dioxide, 10.0, "Free Sulfur Dioxide","Histogram of Free Sulfur Dioxide")
histogramPlot(mergedCleanData, mergedCleanData$total.sulfur.dioxide, 10.0, "Total Sulfur Dioxide", "Histogram of Total Sulfur Dioxide")
histogramPlot(mergedCleanData, mergedCleanData$density, 0.001, "Density", "Histogram of Density")
histogramPlot(mergedCleanData, mergedCleanData$pH, 0.05, "pH","Histogram of pH")
histogramPlot(mergedCleanData, mergedCleanData$sulphates, 0.05, "Sulphates","Histogram of Sulphates")
histogramPlot(mergedCleanData, mergedCleanData$alcohol, 0.5, "Alcohol","Histogram of Alcohol")








# IQR = Q3 - Q1
# upper whisker = Q3 + 1.5*IQR
# lower whisker = Q1 - 1.5*IQR
attibutesOverQualityBoxplot <- function (attribute, title) 
{
  
  ggplot(mergedCleanData, aes(x = factor(quality), y = attribute)) + 
    stat_boxplot(geom ='errorbar') + 
    labs(x = "Quality", y = "Value") + 
    ggtitle(title) +
    geom_jitter(size = 1.5, alpha = 0.1)  +
    geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
    theme(plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~color)
}


attibutesOverQualityBoxplot(mergedCleanData$fixed.acidity, "Fixed Acidity Boxplot")
attibutesOverQualityBoxplot(mergedCleanData$volatile.acidity, "Volatile Acidity Boxplot")
attibutesOverQualityBoxplot(mergedCleanData$citric.acid, "Citric Acidity Boxplot")
attibutesOverQualityBoxplot(mergedCleanData$residual.sugar, "Residual Sugar Boxplot")
attibutesOverQualityBoxplot(mergedCleanData$chlorides, "Chlorides Boxplot")
attibutesOverQualityBoxplot(mergedCleanData$free.sulfur.dioxide, "Free Sulfur Dioxide Boxplot")
attibutesOverQualityBoxplot(mergedCleanData$total.sulfur.dioxide, "Total Sulfur Dioxide Boxplot")
attibutesOverQualityBoxplot(mergedCleanData$density, "Density Boxplot")
attibutesOverQualityBoxplot(mergedCleanData$pH, "pH Boxplot")
attibutesOverQualityBoxplot(mergedCleanData$sulphates, "Sulphates Boxplot")
attibutesOverQualityBoxplot(mergedCleanData$alcohol, "Alcohol Boxplot")






#################################  References  ################################# 

#
#
#
#









 


















