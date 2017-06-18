#IBEX 35 Companies
IBEX35 <- c("ABE.MC", "ACS.MC", "ACX.MC", "AENA.MC", "AMS.MC", "ANA.MC", "BBVA.MC", "BKIA.MC",
            "BKT.MC", "CABK.MC", "CLNX.MC", "DIA.MC", "ELE.MC", "ENG.MC", "FCC.MC", "FER.MC", 
            "GAM.MC", "GAS.MC", "GRF.MC", "IAG.MC", "IBE.MC", "IDR.MC", "ITX.MC", "MAP.MC", 
            "MEL.MC", "MRL.MC", "MTS.MC", "POP.MC", "REE.MC", "SAB.MC", "SAN.MC", "TEF.MC",
            "TL5.MC", "TRE.MC", "VIS.MC")

setwd("Downloads/DATA/")

IBEX35_data <- data.frame(matrix(NA, nrow = 1, ncol = 8))
names(IBEX35_data) <- c("Symbol", "Date", "Open", "High", "Low", "Close", "Adj.Close", "Volume")


#Load Data and format it
library("DMwR")
for(i in seq_along(1:length(IBEX35))){
  assign(IBEX35[i], read.csv(paste(IBEX35[i], ".csv", sep = "")))
  temp <- data.frame(Symbol = rep(IBEX35[i], nrow(eval(as.symbol(IBEX35[i])))), eval(as.symbol(IBEX35[i])))
  #We consider null values as missings
  temp[apply(temp, 2, function(x) x=="null")] <- NA
  temp[,3:8] <- apply(temp[,3:8], 2, as.numeric)
  #We impute the missing values with the closer ones (those values are minority)
  temp <- knnImputation(temp, k = 3, scale = F) 
  IBEX35_data <- rbind(IBEX35_data, temp)
  
}
IBEX35_data <- IBEX35_data[-1,]

#Check if there is any missing value after the pre-processing, there should not be any
library(tidyverse)
IBEX1 <- IBEX35_data %>% mutate(Na_number = apply(., 1, function(x) sum(is.na(x))))
Row_Na <- IBEX1 %>% group_by(Na_number) %>% summarize(n = n(), prop = round(n()/47, 3))
ggplot(data = as.data.frame(Row_Na), aes(Na_number, n)) + geom_bar(stat = "identity") + labs(x = "Frequency", y = "Number of missings") + ggtitle("Number of missings per individual") 





#Convert Date var. to Date format and Symbol var. to factor.
IBEX35_data$Date <- as.Date(IBEX35_data$Date,format="%Y-%m-%d")
IBEX35_data$Symbol <- as.factor(IBEX35_data$Symbol)



library("ggthemes")
library(ggplot2)
#Plot the Adjusted Close price for all the different companies to see how does their share value evolves among time
ggplot(IBEX35_data, aes(Date, Adj.Close, group = Symbol, col = Symbol)) + geom_line() + ggtitle("IBEX 35 Adjusted Close price") + theme_economist() + theme(legend.position="bottom")

#Proceed to Scale the data, to do so we take the log period return, we do this because otherwise the different stocks evolutions are not comparable
#since their prices levels differs. Doing this, we only consider the evolution independently of the different price levels of every stock.
library(tidyquant)
IBEX35_Scaled <- IBEX35_data %>% group_by(Symbol) %>% tq_transmute(select     = Adj.Close, 
                                                                   mutate_fun = periodReturn, 
                                                                   period     = "daily", 
                                                                   type = "log",
                                                                   col_rename = "daily.returns") 
#And now we plot again the Scaled data for the 35 companies
ggplot(IBEX35_Scaled, aes(Date, daily.returns, group = Symbol, col = Symbol)) + geom_line() + ggtitle("IBEX 35 Daily returns") +  theme_economist() + theme(legend.position="bottom")

#Now we observe how the evolution on the stock prices for every company related to the others, calculating the Kendall Correlation.
Ibex35_Scaled_Columns <- spread(IBEX35_Scaled, Symbol, daily.returns) 
library(corrplot)
correlations <- cor(Ibex35_Scaled_Columns[,-1], method="kendall")
corrplot(correlations, method="color")
names1 <- rownames(correlations)
rownames(correlations) <- sapply(rownames(correlations),gsub,pattern=".MC",replacement="")
colnames(correlations) <- sapply(colnames(correlations),gsub,pattern=".MC",replacement="")

#Now we calculate the dissimilarity among the different companies and use it as our distance metric for the clustering.
dissimilarity <- 1 - correlations
distance <- as.dist(dissimilarity)
#Proceed to perform a hierarchical clustering taking the distance metric
hierarchical_Clustering <- hclust(distance)
#Observing the aggregated distance at each iteration we consider 7 clusters a good amount of clusters.
barplot((hierarchical_Clustering$height),main="Aggregated distance at each iteration")
#And we proceed to plot the hierarchical clustering tree.
plot(hierarchical_Clustering, main="Dissimilarity", xlab="")
c1 <- cutree(hierarchical_Clustering, 7)
rect.hclust(hierarchical_Clustering, k=7, border="red") 



clusters <- data.frame(Symbol = names(c1) ,cluster = c1)
clusters$Symbol <- names1
clusters$Symbol <- as.factor(clusters$Symbol)


#Plot the scaled stock evolution separating the plots by each cluster
IBEX35_Scaled <- merge(IBEX35_Scaled, clusters)
ggplot(IBEX35_Scaled, aes(Date, daily.returns, group = Symbol, col = Symbol)) + geom_line() + facet_grid(cluster ~.) + ggtitle("Clustering of Daily Returns") + theme_economist() + theme(legend.position="right")


#Plot the stock evolution separating the plots by each cluster
IBEX35_data <- merge(IBEX35_data, clusters)
ggplot(IBEX35_data, aes(Date, Adj.Close, group = Symbol, col = Symbol)) + geom_line() + facet_grid(cluster ~.) + ggtitle("Clustering of Adjusted Close Price") + theme_economist() + theme(legend.position="right")

#Now we will look which are the most similar and most dissimilar companies on the IBEX35
correlations1 <- correlations
diag(correlations1) <- 0
which(correlations1 == max(correlations1), arr.ind = TRUE)
diag(correlations1) <- 1
which(correlations1 == min(correlations1), arr.ind = TRUE)

#Plot the most similar companies (scaled and non scaled metric)
ggplot(IBEX35_Scaled[IBEX35_Scaled$Symbol == "SAN.MC" | IBEX35_Scaled$Symbol == "BBVA.MC",], aes(Date, daily.returns, group = Symbol, col = Symbol)) + geom_line() + ggtitle("Most Similar Stocks") + theme_economist() + theme(legend.position="right")
ggplot(IBEX35_data[IBEX35_data$Symbol == "SAN.MC" | IBEX35_data$Symbol == "BBVA.MC",], aes(Date, Adj.Close, group = Symbol, col = Symbol)) + geom_line() + ggtitle("Most Similar Stocks") + theme_economist() + theme(legend.position="right")

#Plot the most dissimilar companies (scaled and non scaled metric)
ggplot(IBEX35_Scaled[IBEX35_Scaled$Symbol == "POP.MC" | IBEX35_Scaled$Symbol == "ENG.MC",], aes(Date, daily.returns, group = Symbol, col = Symbol)) + geom_line() + ggtitle("Most Dissimilar Stocks") + theme_economist() + theme(legend.position="right")
ggplot(IBEX35_data[IBEX35_data$Symbol == "POP.MC" | IBEX35_data$Symbol == "ENG.MC",], aes(Date, Adj.Close, group = Symbol, col = Symbol)) + geom_line() + ggtitle("Most Dissimilar Stocks") + theme_economist() + theme(legend.position="right")

                                                  
                                                  IBEX35_Scaled2 <- IBEX35_data %>% group_by(Symbol) %>% tq_transmute(select     = Adj.Close, 
                                                                   mutate_fun = periodReturn, 
                                                                   period     = "yearly", 
                                                                   type = "log",
                                                                   col_rename = "yearly.returns") 



return <- IBEX35_Scaled2 %>% group_by(Symbol) %>% summarise(return = mean(yearly.returns) * 100)
return <- merge(return, clusters)
return <- return[order(return$cluster),]
return$cluster <- as.factor(return$cluster)



ggplot(return, aes(Symbol, return, group = cluster, fill = cluster)) + geom_bar(stat = "identity") + facet_wrap(~cluster, scale = "free") + geom_hline(yintercept = 0)

