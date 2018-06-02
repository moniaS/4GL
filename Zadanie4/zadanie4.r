######## PRZYGOTOWANIE DANYCH ##########
#zbiór danych ze strony 
#https://datahub.io/core/natural-gas#resource-natural-gas-monthly
#www.quandl.com/api/v3/datasets/RATEINF/INFLATION_USA.csv?api_key=UWs93ZbpqbCoD68gg6zx


#wczytanie danych
#data <- read.csv('C:/Users/Monia/Documents/Studia/1 semestr/4GL/Zadanie4/dane.csv', header = TRUE, sep = ',')
data.gaz <- read.csv('D:/Studia - prace/2-stopien/4GL/repository/4GL/Zadanie4/gaz.csv', header = TRUE, sep = ',')
data.inflacja <- read.csv('D:/Studia - prace/2-stopien/4GL/repository/4GL/Zadanie4/inflacja.csv', header = TRUE, sep = ',')

data.gaz$Price <- as.numeric(as.character(data.gaz$Price))
data.gaz$Date <- as.Date(as.character(data.gaz$Date))
data.gaz.timeseries <- as.xts(data.gaz[,-1], order.by = data.gaz$Date)

data.inflacja <- data.inflacja[1:200,]
data.inflacja$Value <- as.numeric(as.character(data.inflacja$Value))
data.inflacja$Date <- as.Date(as.character(data.inflacja$Date))
data.inflacja.timeseries <- as.xts(data.inflacja[,-1], order.by = data.inflacja$Date)

######## ZA£ADOWANIE PAKIETÓW ##########

#za³adowanie biblioteki do grupowania
install.packages("dtwclust")
library(dtwclust)

#za³adowanie biblioteki do time series
install.packages("xts")
library(xts)

######## GRUPOWANIE ZBIORU NR 1 ##########

data.gaz.clust <- tsclust(data.gaz.timeseries, k = 4, type = "hierarchical", distance = "DTW",
                                   seed = 3247)

plot(data.gaz.clust@cluster)

plot(data.gaz.clust, clus = seq_len(x@k), plot = TRUE, type = "dendrogram")

######## GRUPOWANIE ZBIORU NR 2 ##########

data.inflacja.clust <- tsclust(data.inflacja.timeseries, k = 3, type = "hierarchical", distance = "DTW",
                          seed = 3247)

plot(data.inflacja.clust@cluster)

plot(data.inflacja.clust, clus = seq_len(x@k), plot = TRUE, type = "dendrogram")


