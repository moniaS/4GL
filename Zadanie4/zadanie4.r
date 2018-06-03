######## PRZYGOTOWANIE DANYCH ##########
#zbiór danych ze strony 
#https://datahub.io/core/natural-gas#resource-natural-gas-monthly
#www.quandl.com/api/v3/datasets/RATEINF/INFLATION_USA.csv?api_key=UWs93ZbpqbCoD68gg6zx

######## ZA£ADOWANIE PAKIETÓW ##########

#za³adowanie biblioteki do grupowania
install.packages("dtwclust")
library(dtwclust)

#za³adowanie biblioteki do time series
install.packages("xts")
library(xts)

#za³adowanie biblioteki do Twitter’s AnomalyDetection
install.packages("devtools")
devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)

#za³adowanie biblioteki do tsoutliers
install.packages("tsoutliers")
library(tsoutliers)

#za³adowanie biblioteki hdrcde
install.packages("hdrcde")
library(hdrcde)

########### WCZYTANIE DANYCH ############

#data <- read.csv('C:/Users/Monia/Documents/Studia/1 semestr/4GL/Zadanie4/dane.csv', header = TRUE, sep = ',')
data.gaz <- read.csv('D:/Studia - prace/2-stopien/4GL/repository/4GL/Zadanie4/gaz.csv', header = TRUE, sep = ',')
data.inflacja <- read.csv('D:/Studia - prace/2-stopien/4GL/repository/4GL/Zadanie4/inflacja.csv', header = TRUE, sep = ',')

####### PRZYGOTOWANIE DANYCH ############

data.gaz$Price <- as.numeric(as.character(data.gaz$Price))
data.gaz$Date <- as.Date(as.character(data.gaz$Date))
data.gaz.timeseries <- as.xts(data.gaz[,-1], order.by = data.gaz$Date)

data.inflacja <- data.inflacja[1:300,]
data.inflacja$Value <- as.numeric(as.character(data.inflacja$Value))
data.inflacja$Date <- as.Date(as.character(data.inflacja$Date))
data.inflacja.timeseries <- as.xts(data.inflacja[,-1], order.by = data.inflacja$Date)

######## GRUPOWANIE ZBIORU NR 1 ##########

data.gaz.clust <- tsclust(data.gaz.timeseries, k = 4, type = "hierarchical", distance = "DTW",
                                   seed = 3247)
plot(data.gaz.timeseries, xlab="Data", ylab="Cena")

plot(x = data.gaz$Date, y = data.gaz.clust@cldist, xlab="Data", ylab="Odleg³oœæ")

plot(data.gaz, pch = 19, col = data.gaz.clust@cluster, xlab="Data", ylab="Cena")

plot(data.gaz.clust, clus = seq_len(x@k), plot = TRUE, type = "dendrogram")

######## GRUPOWANIE ZBIORU NR 2 ##########

data.inflacja.clust <- tsclust(data.inflacja.timeseries, k = 5, type = "hierarchical", distance = "DTW",
                          seed = 3247)

plot(data.inflacja.timeseries, xlab="Data", ylab="Wartoœæ", main="Inflacja")

plot(x = data.inflacja$Date, y = data.inflacja.clust@cldist, xlab="Data", ylab="Odleg³oœæ")

plot(data.inflacja, pch = 19, col = data.inflacja.clust@cluster, xlab="Data", ylab="Wartoœæ")

plot(data.inflacja.clust, clus = seq_len(x@k), plot = TRUE, type = "dendrogram")


######## Twitter’s AnomalyDetection ###########

#przyklad dla zbioru danych cen gazu 
data.gaz <- read.csv('D:/Studia - prace/2-stopien/4GL/repository/4GL/Zadanie4/gaz-dziennie.csv', header = TRUE, sep = ',')
data.gaz$Date <- as.POSIXct(strptime(data.gaz$Date, "%Y-%m-%d", tz = "UTC"))
#wybor przedzialu danych dla bardziej przejrzystego wyniku
data.gaz <- data.gaz[900:1100,]
anomalie.gaz <- AnomalyDetectionTs(data.gaz, max_anoms=0.05, direction='both', plot=TRUE, ylab="Price", xlab="Date")
anomalie.gaz

#kolejny przyklad dla innego zbioru danych
data.inflacja <- read.csv('D:/Studia - prace/2-stopien/4GL/repository/4GL/Zadanie4/inflacja.csv', header = TRUE, sep = ',')
data.inflacja$Date <- as.POSIXct(strptime(data.inflacja$Date, "%Y-%m-%d", tz = "UTC"))
anomalie.inflacja <- AnomalyDetectionTs(data.inflacja, max_anoms=0.05, direction='both', plot=TRUE, ylab="Values", xlab="Date")
anomalie.inflacja

############## TsOutliers ###############

gaz.ts <- ts(data.gaz[,2], frequency=1)
gaz.outliers <- tso(gaz.ts)
plot(gaz.outliers)

############### Hdrcde ##################

#data w data.gaz musi byc uprzednio sformatowana
cde.gaz <- cde(data.gaz$Date,data.gaz$Price, x.name="Date", y.name="Price")
plot(cde.gaz, plot.fn="hdr")
