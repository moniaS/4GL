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

######## ZA£ADOWANIE PAKIETÓW ##########

#za³adowanie biblioteki do grupowania
install.packages("dtwclust")
library(dtwclust)

#za³adowanie biblioteki do time series
install.packages("xts")
library(xts)

######## GRUPOWANIE ##########

data.gaz.clust <- tsclust(data.gaz.timeseries, type = "partitional", k = 10L, 
              distance = "euclidean", centroid = "pam", seed = 3247L, trace = TRUE)

plot(data.gaz.clust)
