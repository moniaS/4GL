######### PRZYGOTOWANIE DANYCH ##########

#wczytanie danych
data <- read.csv('C:/Users/Monia/Documents/Studia/1 semestr/4GL/Zadanie1/dane.csv', header = FALSE, sep = ' ')
data <- data[4: 5]

#przypisanie nazw kolumnom
names(data) <- c("egz_powiaz", "egz_przedmiot")
set.seed(20)

######## ZALADOWANIE PAKIETOW ##########
install.packages("dbscan")
library(dbscan)

######## ALGORYTM K-SREDNICH ###########
cluster.kmeans <- kmeans(data, 4)
cluster.kmeans
plot(data, pch = 19, col = cluster.kmeans$cluster)

######## ALGORYTM DBSCAN ##############
kNNdistplot(data)
cluster.dbcan <- dbscan(data, eps = 0.1, minPts = 5)
cluster.dbcan
plot(data, pch = 19, col = cluster.dbcan$cluster + 1L)
