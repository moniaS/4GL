######### PRZYGOTOWANIE DANYCH ##########

#wczytanie danych
data <- read.csv('C:/Users/Monia/Documents/Studia/1 semestr/4GL/Zadanie2/dane.csv', header = FALSE, sep = ' ')
#data <- read.csv('D:/Studia - prace/2-stopien/4GL/repository/4GL/Zadanie2/dane.csv', header = FALSE, sep = ' ')

data <- data[1: 5]
#przypisanie nazw kolumnom
names(data) <- c("nauka_przedmiot", "l_powtorzen", "nauka_powiaz", "egz_powiaz", "egz_przedmiot")

######## ZALADOWANIE PAKIETOW ##########

######## ALGORYTM K-SREDNICH ###########
set.seed(20)
cluster.kmeans <- kmeans(data, 4)
cluster.kmeans
plot(data, pch = 19, col = cluster.kmeans$cluster)
cluster.kmeans.groups <- cluster.kmeans$cluster