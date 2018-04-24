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
install.packages("cluster")
library(cluster)

######## ALGORYTM K-SREDNICH ###########
cluster.kmeans <- kmeans(data, 4)
cluster.kmeans
plot(data, pch = 19, col = cluster.kmeans$cluster)

######## ALGORYTM DBSCAN ##############
kNNdistplot(data)
cluster.dbcan <- dbscan(data, eps = 0.1, minPts = 5)
cluster.dbcan
plot(data, pch = 19, col = cluster.dbcan$cluster + 1L)

######## ALGORYTM HIERARCHICZNY 
#inne metody: average, single, complete
cluster.agnes = agnes(data, method="average")
plot(cluster.agnes, main="Dendogram")

#wyœwietlanie danych jako drzewo z podzia³em na grupy
groups<-cutree(cluster.agnes, k=4)
rect.hclust(cluster.agnes, k=4, border="red")

#wyœwietlenie ilosciowego podzialu na grupy
numberOfElements <- table(groups)

#prezentacja danych pogrupowanych na wykresie
nazwa <- rownames(data)
group = factor(cutree(cluster.agnes, k=4))
ggplot(data, aes(egz_powiaz, egz_przedmiot, label=nazwa, color=group)) + geom_point(size=3) + theme_bw() + coord_trans("sqrt", "sqrt")