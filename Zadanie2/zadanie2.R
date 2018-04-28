######### PRZYGOTOWANIE DANYCH ##########

#wczytanie danych
data <- read.csv('dane.csv', header = FALSE, sep = ' ')
data <- data[4: 5]

#przypisanie nazw kolumnom
names(data) <- c("egz_powiaz", "egz_przedmiot")
set.seed(20)

######## ZALADOWANIE PAKIETOW ##########

#za³adowanie biblioteki do algorytmu DBSCAN
install.packages("dbscan")
library(dbscan)

#za³adowanie biblioteki do algorytmu AGNES
install.packages("cluster")
library(cluster)

#za³adowanie biblioteki do algorytmu LOF
install.packages("Rlof")
library(Rlof)

#za³adowanie biblioteki do rysowania wykresu
install.packages("ggplot2")
library(ggplot2)

######## ALGORYTM K-SREDNICH ###########
cluster.kmeans <- kmeans(data, 8)
cluster.kmeans
plot(data, pch = 19, col = cluster.kmeans$cluster)

######## ALGORYTM DBSCAN ##############
kNNdistplot(data, k=4)
cluster.dbcan <- dbscan(data, eps = 0.095, minPts = 4)
cluster.dbcan
plot(data, pch = 19, col = cluster.dbcan$cluster + 1L)

######## ALGORYTM HIERARCHICZNY #######
#inne metody: average, single, complete
cluster.agnes = agnes(data, method="complete")
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

######## DODANIE NIEPASUJ¥CYCH REKORDÓW #######

data <- rbind(data, c(2, 8))
data <- rbind(data, c(7, 7))
data <- rbind(data, c(4, 7))
data <- rbind(data, c(8, 3))
data <- rbind(data, c(8, 5))

######## ALGORYTM LOF #######

data$egz_powiaz <- as.numeric(data$egz_powiaz)
data$egz_przedmiot <- as.numeric(data$egz_przedmiot)

lof.results <- lof(data, 9)

data.frame(lof.results)

plot(lof.results, pch=19, xlab="Indeks", ylab="Wyniki algorytmu lof", col="blue")

######## ALGORYTM COF #######

cof <- function (data, k) {
  dist.eucl <- dist(data, method = "euclidean")
  dist.eucl <- round(as.matrix(dist.eucl), 2)
  suma_odl_do_sasiadow <- 0
  suma_odl_do_sasiadow_sasiada <- 0
  licznik <- 0
  mianownik <- 0
  cof_wartosci <- c()
  for (i in 1: nrow(dist.eucl)){
    wiersz <- dist.eucl[i,] 
    wiersz <- sort(wiersz)
    wiersz <- wiersz[1: k + 1] 
    suma_odl_do_sasiadow <- 0
    for(j in 1: length(wiersz)) {
      index <- which( wiersz == wiersz[j] )
      suma_odl_do_sasiadow <- suma_odl_do_sasiadow + wiersz[j]
      wiersz_sasiada <- dist.eucl[index, ] 
      wiersz_sasiada <- sort(wiersz_sasiada)
      wiersz_sasiada <- wiersz_sasiada[1: k + 1]
      suma_odl_do_sasiadow_sasiada <- 0
      for(l in 1: length(wiersz_sasiada)) {
        suma_odl_do_sasiadow_sasiada <- suma_odl_do_sasiadow_sasiada + wiersz_sasiada[l]
      }
      mianownik <- mianownik + suma_odl_do_sasiadow_sasiada
    }
    licznik <- suma_odl_do_sasiadow * k
    wartosc_cof <- licznik / mianownik
    cof_wartosci <- rbind(cof_wartosci, c(wartosc_cof))
  }
  return(cof_wartosci)
}

cof.results <- cof(data, 10)

znajdz_wyjatki_cof <- function(cof_wartosci, wspolczynnik_wyjatkowosci) {
  cof_wartosci_posortowane <- sort(cof_wartosci)
  cof_wartosc_maksymalna <- cof_wartosci_posortowane[length(cof_wartosci_posortowane)]
  cof_wyjatki <- c()
  wartosc <- cof_wartosc_maksymalna - ((cof_wartosc_maksymalna - 1)/wspolczynnik_wyjatkowosci)
  for (i in 1: nrow(cof_wartosci)){
    if (cof_wartosci[i] > wartosc) {
      cof_wyjatki <- rbind(cof_wyjatki, c(cof_wartosci[i]))
    }
  }
  return(cof_wyjatki)
}

cof.exceptions <- znajdz_wyjatki_cof(cof.results, 0.83)

plot(cof.results, pch=19, xlab="Indeks", ylab="Wyniki algorytmu cof", col="blue")


