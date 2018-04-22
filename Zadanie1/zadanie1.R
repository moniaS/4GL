######### PRZYGOTOWANIE DANYCH ##########

#wczytanie danych
data <- read.csv('dane.csv', header = FALSE, sep = ' ')

#przypisanie nazw kolumnom
names(data) <- c("nauka_przedmiot", "l_powtorzen", "nauka_powiaz", "egz_powiaz", "egz_przedmiot", "wiedza")

#wymieszanie danych ze zbioru
data<- data[sample(nrow(data)),] 

#podzielenie danych na 2 zbiory
train.index <- sample(1:nrow(data), 0.7 * nrow(data))
test.index <- setdiff(1:nrow(data), train.index)

#przypisanie danych treningowych i testowych (kolumny 1-5)
train.data <- data[train.index, 1:5]
test.data <- data[test.index, 1:5]

#przypisanie klas ze zbioru treningowego i testowego (kolumna 6)
train.labels <- data[train.index, 6]
test.labels <- data[test.index, 6]

#przypisanie danych treningowych z klasami
train.data.labels <- data[test.index, 1:6]

#za³adowanie biblioteki do tworzenia wykresu
library(ggvis)

#wyswietlenie wykresu prezentuj¹cego poziom wiedzy w zaleznosci od nauka_przedmiot i nauka_powiaz
data %>% ggvis(~nauka_przedmiot, ~nauka_powiaz, fill = ~wiedza) %>% layer_points()

#wyswietlenie wykresu prezentuj¹cego poziom wiedzy w zaleznosci od egz_powiaz i egz_przedmiot
data %>% ggvis(~egz_powiaz, ~egz_przedmiot, fill = ~wiedza) %>% layer_points()

######### DEFINICJA FUNKCJI WYŒWIETLAJ¥CEJ WYKRES S£UPKOWY ##########

showClassificationResults <- function(prediction, expected){
  results <- table(prediction, expected, dnn = c("przewidziane", "oczekiwane"))
  data.frame(results)
  correctly.classified = diag(results) # liczba poprawnie przyporzadkowanych instancji dla kazdej klasy
  predictions = apply(results, 1, sum) # liczba wykonanych klasyfikacji
  incorrectly.classified = predictions - correctly.classified # liczba niepoprawnie przyporzadkowanych instancji dla kazdej klasy
  list <- list()
  list <- rbind(list, correctly.classified)
  list <- rbind(list, incorrectly.classified)
  barplot(as.matrix(list),
          main = "Wyniki klasyfikacji ka¿dej z klas",
          xlab = "Klasa",
          col = c("green", "red"),
          ylim=c(0, 40)
  )
  legend("topright",
         c("Poprawne","Niepoprawne"),
         fill = c("green", "red")
  )
}

######### ALGORYTM KNN - MIARA EUKLIDESOWA #############

#zaladowanie biblioteki do algorytmu knn z miara euklidesowa
library(class)

#za³adowanie biblioteki do tworzenia CrossTable
library(gmodels)

#klasyfikacja za pomoca algorytmu knn, miara euklidesowa, k = 5
knn.predict.euclides <- knn(train = train.data, test = test.data, cl = train.labels, k =5)

#wyswietlenie uproszczonej tabelki informujacej o liczbie pokrywajacych sie wynikow klasyfikacji
table(x = knn.predict.euclides, y = test.labels, dnn = c('Przewidziane', 'Aktualne'))

#wyswietlenie wykresu slupkowego z rezultatami klasyfikacji
showClassificationResults(knn.predict.euclides, test.labels)

######### ALGORYTM KNN - MIARA GOWERA #############

#zaladowanie biblioteki do algorytmu knn z miara Gowera
library(dprep)

#za³adowanie biblioteki do tworzenia CrossTable
library(gmodels)

#klasyfikacja za pomoc¹ algorytmu knn, miara Gowera, k = 7
knn.predict.gower = knngow(train.data.labels, test.data, 7)

#wyswietlenie uproszczonej tabelki informujacej o liczbie pokrywajacych sie wynikow klasyfikacji
table(x = knn.predict.gower, y = test.labels, dnn = c('Przewidziane', 'Aktualne'))

#wyswietlenie wykresu slupkowego z rezultatami klasyfikacji
showClassificationResults(knn.predict.gower, test.labels)

####### ALGORYTM BAYESOWSKI ########### 

#zaladowanie biblioteki do algorytmu bayesowskiego
library(e1071)

#zaladowanie biblioteki do tabeli CrossTable
library(gmodels)

#klasyfikacja za pomoca algorytmu naiwnego Bayesa
bayes.classifier = naiveBayes(train.data, train.labels)

#sprawdzenie wynikow klasyfikacji
bayes.predict = predict(bayes.classifier, as.matrix.data.frame(test.data))

#wyswietlenie tabelki informujacej o liczbie pokrywajacych sie wynikow klasyfikacji
CrossTable(x = bayes.predict, y = test.labels,  prop.chisq = FALSE, dnn = c('przewidziane','aktualne'))

#wyswietlenie wykresu slupkowego z rezultatami klasyfikacji
showClassificationResults(bayes.predict, test.labels)

########## DRZEWO DECYZYJNE ##########

#zaladowanie biblioteki do rysowania drzewa decyzyjnego
library(rattle)

#zaladowanie biblioteki do tabeli CrossTable
library(gmodels)

#zaladowanie biblioteki do tworzenia modelu drzewa decyzyjnego
library(rpart)

#budowanie drzewa decyzyjnego
decision.tree.model <- rpart(wiedza ~., data = train.data.labels, method = "class")

#tworzenie diagramu drzewa decyzyjnego
fancyRpartPlot(decision.tree.model)

#klasyfikacja za pomoca drzewa decyzyjnego
decision.tree.predict <- predict(decision.tree.model, test.data, type = "class")

#wyswietlenie tabelki informujacej o liczbie pokrywajacych sie wynikow klasyfikacji
CrossTable(y = test.labels, x = decision.tree.predict, prop.chisq = FALSE, dnn = c('przewidziane', 'aktualne'))

#wyswietlenie wykresu slupkowego z rezultatami klasyfikacji
showClassificationResults(decision.tree.predict, test.labels)

################ Dodawanie danych #################

#dodanie niepasuj¹cych rekordow do zbioru danych
data <- rbind(data, c(0.5, 0.5, 0.5, 2,  5, "very_low"))
data <- rbind(data, c(0.5, 0.5, 0.5, 1.5,  3, "very_low"))
data <- rbind(data, c(0.1, 0.4, 0.3, 2.5,  4, "very_low"))
data <- rbind(data, c(0.2, 0.3, 0.5, 3,  6, "very_low"))
data <- rbind(data, c(0.8, 0.9, 0.7, 1.1,  2, "very_low"))

#nalezy ponownie pomieszac dane, podzielic i przeprowadzic klasyfikacje algorytmami knn, bayes i drzewem decyzyjnym

################ Algorytm LOF #################
library(Rlof)

data$egz_powiaz <- as.numeric(data$egz_powiaz)
data$egz_przedmiot <- as.numeric(data$egz_przedmiot)

results_lof <- lof(data[4:5], 10)

data.frame(data.lof)

plot(results_lof, pch=19, xlab="Indeks", ylab="Wyniki algorytmu lof", col="blue")