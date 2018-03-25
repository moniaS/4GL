######### PRZYGOTOWANIE DANYCH ##########

#wczytanie danych
data <- read.csv('C:/Users/Monia/Documents/Studia/1 semestr/4GL/Zadanie1/dane.csv', header = FALSE, sep = ' ')

#przypisanie nazw kolumnom
names(data) <- c("nauka_przedmiot", "l_powtorzen", "nauka_powiaz", "egz_powiaz", "egz_przedmiot", "wiedza")

#wyswietlenie pierwszych pozycji zbioru
head(data)

#wyswietlenie podsumowania zbioru danych
summary(data)

#dzieki temu wywo³aniu mozna otrzymac powtarzalne rezultaty
set.seed(12345)

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


######### ALGORYTM KNN - MIARA EUKLIDESOWA #############

#zaladowanie biblioteki do algorytmu knn z miara euklidesowa
library(class)

#za³adowanie biblioteki do tworzenia CrossTable
library(gmodels)

#wyswietlenie wykresu prezentuj¹cego poziom wiedzy w zaleznosci od wyniku egzaminow
data %>% ggvis(~egz_powiaz, ~egz_przedmiot, fill = ~wiedza) %>% layer_points()

#klasyfikacja za pomoca algorytmu knn, miara euklidesowa, k = 3
knn.predict.euclides <- knn(train = train.data, test = test.data, cl = train.labels, k = 3)

#polaczenie `test.labels` i 'knn.predict.euclides`
knn.merge.euclides <- data.frame(test.labels, knn.predict.euclides)
names(knn.merge.euclides) <- c("Aktualne", "Przewidziane")

#wyswietlenie tabelki informujacej o liczbie pokrywajacych sie wynikow klasyfikacji
CrossTable(x = test.labels, y = knn.predict.euclides, prop.chisq = FALSE, dnn = c('aktualne', 'przewidziane'))

######### ALGORYTM KNN - MIARA GOWERA #############

#zaladowanie biblioteki do algorytmu knn z miara Gowera
library(dprep)

#za³adowanie biblioteki do tworzenia CrossTable
library(gmodels)

#klasyfikacja za pomoc¹ algorytmu knn, miara Gowera, k = 3
knn.predict.gower = knngow(train.data.labels, test.data, 3)

#polaczenie `test.labels` i 'knn.predict.gower`
knn.merge.gower <- data.frame(test.labels, knn.predict.gower)
names(knn.merge.gower) <- c("Aktualne", "Przewidziane")

#obie zmienne musza byc wektorami, jesli sa nie trzeba tego wykonywac
#knnGowClassifier <- knnGowClassifier[,1]
#testGowLabels <- testGowLabels[,1]

#wyswietlenie tabelki informujacej o liczbie pokrywajacych sie wynikow klasyfikacji
CrossTable(x = test.labels, y = knn.predict.gower, prop.chisq = FALSE, dnn = c('aktualne', 'przewidziane'))

####### ALGORYTM BAYESOWSKI ########### 

#zaladowanie biblioteki do algorytmu bayesowskiego
library(e1071)

#zaladowanie biblioteki do tabeli CrossTable
library(gmodels)

#klasyfikacja za pomoca algorytmu naiwnego Bayesa
bayes.classifier = naiveBayes(train.data, train.labels)

#sprawdzenie wynikow klasyfikacji
bayes.predict = predict(bayes.classifier, test.data)

#polaczenie 'test.labels' i 'bayes.predict'
bayes.merge = data.frame(test.labels, bayes.predict)
names(bayes.merge) <- c("Aktualne", "Przewidziane")

#wyswietlenie tabelki informujacej o liczbie pokrywajacych sie wynikow klasyfikacji
CrossTable(x = test.labels, y = bayes.predict, prop.chisq = FALSE, dnn = c('aktualne','przewidziane'))

########## DRZEWO DECYZYJNE ##########

#zaladowanie biblioteki do tworzenia modelu drzewa decyzyjnego
library(rpart)

#zaladowanie biblioteki do rysowania drzewa decyzyjnego
library(rattle)

#zaladowanie biblioteki do tabeli CrossTable
library(gmodels)

#budowanie drzewa decyzyjnego
decision.tree.model <- rpart(wiedza ~ nauka_przedmiot + l_powtorzen + nauka_powiaz + egz_powiaz + egz_przedmiot, data = train.data.labels, method = "class")

#tworzenie diagramu drzewa decyzyjnego
fancyRpartPlot(decision.tree.model)

#klasyfikacja za pomoca drzewa decyzyjnego
decision.tree.predict <- predict(decision.tree.model, test.data, type = "class")

#polaczenie 'test.labels' i 'decision.tree.predict'
decision.tree.merge <- data.frame(test.labels, decision.tree.predict)
names(decision.tree.merge) <- c("Aktualne", "Przewidziane")

#wyswietlenie tabelki informujacej o liczbie pokrywajacych sie wynikow klasyfikacji
CrossTable(x = test.labels, y = decision.tree.predict, prop.chisq = FALSE, dnn = c('aktualne','przewidziane'))
