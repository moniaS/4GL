#########ALGORYTM KNN#############

#wczytanie danych
data <- read.csv('C:/Users/Monia/Documents/Studia/1 semestr/4GL/Zadanie1/dane.csv', header = FALSE, sep = ' ')

#przypisanie nazw kolumnom
names(data) <- c("nauka_przedmiot", "l_powtorzen", "nauka_powiaz", "egz_powiaz", "egz_przedmiot", "wiedza")

#wyswietlenie pierwszych pozycji zbioru
head(data)

#wyœwietlenie podsmumowania zbioru danych
summary(data)

#wyswietlenie wykresu prezentuj¹cego poziom wiedzy w zaleznosci od wyniku egzaminow
data %>% ggvis(~egz_powiaz, ~egz_przedmiot, fill = ~wiedza) %>% layer_points()

#dziêki temu wywo³aniu mozna otrzymac powtarzalne rezultaty
set.seed(1234)

#wymieszanie danych ze zbioru
data<- data[sample(nrow(data)),] 

#podzielenie danych na 2 zbiory
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))

#przypisanie danych treningowych i testowych (kolumny 1-5)
trainData <- data[ind==1, 1:5]
testData <- data[ind==2, 1:5]

#przypisanie klas ze zbioru treningowego i testowego (kolumna 6)
trainLabels <- data[ind==1, 6]
testLabels <- data[ind==2, 6]

#za³adowanie biblioteki class
library(class)

#klasyfikacja za pomoc¹ algorytmu knn
knnClassifier <- class:::knn(train = trainData, test = testData, cl = trainLabels, k=3)
labels <- data.frame(trainLabels)

#po³¹czenie 'knnClassifier` i `testLabels` 
knnMerge <- data.frame(knnClassifier, testLabels)
names(knnMerge) <- c("Przewidziane", "Aktualne")

#wyswietlenie tabelki informujacej o liczbie pokrywajacych sie wynikow klasyfikacji
CrossTable(x = testLabels, y = knnClassifier, prop.chisq=FALSE, dnn = c('aktualne', 'przewidziane'))

########## knn - gower ###########
library(dprep)

ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))

training <- data[ind==1, 1:6]
testing <- data[ind==2, 1:6]

knnGowClassifier = knngow(training, testing, 3)

testGowLabels <- testing [,6]

knnMerge <- data.frame(knnGowClassifier, testGowLabels)
View(knnMerge)

#obie zmienne musza byc wektorami, jesli sa nie trzeba tego wykonywac
#knnGowClassifier <- knnGowClassifier[,1]
#testGowLabels <- testGowLabels[,1]

library(gmodels)
CrossTable(x = testGowLabels, y = knnGowClassifier, prop.chisq=FALSE, dnn = c('aktualne', 'przewidziane'))

#######ALGORYTM BAYESOWSKI#############

#nalezy pobrac odpowiednie biblioteki
library(e1071)
library(caret)

#przypisanie wartosci z kolumn 1-5 do x i klasy do y
x=data[,-6]
y=data$wiedza

#stworzenie modelu, cross validation=10
model = train(x, y, 'nb', trControl=trainControl(method='cv', number=10))

#podsumowanie modelu
model

#######ALGORYTM BAYESOWSKI - WERSJA 2###########
#wczytanie danych
data <- read.csv('C:/Users/Monia/Documents/Studia/1 semestr/4GL/Zadanie1/dane.csv', header = FALSE, sep = ' ')

#wyœwietlenie podsmumowania zbioru danych
summary(data)

#dziêki temu wywo³aniu mo¿na otrzymaæ powtarzalne rezultaty
set.seed(1234)

#wymieszanie danych ze zbioru
data<- data[sample(nrow(data)),] 

#podzielenie danych na 2 zbiory
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.67, 0.33))

#przypisanie danych treningowych i testowych (kolumny 1-5)
trainData <- data[ind==1, 1:5]
testData <- data[ind==2, 1:5]

#przypisanie klas ze zbioru treningowego i testowego (kolumna 6)
trainLabels <- data[ind==1, 6]
testLabels <- data[ind==2, 6]

#zaladowanie biblioteki
library(e1071)

#klasyfikacja za pomoc¹ algorytmu naiwnego Bayesa
bayesClassifier = naiveBayes(trainData, trainLabels)

#sprawdzenie wyników klasyfikacji
bayesTestPredict = predict(bayesClassifier, testData)

#zaladowanie biblioteki
library(gmodels)

#wyswietlenie tabelki pokazujacej rezultaty klasyfikacji
CrossTable(x = testLabels, y = bayesTestPredict, prop.chisq = FALSE, prop.t = FALSE, 
           prop.r = FALSE, dnn = c('aktualne','przewidziane'))

##########Drzewo decyzyjne##########

library(rpart)

tree_data <- rpart(data ~ nauka_przedmiot + l_powtorzen + nauka_powiaz + egz_powiaz + egz_przedmiot, method = "class", data=wiedza)

printcp(tree_data)

##########Drzewo decyzyjne - 2##########
#wczytanie danych
data <- read.csv('C:/Users/Monia/Documents/Studia/1 semestr/4GL/Zadanie1/dane.csv', header = FALSE, sep = ' ')

#przypisanie nazw kolumnom
names(data) <- c("nauka_przedmiot", "l_powtorzen", "nauka_powiaz", "egz_powiaz", "egz_przedmiot", "wiedza")

#wyœwietlenie podsmumowania zbioru danych
summary(data)

#dziêki temu wywo³aniu mo¿na otrzymaæ powtarzalne rezultaty
set.seed(1234)

#wymieszanie danych ze zbioru
data<- data[sample(nrow(data)),] 

#podzielenie danych na 2 zbiory
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.67, 0.33))

#przypisanie danych treningowych i testowych
trainDataLabels <- data[ind==1, 1:6]
testData <- data[ind==2, 1:5]

#przypisanie klas ze zbioru treningowego i testowego (kolumna 6)
trainLabels <- data[ind==1, 6]
testLabels <- data[ind==2, 6]

#zaladowanie biblioteki do tworzenia modelu drzewa decyzyjnego
library(rpart)

#budowanie drzewa decyzyjnego
decisionTree <- rpart(wiedza ~ nauka_przedmiot + l_powtorzen + nauka_powiaz + egz_powiaz + egz_przedmiot, data = trainDataLabels, method = "class")

#za³adowanie biblioteki do rysowania drzewa decyzyjnego
library(rattle)

#tworzenie diagramu drzewa decyzyjnego
fancyRpartPlot(decisionTree)

#klasyfikacja za pomoc¹ drzewa decyzyjnego
decisionTreePredict <- predict(decisionTree, testData, type = "class")

#po³¹czenie 'decisionTreePredict' i 'testLabels'
decisionTreeMerge <- data.frame(decisionTreePredict, testLabels)
names(decisionTreeMerge) <- c("Przewidziane", "Aktualne")

#za³adowanie biblioteki
library(gmodels)

#wyswietlenie tabelki pokazujacej rezultaty klasyfikacji
CrossTable(x = testLabels, y = decisionTreePredict, prop.chisq = FALSE, prop.t = FALSE, 
           prop.r = FALSE, dnn = c('aktualne','przewidziane'))
