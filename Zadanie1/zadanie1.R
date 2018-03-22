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
data_pred <- knn(train = trainData, test = testData, cl = trainLabels, k=3)
labels <- data.frame(trainLabels)

#po³¹czenie `data_pred` i `testLabels` 
merge <- data.frame(data_pred, testLabels)

names(merge) <- c("Przewidywane rezultaty", "Oczekiwane rezultaty")

#wyswietlenie tabelki informujacej o liczbie pokrywajacych sie wynikow klasyfikacji
CrossTable(x = testLabels, y = data_pred, prop.chisq=FALSE)

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

