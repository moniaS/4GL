######## PRZYGOTOWANIE DANYCH ##########
#zbi�r danych ze strony https://www.kaggle.com/aljarah/xAPI-Edu-Data

#wczytanie danych
data <- read.csv('C:/Users/Monia/Documents/Studia/1 semestr/4GL/Zadanie3/dane.csv', header = TRUE, sep = ',')
data <- data[c(7,11,12)]
names(data) <- c("topic", "visited_resources", "absence_days")

######## ZALADOWANIE PAKIETOW ##########

#za�adowanie biblioteki do obliczenia dystansu
install.packages("analogue")
library(analogue)

#za�adowanie biblioteki do eksportowania do Excel
install.packages("xlsx")
library(xlsx)

#za�adowanie biblioteki do tworzenia wykresu
install.packages("ggvis")
library(ggvis)

#za�adowanie biblioteki do obliczania funkcji przynale�no�ci
install.packages("FuzzyR")
library(FuzzyR)

######## OBLICZENIE ODLEGLOSCI #########

distance.euclidean <- distance(data, method = "euclidean")
write.xlsx (x = as.data.frame(distance.euclidean), file = "euclidean_results.xlsx")

distance.manhattan<- distance(data, method = "manhattan")
write.xlsx (x = as.data.frame(distance.manhattan), file = "manhattan_results.xlsx")

distance.gower <- distance(data, method = "gower")
write.xlsx (x = as.data.frame(distance.gower), file = "gower_results.xlsx")

#DODAC CZWARTA MIARE

######## ROZRZUT DANYCH #########

#wyswietlenie wykresu prezentuj�cego rozrzut danych dla atrybut�w visited_resources i topic
data %>% ggvis(~visited_resources, ~topic) %>% layer_points()

#wyswietlenie wykresu prezentuj�cego rozrzut danych dla atrybut�w visited_resources i absence_days
data %>% ggvis(~visited_resources, ~absence_days) %>% layer_points()

#wyswietlenie wykresu prezentuj�cego rozrzut danych dla atrybut�w absence_days i topic
data %>% ggvis(~absence_days, ~topic) %>% layer_points()

####### OBLICZANIE FUNKCJI PRZYNALE�NO�CI ########

#funkcja przynale�no�ci tr�jk�tna dla atrybutu absence_days

function.triangle.low <- genmf('trimf', c(0, 10, 30))
triangle.low.values <- evalmf(data[,c(3)], function.triangle.low)

function.triangle.medium <- genmf('trimf', c(25, 40, 70))
triangle.medium.values <- evalmf(data[,c(3)], function.triangle.medium)

function.triangle.high <- genmf('trimf', c(60, 80, 100))
triangle.high.values <- evalmf(data[,c(3)], function.triangle.high)

#funkcja przynale�no�ci trapezoidalna dla atrybutu visited_resources

function.trapezoid.low <- genmf('trapmf', c(0, 10, 20, 30))
trapezoid.low.values <- evalmf(data[,c(2)], function.trapezoid.low)

function.triangle.medium <- genmf('trapmf', c(25, 40, 55, 70))
triangle.medium.values <- evalmf(data[,c(3)], function.triangle.medium)

function.triangle.high <- genmf('trapmf', c(60, 70, 85, 100))
triangle.high.values <- evalmf(data[,c(3)], function.triangle.high)

