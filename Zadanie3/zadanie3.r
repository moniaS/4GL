######## PRZYGOTOWANIE DANYCH ##########
#zbiór danych ze strony https://www.kaggle.com/aljarah/xAPI-Edu-Data

#wczytanie danych
data <- read.csv('C:/Users/Monia/Documents/Studia/1 semestr/4GL/Zadanie3/dane.csv', header = TRUE, sep = ',')
data <- data[c(7,11,12)]
names(data) <- c("topic", "visited_resources", "absence_days")

######## ZALADOWANIE PAKIETOW ##########

#za³adowanie biblioteki do obliczenia dystansu
install.packages("analogue")
library(analogue)

#za³adowanie biblioteki do eksportowania do Excel
install.packages("xlsx")
library(xlsx)

#za³adowanie biblioteki do tworzenia wykresu
install.packages("ggvis")
library(ggvis)

#za³adowanie biblioteki do obliczania funkcji przynale¿noœci
install.packages("FuzzyR")
library(FuzzyR)

######## OBLICZENIE ODLEGLOSCI #########

distance.euclidean <- distance(data, method = "euclidean")
write.xlsx (x = as.data.frame(distance.euclidean), file = "euclidean_results.xlsx")

distance.manhattan<- distance(data, method = "manhattan")
write.xlsx (x = as.data.frame(distance.manhattan), file = "manhattan_results.xlsx")

distance.gower <- distance(data, method = "gower")
write.xlsx (x = as.data.frame(distance.gower), file = "gower_results.xlsx")

distance.chord <- distance(data, method = "chord")
write.xlsx (x = as.data.frame(distance.chord), file = "chord_results.xlsx")

######## ROZRZUT DANYCH #########

#wyswietlenie wykresu prezentuj¹cego rozrzut danych dla atrybutów visited_resources i topic
data %>% ggvis(~visited_resources, ~topic) %>% layer_points()

#wyswietlenie wykresu prezentuj¹cego rozrzut danych dla atrybutów visited_resources i absence_days
data %>% ggvis(~visited_resources, ~absence_days) %>% layer_points()

#wyswietlenie wykresu prezentuj¹cego rozrzut danych dla atrybutów absence_days i topic
data %>% ggvis(~absence_days, ~topic) %>% layer_points()

####### OBLICZANIE FUNKCJI PRZYNALE¯NOŒCI ########

#funkcja przynale¿noœci trójk¹tna dla atrybutu absence_days

function.triangle.low <- genmf('trimf', c(0, 0, 50))
triangle.low.values <- evalmf(data[,c(3)], function.triangle.low)
data[ , "low absence days"] <- triangle.low.values

function.triangle.medium <- genmf('trimf', c(0, 50, 100))
triangle.medium.values <- evalmf(data[,c(3)], function.triangle.medium)
data[ , "medium absence days"] <- triangle.medium.values

function.triangle.high <- genmf('trimf', c(50, 100, 100))
triangle.high.values <- evalmf(data[,c(3)], function.triangle.high)
data[ , "high absence days"] <- triangle.high.values

#wykres funkcji przynale¿noœci dla atrybutu absence_days

function.triangle <- newfis('tipper')
function.triangle <- addvar(function.triangle, 'input', 'absence_days', c(0, 100))
function.triangle <- addmf(function.triangle, 'input', 1, 'niska', 'trimf', c(0, 0, 50))
function.triangle <- addmf(function.triangle, 'input', 1, 'œrednia', 'trimf', c(0, 50, 100))
function.triangle <- addmf(function.triangle, 'input', 1, 'wysoka', 'trimf', c(50, 100, 100))
plotmf(function.triangle, "input", 1)

#funkcja przynale¿noœci trapezoidalna dla atrybutu visited_resources

function.trapezoid.very_low <- genmf('trapmf', c(0, 0, 10, 25))
trapezoid.very_low.values <- evalmf(data[,c(2)], function.trapezoid.very_low)
data[ , "very low visited resources"] <- trapezoid.very_low.values

function.trapezoid.low <- genmf('trapmf', c(10, 25, 40, 55))
trapezoid.low.values <- evalmf(data[,c(2)], function.trapezoid.low)
data[ , "low visited resources"] <- trapezoid.low.values

function.trapezoid.medium <- genmf('trapmf', c(40, 55, 70, 85))
trapezoid.medium.values <- evalmf(data[,c(2)], function.trapezoid.medium)
data[ , "medium visited resources"] <- trapezoid.medium.values

function.trapezoid.high <- genmf('trapmf', c(70, 85, 100, 100))
trapezoid.high.values <- evalmf(data[,c(2)], function.trapezoid.high)
data[ , "high visited resources"] <- trapezoid.high.values

#wykres funkcji przynale¿noœci dla atrybutu visited_resources

function.trapezoid <- newfis('tipper')
function.trapezoid <- addvar(function.trapezoid, 'input', 'visited_resources', c(0, 100))
function.trapezoid <- addmf(function.trapezoid, 'input', 1, 'bardzo niska', 'trapmf', c(0, 0, 10, 25))
function.trapezoid <- addmf(function.trapezoid, 'input', 1, 'niska', 'trapmf', c(10, 25, 40, 55))
function.trapezoid <- addmf(function.trapezoid, 'input', 1, 'œrednia', 'trapmf', c(40, 55, 70, 85))
function.trapezoid <- addmf(function.trapezoid, 'input', 1, 'wysoka', 'trapmf', c(70, 85, 100, 100))
plotmf(function.trapezoid, "input", 1)

#eksport wyników do Excela

write.xlsx (x = as.data.frame(data), file = "results.xlsx")
