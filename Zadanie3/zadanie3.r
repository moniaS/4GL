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

######## OBLICZENIE ODLEGLOSCI #########

distance.euclidean <- distance(data, method = "euclidean")
write.xlsx (x = as.data.frame(distance.euclidean), file = "euclidean_results.xlsx")

distance.manhattan<- distance(data, method = "manhattan")
write.xlsx (x = as.data.frame(distance.manhattan), file = "manhattan_results.xlsx")

distance.gower <- distance(data, method = "gower")
write.xlsx (x = as.data.frame(distance.gower), file = "gower_results.xlsx")

#DODAC CZWARTA MIARE

######## ROZRZUT DANYCH #########

#wyswietlenie wykresu prezentuj¹cego rozrzut danych dla atrybutów visited_resources i topic
data %>% ggvis(~visited_resources, ~topic) %>% layer_points()

#wyswietlenie wykresu prezentuj¹cego rozrzut danych dla atrybutów visited_resources i absence_days
data %>% ggvis(~visited_resources, ~absence_days) %>% layer_points()

#wyswietlenie wykresu prezentuj¹cego rozrzut danych dla atrybutów absence_days i topic
data %>% ggvis(~absence_days, ~topic) %>% layer_points()
