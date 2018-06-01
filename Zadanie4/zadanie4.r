######## PRZYGOTOWANIE DANYCH ##########
#zbiór danych ze strony 
#https://datahub.io/core/natural-gas#resource-natural-gas-monthly
#www.quandl.com/api/v3/datasets/RATEINF/INFLATION_USA.csv?api_key=UWs93ZbpqbCoD68gg6zx


#wczytanie danych
#data <- read.csv('C:/Users/Monia/Documents/Studia/1 semestr/4GL/Zadanie4/dane.csv', header = TRUE, sep = ',')
data.gaz <- read.csv('D:/Studia - prace/2-stopien/4GL/repository/4GL/Zadanie4/gaz.csv', header = TRUE, sep = ',')
data.inflacja <- read.csv('D:/Studia - prace/2-stopien/4GL/repository/4GL/Zadanie4/inflacja.csv', header = TRUE, sep = ',')

