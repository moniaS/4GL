# za³adowanie pakietu

install.packages("magick")
library(magick)

############# ODCZYT I ZAPIS OBRAZKÓW ################

# wczytanie obrazka

shrek<-image_read('https://vignette.wikia.nocookie.net/deathbattlefanon/images/c/ce/Shrek_clipart4.png/revision/latest?cb=20140813064239')
print(shrek)
image_info(shrek)
image_comment(shrek, "Obrazek Shreka")

# zapis obrazka

image_write(shrek, path = "shrek.png")

# zmiana formatu obrazka

image_write(shrek, path = "shrek.jpg", format = "jpg")
shrek_jpg <- image_convert(shrek, "jpg")

############# TRANSFORMACJE OBRAZKÓW ################

# skalowanie wzglêdem szerkoœci 
shrek <- image_scale(shrek, "200")

# wyciêcie czêœci obrazka
image_crop(shrek, "60x80+70")

# skalowanie wzglêdem szerkoœci
image_scale(shrek, "150")

# skalowanie wzglêdem wysokoœci
image_scale(shrek, "x150")

# obracanie
image_rotate(shrek, 45)

# obrót pionowy
image_flip(shrek)

############# GEOMETRIA W OBRAZACH ################

# dodanie punktu na obrazku
image_write(image_annotate(shrek, "Shrek", location = geometry_point(60, 150), size = 24), "shrek_point.png")

# wyciêcie czêœci obrazka
image_crop(shrek, geometry_area(80, 120, 60))

# zmiana rozmiaru w px
image_write(image_resize(shrek, geometry_size_pixels(height = 150)), "shrek_resize_px.png")

# zmiana rozmiaru w %
image_write(image_resize(shrek, geometry_size_percent(80)), "shrek_resize_%.png")

############# KOLORY ################

# dodanie t³a obrazu
image_background(shrek, "red")

# przekszta³cenie obrazu do skali szaroœci
image_quantize(shrek, colorspace = 'gray')

# zwiêkszenie jasnoœci
image_modulate(shrek, brightness = 150)

# zwiêkszenie nasycenia
image_modulate(shrek, saturation = 200)

# dodanie czerwonego koloru
image_colorize(shrek, 20, "red")

# zast¹pienie pikseli z najbli¿szego s¹siedztwa
image_median(shrek, radius = 5)

