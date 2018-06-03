# za�adowanie pakietu

install.packages("magick")
library(magick)

############# ODCZYT I ZAPIS OBRAZK�W ################

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

############# TRANSFORMACJE OBRAZK�W ################

# skalowanie wzgl�dem szerko�ci 
shrek <- image_scale(shrek, "200")

# wyci�cie cz�ci obrazka
image_crop(shrek, "60x80+70")

# skalowanie wzgl�dem szerko�ci
image_scale(shrek, "150")

# skalowanie wzgl�dem wysoko�ci
image_scale(shrek, "x150")

# obracanie
image_rotate(shrek, 45)

# obr�t pionowy
image_flip(shrek)

############# GEOMETRIA W OBRAZACH ################

# dodanie punktu na obrazku
image_write(image_annotate(shrek, "Shrek", location = geometry_point(60, 150), size = 24), "shrek_point.png")

# wyci�cie cz�ci obrazka
image_crop(shrek, geometry_area(80, 120, 60))

# zmiana rozmiaru w px
image_write(image_resize(shrek, geometry_size_pixels(height = 150)), "shrek_resize_px.png")

# zmiana rozmiaru w %
image_write(image_resize(shrek, geometry_size_percent(80)), "shrek_resize_%.png")

############# KOLORY ################

# dodanie t�a obrazu
image_background(shrek, "red")

# przekszta�cenie obrazu do skali szaro�ci
image_quantize(shrek, colorspace = 'gray')

# zwi�kszenie jasno�ci
image_modulate(shrek, brightness = 150)

# zwi�kszenie nasycenia
image_modulate(shrek, saturation = 200)

# dodanie czerwonego koloru
image_colorize(shrek, 20, "red")

# zast�pienie pikseli z najbli�szego s�siedztwa
image_median(shrek, radius = 5)

