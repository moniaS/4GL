# za³adowanie pakietu

install.packages("magick")
library(magick)

############# ODCZYT I ZAPIS OBRAZKÓW ################

# wczytanie obrazka

shrek<-image_read('https://vignette.wikia.nocookie.net/deathbattlefanon/images/c/ce/Shrek_clipart4.png/revision/latest?cb=20140813064239')
shrek_2<-image_read('https://vignette.wikia.nocookie.net/fantendo/images/9/9c/Shrek-PNG-File.png/revision/latest?cb=20170228010750')
shrek_3<-image_read('http://pngimg.com/uploads/shrek/shrek_PNG34.png')
gif<-image_read('https://media.giphy.com/media/G78JNWI04lXqM/source.gif')
t³o<-image_read('https://i.imgur.com/CuShj4j.jpg')
osio³<-image_read('https://pl.seaicons.com/wp-content/uploads/2015/07/Donkey-2-icon.png')
fiona <- image_read('http://icons.iconarchive.com/icons/majdi-khawaja/shrek/256/Fiona-3-icon.png')

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
shrek_2 <- image_scale(shrek_2, "200")
shrek_3 <- image_scale(shrek_3, "200")
osio³ <- image_scale(osio³, "150")
fiona <- image_scale(fiona, "x160")

# skalowanie wzglêdem szerkoœci 
t³o <- image_scale(t³o, "400")

# skalowanie wzglêdem szerkoœci 
fiona <- image_scale(fiona, "x200")

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

############# KOMPOZYCJA ################

# kompozycja obrazków
img <- image_composite(t³o, shrek, offset = "+0+50")
img <- image_composite(img, osio³, offset = "+250+80")
img <- image_composite(img, fiona, offset = "+150+60")

# dodanie obramowania
image_write(image_border(img, "green", "20x10"), "shrek_border.png")

# dodanie ramki
image_write(image_frame(img, "brown"), "shrek_frame.png")

############# ANIMACJE ################

shreks <- image_scale(c(shrek, shrek_2, shrek_3), "200")

image_animate(image_morph(shreks, 10))

image_animate(image_montage(shreks))



