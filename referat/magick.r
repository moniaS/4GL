# za³adowanie pakietu

install.packages("magick")
library(magick)

install.packages("tesseract")
library(tesseract)

############# ODCZYT I ZAPIS OBRAZKÓW ################

# wczytanie obrazka

shrek<-image_read('https://vignette.wikia.nocookie.net/deathbattlefanon/images/c/ce/Shrek_clipart4.png/revision/latest?cb=20140813064239')
shrek_2<-image_read('https://vignette.wikia.nocookie.net/fantendo/images/9/9c/Shrek-PNG-File.png/revision/latest?cb=20170228010750')
shrek_3<-image_read('http://pngimg.com/uploads/shrek/shrek_PNG34.png')
gif<-image_read('https://media.giphy.com/media/G78JNWI04lXqM/source.gif')
t³o<-image_read('https://i.imgur.com/CuShj4j.jpg')
osio³<-image_read('https://pl.seaicons.com/wp-content/uploads/2015/07/Donkey-2-icon.png')
fiona <- image_read('http://icons.iconarchive.com/icons/majdi-khawaja/shrek/256/Fiona-3-icon.png')
scientist <- image_read('http://science.phillipmartin.info/scientist_explaining.png')
logo1 <- image_read("https://www.coca-cola.co.uk/content/dam/journey/gb/en/hidden/History/Heritage/Non-Hero-article-images/Coca-Cola-Logo-1887.png")
logo2 <- image_read("https://www.coca-cola.co.uk/content/dam/journey/gb/en/hidden/History/Heritage/Non-Hero-article-images/Coca-Cola-Logo-1890.png")
logo3 <- image_read("https://www.coca-cola.co.uk/content/dam/journey/gb/en/hidden/History/Heritage/Non-Hero-article-images/Coca-Cola-Logo-1941.png")


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
scientist <- image_scale(scientist,"x250")

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


############ EFEKTY ##############

#szumy w obrazku
image_noise(shrek, noisetyp = "Poisson" )

#rozmazanie
image_blur(shrek, radius = 2, sigma = 2)

#efekt wêgla
image_charcoal(shrek, radius = 1, sigma = 1)

#efekt farb olejnych
image_oilpaint(shrek, radius = 1)

#negatyw
shrek <- image_negate(shrek)

#wyg³adzanie
image_despeckle(shrek, times = 4L)

#za³amywanie zdjêcia
image_implode(shrek, factor = 0.4)

#inne zmiany
image_edge(shrek, radius = 5)
image_emboss(shrek, radius = 1, sigma = 0.5)



######### MALOWANIE I PISANIE PO OBRAZKU #########

#wstrzykiwanie koloru
image_fill(shrek, "blue", point = "+80+80", fuzz = 20)

#nak³adanie tekstu
image_annotate(shrek, "SCARY SHREK", gravity = "Center", degrees=30, size=20, font="mono", color="black", strokecolor = "red", boxcolor = "white")

#########  NAK£ADANIE GRAFIK I RYSUNKÓW ###########
library(ggplot2)



#image_graph
img <- image_graph(600, 400)
plot(iris$Petal.Length, iris$Petal.Width, pch=21, bg=c("red","green3","blue"), main="Iris Data")
#plot(iris$Petal.Width, iris$Petal.Length, pch=21, bg=c("red","green3","blue"), main="Iris Data")
dev.off()

img <- image_modulate(img, saturation = 200)
img <- image_implode(img, factor = 0.4)
img <- image_composite(img, scientist, offset = "+120+20")
img <- image_rotate(img, 20)
print(img)

#image_draw
img <- image_draw(shrek)
rect(50, 0, 150, 70, border = "red", lty = "dashed", lwd = 5)
abline(h = 150, col = 'blue', lwd = '10', lty = "dotted")
text(20, 110, "Shrek", family = "monospace", cex = 2, srt = 90)
dev.off()

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

#SHREK
shreks <- image_scale(c(shrek, shrek_2, shrek_3), "200")

image_animate(image_morph(shreks, 10))

image_animate(image_montage(shreks))

#COCA COLA
#tworzenie obiektu który zawiera ramki z ró¿nymi obrazmkami
both <- image_scale(c(logo1, logo2, logo3), "400")

#animacja - obrazki zmieniaj¹ siê co sekundê
image_animate(image_scale(both), fps = 1, dispose = "previous")

#animacja - przejœcie pomiêdzy obrazkami
image_animate(image_morph(both, 20))

#mozaik - na³o¿enie wszystkich rysunków
image_mosaic(both)


######### WYKRYWANIE TEKSTU ########

if(require("tesseract")){
  img <- image_read("https://quotesstory.com/wp-content/uploads/2017/05/life-top-40-funny-witty-quotes.jpg")
  text <- image_ocr(img)
}
cat(text)



