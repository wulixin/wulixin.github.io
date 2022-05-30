

library(magick)
tiger <- image_read('//Users//wulixin//Desktop//liuyifei.png')
print(tiger)

image_border(image_background(tiger, "hotpink"), "#000080", "20x10")

# Trim margins
image_trim(tiger)

tiger<-image_scale(tiger, "x300") # height: 300px

image_rotate(tiger, 45)

# Brightness, Saturation, Hue
image_modulate(tiger, brightness = 80, saturation = 120, hue = 90)
image_negate(tiger)

# Customize text
image_annotate(tiger, "focus me LOVE ", size = 30, color = "red", boxcolor = "pink",
               degrees = 60, location = "+50+100")


frink <- image_read("https://jeroen.github.io/images/frink.png")
frink2 <- image_scale(frink, "100")
image_info(frink)
image_info(frink2)


test <- image_rotate(frink, 90)
test <- image_background(test, "blue", flatten = TRUE)
test <- image_border(test, "red", "10x10")
test <- image_annotate(test, "This is how we combine transformations", color = "white", size = 30)
print(test)

image_read("https://jeroen.github.io/images/frink.png") %>%
  image_rotate(270) %>%
  image_background("pink", flatten = TRUE) %>%
  image_border("red", "10x10") %>%
  image_annotate("The same thing with pipes", color = "white", size = 30)


bigdata <- image_read('//Users//wulixin//Desktop//bigdata.png')
frink <- image_read("//Users//wulixin//Desktop//liuyifei.png")
logo <- image_read("https://jeroen.github.io/images/Rlogo.png")
img <- c(bigdata, logo, frink)
img <- image_scale(img, "300x300")
image_info(img)


image_append(image_scale(img, "x200"))

image_append(image_scale(img, "100"), stack = TRUE)


bigdatafrink <- image_scale(image_rotate(image_background(tiger, "none"), 15), "x300")
image_composite(image_scale(bigdata, "x400"), bigdatafrink, offset = "+180+100")

image_animate(image_scale(img, "200x200"), fps = 1, dispose = "previous")

newlogo <- image_scale(image_read("//Users//wulixin//Desktop//liuyifei.png"))
oldlogo <- image_scale(image_read("//Users//wulixin//Desktop//liuyifei.png"))
image_resize(c(oldlogo, newlogo), '200x150!') %>%
  image_background('white') %>%
  image_morph() %>%
  image_animate(optimize = TRUE)


# Print over another graphic
plot(cars)
rasterImage(tiger, 21, 0, 25, 80)

library(ggplot2)
library(grid)
qplot(speed, dist, data = cars, geom = c("point", "smooth"))


grid.raster(frink)

###读取照片上的文字
tiger<-image_annotate(tiger, "focus me LOVE ", size = 30, color = "red", boxcolor = "pink",
               degrees = 60, location = "+50+100")

# Extract text
cat(image_ocr(tiger))


