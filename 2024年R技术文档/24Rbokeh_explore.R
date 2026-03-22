

library(rbokeh)


p <- figure() %>%
  ly_points(Sepal.Length, Sepal.Width, data = iris,
            color = Species, glyph = Species,
            hover = list(Sepal.Length, Sepal.Width))

p



z <- lm(dist ~ speed, data = cars)
p <- figure(width = 600, height = 600) %>%
  ly_points(cars, hover = cars) %>%
  ly_lines(lowess(cars), legend = "lowess") %>%
  ly_abline(z, type = 2, legend = "lm")
p



h <- figure(width = 600, height = 400) %>%
  ly_hist(eruptions, data = faithful, breaks = 40, freq = FALSE) %>%
  ly_density(eruptions, data = faithful)
h

# prepare data
elements <- subset(elements, !is.na(group))
elements$group <- as.character(elements$group)
elements$period <- as.character(elements$period)

# add colors for groups
metals <- c("alkali metal", "alkaline earth metal", "halogen",
            "metal", "metalloid", "noble gas", "nonmetal", "transition metal")
colors <- c("#a6cee3", "#1f78b4", "#fdbf6f", "#b2df8a", "#33a02c",
            "#bbbb88", "#baa2a6", "#e08e79")
elements$color <- colors[match(elements$metal, metals)]
elements$type <- elements$metal

# make coordinates for labels
elements$symx <- paste(elements$group, ":0.1", sep = "")
elements$numbery <- paste(elements$period, ":0.8", sep = "")
elements$massy <- paste(elements$period, ":0.15", sep = "")
elements$namey <- paste(elements$period, ":0.3", sep = "")

# create figure
p <- figure(title = "Periodic Table", tools = c("resize", "hover"),
            ylim = as.character(c(7:1)), xlim = as.character(1:18),
            xgrid = FALSE, ygrid = FALSE, xlab = "", ylab = "",
            height = 445, width = 800) %>%
  
  # plot rectangles
  ly_crect(group, period, data = elements, 0.9, 0.9,
           fill_color = color, line_color = color, fill_alpha = 0.6,
           hover = list(name, atomic.number, type, atomic.mass,
                        electronic.configuration)) %>%
  
  # add symbol text
  ly_text(symx, period, text = symbol, data = elements,
          font_style = "bold", font_size = "10pt",
          align = "left", baseline = "middle") %>%
  
  # add atomic number text
  ly_text(symx, numbery, text = atomic.number, data = elements,
          font_size = "6pt", align = "left", baseline = "middle") %>%
  
  # add name text
  ly_text(symx, namey, text = name, data = elements,
          font_size = "4pt", align = "left", baseline = "middle") %>%
  
  # add atomic mass text
  ly_text(symx, massy, text = atomic.mass, data = elements,
          font_size = "4pt", align = "left", baseline = "middle")

p

library(maps)
data(world.cities)
caps <- subset(world.cities, capital == 1)
caps$population <- prettyNum(caps$pop, big.mark = ",")
figure(width = 800, height = 450, padding_factor = 0) %>%
  ly_map("world", col = "gray") %>%
  ly_points(long, lat, data = caps, size = 5,
            hover = c(name, country.etc, population))


orstationc <- read.csv("http://geog.uoregon.edu/bartlein/old_courses/geog414s05/data/orstationc.csv")

gmap(lat = 44.1, lng = -120.767, zoom = 6, width = 700, height = 600) %>%
  ly_points(lon, lat, data = orstationc, alpha = 0.8, col = "red",
            hover = c(station, Name, elev, tann))


p <- figure(width = 800, height = 400) %>%
  ly_lines(date, Freq, data = flightfreq, alpha = 0.3) %>%
  ly_points(date, Freq, data = flightfreq,
            hover = list(date, Freq, dow), size = 5) %>%
  ly_abline(v = as.Date("2001-09-11"))
p



###################grid plot 
tools <- c("pan", "wheel_zoom", "box_zoom", "box_select", "reset")
nms <- expand.grid(names(iris)[1:4], rev(names(iris)[1:4]), stringsAsFactors = FALSE)
splom_list <- vector("list", 16)
for(ii in seq_len(nrow(nms))) {
  splom_list[[ii]] <- figure(width = 200, height = 200, tools = tools,
                             xlab = nms$Var1[ii], ylab = nms$Var2[ii]) %>%
    ly_points(nms$Var1[ii], nms$Var2[ii], data = iris,
              color = Species, size = 5, legend = FALSE)
}
grid_plot(splom_list, ncol = 4, same_axes = TRUE, link_data = TRUE)


figure(ylab = "Height (inches)", width = 600) %>%
  ly_boxplot(voice.part, height, data = lattice::singer)

#quantile plot 
figure(legend_location = "top_left") %>%
  ly_quantile(Sepal.Length, group = Species, data = iris)

wa_cancer <- droplevels(subset(latticeExtra::USCancerRates, state == "Washington"))
## y axis sorted by male rate
ylim <- levels(with(wa_cancer, reorder(county, rate.male)))

figure(ylim = ylim, width = 700, height = 600, tools = "") %>%
  ly_segments(LCL95.male, county, UCL95.male,
              county, data = wa_cancer, color = NULL, width = 2) %>%
  ly_points(rate.male, county, glyph = 16, data = wa_cancer)



#######################################################
figure() %>%
  ly_points(cars, color = "red", size = 20)

n <- nrow(cars)
ramp <- colorRampPalette(c("red", "blue"))(n)
figure() %>%
  ly_points(cars, color = ramp, size = seq_len(n))



figure() %>%
  ly_points(Sepal.Length, Sepal.Width, data = iris, color = Species)


z <- lm(dist ~ speed, data = cars)
figure(width = 600, height = 600) %>%
  ly_points(cars, hover = cars, legend = "data") %>%
  ly_lines(lowess(cars), legend = "lowess") %>%
  ly_abline(z, type = 2, legend = "lm")












