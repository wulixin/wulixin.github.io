





devtools::install_github("ricardo-bion/ggradar", 
                         dependencies = TRUE)
devtools::install_github("ricardo-bion/ggdensity", 
                         dependencies=TRUE)

remotes::install_github("jamesotto852/ggdensity")

library("tidyverse"); theme_set(theme_minimal())
theme_update(panel.grid.minor = element_blank())
library("ggdensity")
library("patchwork")


df <- data.frame("x" = rnorm(1000), "y" = rnorm(1000))
p <- ggplot(df, aes(x, y)) + coord_equal()
p + geom_density_2d_filled()

p + geom_hdr()

library("palmerpenguins")

ggplot(penguins, aes(flipper_length_mm, bill_length_mm, fill = species)) +
  geom_hdr(xlim = c(160, 240), ylim = c(30, 70)) +
  geom_point(shape = 21)


ggplot(penguins, aes(flipper_length_mm, bill_length_mm, color = species)) +
  geom_hdr_lines(xlim = c(160, 240), ylim = c(30, 70)) +
  geom_point(size = 1)

ggplot(penguins, aes(flipper_length_mm, bill_length_mm, fill = species)) +
  geom_hdr(xlim = c(160, 240), ylim = c(30, 70)) +
  geom_point(shape = 21) +
  facet_wrap(vars(species))

set.seed(123)
th <- c(3, 5)
df <- data.frame("x" = rexp(1000, th[1]), "y" = rexp(1000, th[2]))

# construct the likelihood function
l <- function(th) {
  log_liks <- apply(df, 1, function(xy) {
    dexp(xy[1], rate = th[1], log = TRUE) +
      dexp(xy[2], rate = th[2], log = TRUE)
  })
  sum(log_liks)
}

# compute the mle
(th_hat <- optim(c(2, 2), l, control = list(fnscale = -1))$par)
#> [1] 2.912736 5.032125

# construct the parametric density estimate
f <- function(x, y, th) dexp(x, th[1]) * dexp(y, th[2])

# pass estimated density into geom_hdr_fun()
ggplot(df, aes(x, y)) +
  geom_hdr_fun(fun = f, args = list(th = th_hat)) +
  geom_point(shape = 21, fill = "lightgreen", alpha = .25) +
  coord_equal()

p_points <- ggplot(diamonds, aes(carat, price)) +
  geom_point()

p_hdr_points <- ggplot(diamonds, aes(carat, price)) +
  geom_hdr_points()

p_points + p_hdr_points


ggplot(cars, aes(speed, dist)) +
  geom_density_2d() +
  geom_point() +
  geom_rug()


ggplot(cars, aes(speed, dist)) +
  geom_hdr() +
  geom_point(color = "red") +
  geom_hdr_rug(aes(fill = after_stat(probs)), length = unit(.2, "cm"), alpha = 1) + 
  scale_fill_viridis_d(option = "magma", begin = .8, end = 0)

p1 <- ggplot(cars, aes(speed, dist)) +
  geom_hdr() +
  geom_point(color = "red") +
  guides(alpha = "none") +
  ggtitle("Default geom_hdr()")

p2 <- ggplot(cars, aes(speed, dist)) +
  geom_hdr(xlim = c(-20, 50), ylim = c(-40, 140)) +
  geom_point(color = "red") +
  guides(alpha = "none") +
  ggtitle("Manually set xlim, ylim")

p3 <- ggplot(cars, aes(speed, dist)) +
  geom_hdr(xlim = c(-20, 50), ylim = c(-40, 140)) +
  geom_point(color = "red") +
  guides(alpha = "none") +
  scale_y_continuous(breaks = 25*(0:5)) +
  coord_cartesian(xlim = c(4, 25), ylim = c(-1, 120)) + 
  ggtitle("Zoom with coord_cartesian()")

(p1 / p2 / p3) & theme(title = element_text(size = 9))


p_hdr_scale <- ggplot(faithful, aes(eruptions, waiting)) +
  geom_hdr(
    xlim = scales::expand_range(range(faithful$eruptions), mul = .25),
    ylim = scales::expand_range(range(faithful$waiting),   mul = .25)
  ) +
  geom_point(color = "red") +
  scale_x_continuous(breaks = 0:6) +
  scale_y_continuous(breaks = (3:10)*10) +
  guides(alpha = "none")

den <- with(faithful,
            MASS::kde2d(eruptions, waiting, n = 100, lims = c(0,6,30,105))
)

library("hdrcde")
#> This is hdrcde 3.4
p_den <- ~ with(faithful,
                plot(
                  hdr.2d(eruptions, waiting, prob = c(50, 80, 95, 99), den = den),
                  pointcol = "red",
                  show.points = TRUE,
                  xlim = c(0, 6),
                  ylim = c(30, 105)
                )
)

par(mar = c(0,1.75,0,0), bg = NA)

library(gridGraphics)

p_hdr_scale +
  coord_cartesian(xlim = c(0, 6), ylim = c(30, 105), expand = FALSE) +
  wrap_elements(panel = p_den, clip = FALSE)



par(mar = c(3, 3, 1, 1) + 0.1, mfrow = c(1, 2))
with(faithful,
     plot(
       hdr.2d(eruptions, waiting, prob = c(50, 80, 95, 99), 
              kde.package = "ash", xextend = .20),
       pointcol = "red",
       show.points = TRUE,
       xlim = c(0, 6),
       ylim = c(30, 105)
     )
)
with(faithful,
     plot(
       hdr.2d(eruptions, waiting, prob = c(50, 80, 95, 99), 
              kde.package = "ks", xextend = .20),
       pointcol = "red",
       show.points = TRUE,
       xlim = c(0, 6),
       ylim = c(30, 105)
     )
)


library(ggradar)
library(dplyr)
library(scales)
library(tibble)

mtcars_radar <- mtcars %>% 
  as_tibble(rownames = "group") %>% 
  mutate_at(vars(-group), rescale) %>% 
  tail(4) %>% 
  select(1:10)

ggradar(mtcars_radar)

mtcars %>% 
  as_tibble(rownames = "group") %>% 
  mutate_at(vars(-group), rescale) %>% 
  tail(4) %>% 
  select(1:10) %>% 
  ggradar(font.radar = "Circular Air")

library(ggdensity)
library(DataExplorer)
library(correlationfunnel)
library(explore)
library(ggtech)

d <- qplot(carat, data = diamonds[diamonds$color %in%LETTERS[4:7], ], 
           geom = "histogram", bins=30, fill = color)


d + theme_tech(theme="airbnb") + 
  scale_fill_tech(theme="airbnb") + 
  labs(title="Airbnb theme", 
       subtitle="now with subtitles for ggplot2 >= 2.1.0")


d + theme_airbnb_fancy() + 
  scale_fill_tech(theme="airbnb")  + 
  labs(title="Airbnb theme", 
       subtitle="now with subtitles for ggplot2 >= 2.1.0")

d + theme_tech(theme="google") + 
  scale_fill_tech(theme="google") + 
  labs(title="Google theme", 
       subtitle="now with subtitles for ggplot2 >= 2.1.0")


ggplot(aes(x,y), data=d2) + 
  geom_tech(size=0.15, theme="twitter") + 
  theme_tech("twitter") +
  ggtitle("Twitter geom")

