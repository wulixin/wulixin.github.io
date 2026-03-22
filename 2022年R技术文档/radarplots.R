
##########雷达图的应用
library(ggradar)
library(radarchart)
data(skillsByName)
data(skills)
head(skillsByName)
library(tidyr)
skillsByLabel <- gather(skillsByName, key=Label, value=Score, -Name) %>%
  spread(key=Name, value=Score)

skillsByLabel

skillsByLabel <- as.data.frame(t(skillsByName[,-1]))
names(skillsByLabel) <- skillsByName$Name


skillsByLabel <- cbind(Label=row.names(skillsByLabel), skillsByLabel)

row.names(skillsByLabel) <- NULL

chartJSRadar(scores = skillsByLabel, maxScale = 20)

library(radarBoxplot)
data("winequality_red")
head(winequality_red)

# Regular
radarBoxplot(quality ~ ., winequality_red)

# Orange and green pattern with grey median
radarBoxplot(quality ~ ., winequality_red,
             use.ggplot2=FALSE, medianLine=list(col="grey"),
             innerPolygon=list(col="#FFA500CC"),
             outerPolygon=list(col=rgb(0,.7,0,0.6)))

# Plot in 2 rows and 3 columns
# change columns order (counter clockwise)
radarBoxplot(quality ~ volatile.acidity + citric.acid +
               residual.sugar + fixed.acidity + chlorides +
               free.sulfur.dioxide + total.sulfur.dioxide +
               density + pH + sulphates + alcohol,
             data = winequality_red,
             mfrow=c(2,3))
