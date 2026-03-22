

library(radarchart)
chartJSRadar(scores=skills)

# Or using a list interface
labs <- c("热度", "知名度", "群众基础", "爆发力",  "活跃度", "投机度")

scores <- list("Rich" = c(9, 7, 4, 5, 3, 7),
               "Andy" = c(7, 6, 6, 2, 6, 9),
               "Aimee" = c(6, 5, 8, 4, 7, 6))

# Default settings
chartJSRadar(scores=scores, labs=labs)

# Fix the max score
chartJSRadar(scores=scores, labs=labs, maxScale=10)

# Fix max and spacing
chartJSRadar(scores=scores, labs=labs, maxScale=12, scaleStepWidth = 2)

# Change title and remove legend
chartJSRadar(scores=scores, labs=labs, main = "Data Science Radar", showLegend = FALSE)

# Add pass through settings for extra options
chartJSRadar(scores=scores, labs=labs, maxScale =10, scaleLineWidth=5)

