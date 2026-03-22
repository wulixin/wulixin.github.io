

library(lares)

###########################corr()
data(dft) # Titanic dataset
df <- dft[, 2:5]

# Correlation matrix (without redundancy)
corr(df, half = TRUE)

# Ignore specific column
corr(df, ignore = "Pclass")

# Calculate p-values as well
corr(df, pvalue = TRUE, limit = 1)

# Test when no more than 2 non-missing values
df$trash <- c(1, rep(NA, nrow(df) - 1))
# and another method...
corr(df, method = "spearman")



##############corr_cross()
####给出最强的股票相关性最强的；
####给出最弱的股票相关性最强的；

# Only data with no plot
corr_cross(dft, plot = FALSE, top = 10)

# Show only most relevant results filtered by pvalue
corr_cross(dft, rm.na = TRUE, max_pvalue = 0.05, top = 15)

# Cross-Correlation for certain variables
corr_cross(dft, contains = c("Survived", "Fare"))

# Cross-Correlation max values per category
corr_cross(dft, type = 2, top = NA)


##########################生存率分析为TRUE
#####################生存下来与行业，赛带，概念，成交量的关系；
###################corr_var()
corr_var(dft, Survived, method = "spearman", plot = TRUE, top = 10)

# With plots, results are easier to compare:

# Correlate Survived with everything else and show only significant results
dft %>% corr_var(Survived_TRUE, max_pvalue = 0.05)

# Top 15 with less than 50% correlation and show ranks
dft %>% corr_var(Survived_TRUE, ceiling = .6, top = 15, ranks = TRUE)



###################################################

# Compute a correlation matrix
data(mtcars)
corr <- round(cor(mtcars), 1)
corr

# Compute a matrix of correlation p-values
p.mat <- cor_pmat(mtcars)
p.mat

# Visualize the correlation matrix
# --------------------------------
# method = "square" or "circle"
ggcorrplot(corr)
ggcorrplot(corr, method = "circle")

# Reordering the correlation matrix
# --------------------------------
# using hierarchical clustering
ggcorrplot(corr, hc.order = TRUE, outline.color = "white")

# Types of correlogram layout
# --------------------------------
# Get the lower triangle
ggcorrplot(corr,
           hc.order = TRUE, type = "lower",
           outline.color = "white"
)
# Get the upeper triangle
ggcorrplot(corr,
           hc.order = TRUE, type = "upper",
           outline.color = "white"
)

# Change colors and theme
# --------------------------------
# Argument colors
ggcorrplot(corr,
           hc.order = TRUE, type = "lower",
           outline.color = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726")
)

# Add correlation coefficients
# --------------------------------
# argument lab = TRUE
ggcorrplot(corr,
           hc.order = TRUE, type = "lower",
           lab = TRUE,
           ggtheme = ggplot2::theme_dark(),
)

# Add correlation significance level
# --------------------------------
# Argument p.mat
# Barring the no significant coefficient
ggcorrplot(corr,
           hc.order = TRUE,
           type = "lower", p.mat = p.mat
)
# Leave blank on no significant coefficient
ggcorrplot(corr,
           p.mat = p.mat, hc.order = TRUE,
           type = "lower", insig = "blank"
)

# Changing number of digits for correlation coeffcient
# --------------------------------
ggcorrplot(cor(mtcars),
           type = "lower",
           insig = "blank",
           lab = TRUE,
           digits = 3
)





















