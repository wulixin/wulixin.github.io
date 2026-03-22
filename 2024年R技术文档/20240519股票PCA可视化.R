

library(highcharter)
hchart(princomp(X, cor = TRUE))%>%hc_add_theme(hc_theme_ffx())%>%
  hc_yAxis( title = list(text = ""),
            reversed = TRUE, offset = -20,tickLength = 0,
            gridLineWidth = 0, 
            minorGridLineWidth = 0,
            labels = list(style = list(fontSize = "15px")) ) %>%
  #hc_tooltip(formatter = fntltp) %>%
  #hc_xAxis(plotLines = list(plotline)) %>%
  hc_title(text = "天网作战系统之主成分分析") %>%
  hc_subtitle(text = "首战即决战，一战定乾坤，干大战定天下——LiXin Wu" ) %>% 
  hc_legend(layout = "horizontal",verticalAlign = "top",align = "left",valueDecimals = 0) %>%
  hc_size(height = 750,width = 800)


fit_pca <- princomp(X, cor = TRUE)

fit_pca


# VISUALIZE PCA ----
# - Visualization with ggplot is covered in DSRB 101-R Week 4

g <- autoplot(object = fit_pca,
  x = 1,
  y = 2,
  # Labels
  data = X,
  label = TRUE,
  label.label = "vehicle",
  label.size = 3,
  loadings.label = TRUE,
  loadings.label.size = 7,
  scale = 0) +
  labs(title = "银行业主成分分析")+
  theme_minimal()

g

plotly::ggplotly(g)

install.packages("scatterplot3d")
library(scatterplot3d)

pc1<-fit_pca$