prophesize <- function(df, n_future = 60, country = NULL,
                       trend.param = 0.05, logged = FALSE, pout = 0.03,
                       project = "Prophet Forecast") {
  try_require("prophet")
  
  df <- data.frame(df[, c(1, 2)])
  metric <- colnames(df)[2]
  colnames(df) <- c("ds", "y")
  df <- arrange(df, .data$ds)
  if (logged) df$y <- log(df$y)
  
  # Outliers
  df <- df[!rank(-df$y) %in% 1:round(nrow(df) * pout), ]
  
  # Run prophet functions
  m <- prophet(
    yearly.seasonality = TRUE, daily.seasonality = FALSE,
    changepoint.prior.scale = trend.param
  )
  if (!is.null(country)) {
    m <- add_country_holidays(m, country_name = country)
  }
  m <- fit.prophet(m, df)
  future <- make_future_dataframe(m, periods = n_future)
  
  forecast <- predict(m, future)
  forecast$y <- forecast$trend + forecast$additive_terms
  
  p <- plot(m, forecast) + theme_lares() +
    labs(
      y = metric, x = "Dates",
      title = project,
      subtitle = paste("Forecast results for the next", n_future, "days")
    ) +
    scale_y_comma()
  
  plots2 <- prophet_plot_components(m, forecast, render_plot = FALSE)
  plots2 <- lapply(plots2, function(x) x + theme_lares())
  plot2 <- wrap_plots(plots2, ncol = 1) +
    plot_annotation(title = "Forecast components", theme = theme_lares())
  
  return(list(result = forecast, model = m, plot = p, components = plot2))
}
