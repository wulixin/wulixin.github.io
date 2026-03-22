




library(lares)
library(qqman)

pro_api<-function (token) {
  http_url <- "http://api.Tushare.pro"
  return(function(api_name, ...) {
    params <- list(token = token, api_name = api_name, params = list(...))
    r <- httr::POST(http_url, body = params, encode = "json")
    res <- httr::content(r, "parsed", "application/json")
    if (is.null(res$data)) {
      return(NULL)
    }
    columns <- res$data$fields
    items <- res$data$items
    df <- as.data.frame(data.table::rbindlist(items))
    colnames(df) <- columns
    return(df)
  }) }



stocks_quote<-function (symbols) 
{
  ret <- noret <- NULL
  qRoot <- paste0("https://query1.finance.yahoo.com/v7/finance/quote?fields=symbol,", 
                  "longName,regularMarketPrice,regularMarketChange,regularMarketTime&formatted=false&symbols=")
  for (i in seq_along(symbols)) {
    z <- fromJSON(paste(qRoot, paste(symbols[i], collapse = ","), 
                        sep = ""))
    if (length(z$quoteResponse$result) > 0) {
      cols <- c("symbol", "quoteType", "regularMarketTime", 
                "regularMarketPrice", "regularMarketChange", 
                "market", "longName")
      if (!"longName" %in% colnames(z$quoteResponse$result)) {
        z$quoteResponse$result$longName <- z$quoteResponse$result$symbol
      }
      z <- select(z$quoteResponse$result, one_of(cols))
      ret <- rbind(ret, z)
    }
    else {
      noret <- rbind(noret, symbols[i])
    }
  }
  if (length(noret) > 0) {
    message(paste("No results for", vector2text(noret)))
  }
  if (length(ret) > 0) {
    colnames(ret) <- c("Symbol", "Type", "QuoteTime", "Value", 
                       "DailyChange", "Market", "SymbolName")
    ret <- data.frame(ret) %>% mutate(QuoteTime = as.POSIXct(.data$QuoteTime, 
                                                             origin = "1970-01-01 00:00:00"))
    row.names(ret) <- NULL
    return(ret)
  }
}

# Multiple quotes at the same time
stocks_quote(c("VTI", "VOO", "TSLA"))

## Not run: 
# CRAN
df <- stocks_hist(symbols = c("VTI", "FB", "FIW"), from = Sys.Date() - 180)
print(head(df))
plot(df)




library(tidyverse)
library(tidyquant)
Ra <- c("AAPL", "GOOG", "NFLX") %>%
  tq_get(get  = "stock.prices",
         from = "2010-01-01",
         to   = "2015-12-31") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra")
Ra


library(Quandl)
Quandl.api_key("5tLBvJCNq1-k9UmG5x22")
library(quantmod)
getSymbols("DEXJPUS", src = "FRED") 
getSymbols("AAPL", src = "yahoo")


#write.csv(datasum,'//Users//wulixin//Desktop//datasum.csv')



library(tidyquant)
library(tidyverse)
data(mpg)

mpg

summary(mpg)


mpg_summarized_tbl<- mpg%>%
  select(-year)%>%
  group_by(class)%>%
  summarise(across(.cols=where(is.numeric),.fns=median,.names="{.col}_median"),
            count=n())%>%
  ungroup()%>%
  mutate(prop=count/sum(count),
         all_groups="all_groups",
         class=fct_reorder(class,prop))

mpg_summarized_tbl%>%
   ggplot(aes(x=class))+
  geom_col(aes(y=prop,
               label=str_glue("{scales::percent(prop)}")))+
  geom_line(aes(y=hwy_median,group=all_groups))+
  geom_point(aes(y=hwy_median,group=all_groups))




transformer_dual_y_axis<-
  

  
  
transformer<-mpg_summarized_tbl %>%
   transformer_dual_y_axis(
     primary_column=prop,
     secondary_column=hwy_median,
     include_y_zero=TRUE)

library(funkyheatmap)

library(funkyheatmap)
library(dplyr, warn.conflicts = FALSE)
library(tibble, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)

data("mtcars")

data <- mtcars %>% 
  rownames_to_column("id") %>%
  arrange(desc(mpg)) %>%
  head(20)

funky_heatmap(data)

  
  
library(forecTheta)

y1 = 2+ 0.15*(1:20) + rnorm(20,2)
y2 = y1[20]+ 0.3*(1:30) + rnorm(30,2)
y =  as.ts(c(y1,y2))
out <- dotm(y, h=10)
plot(out)

library(BayesARIMAX)
set.seed(121)
Y<- arima.sim(list(order = c(1,1,1),ar=0.7,ma=0.4), n = 49)
X=rnorm(50,4,1)
BayesARIMAX(Y,X)

library(tsPI)

pred_StructTS <- predict(StructTS(Nile, type ="level"), n.ahead = 10, se.fit = TRUE)
pred_StructTS <- cbind(pred = pred_StructTS$pred,
                       lwr = pred_StructTS$pred - qnorm(0.975)*pred_StructTS$se,
                       upr = pred_StructTS$pred + qnorm(0.975)*pred_StructTS$se)

set.seed(123)
pred <- struct_pi(Nile, type = "level", n_ahead = 10)

ts.plot(ts.union(Nile,pred_StructTS, pred[,1:3]), col = c(1,2,2,2,3,3,3),
        lty = c(1,1,2,2,1,2,2))

