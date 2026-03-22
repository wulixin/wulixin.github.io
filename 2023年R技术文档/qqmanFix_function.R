

library(qqman)
library(lares)
fix(stocks_quote)

function (symbols) 
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


fix(stocks_report)
# Multiple quotes at the same time
stocks_quote(c("VTI", "VOO", "TSLA"))

## Not run: 
# CRAN
df <- stocks_hist(symbols = c("VTI", "FB", "FIW"), from = Sys.Date() - 180)
print(head(df))
plot(df)






fix(pro_api)

edit(pro_api())
function (token) 
{
  http_url <- "http://api.waditu.com"
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
  })
}


