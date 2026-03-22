
function (x, get = "stock.prices", complete_cases = TRUE, ...) 
{
  get <- stringr::str_to_lower(get)
  if (length(get) > 1) 
    stop(call. = FALSE, "tq_get(): Please use only one value for `get` source.")
  if ("quandl" %in% get) {
    if (is.null(quandl_api_key())) 
      warning("No Quandl API key detected. Limited to 50 anonymous calls per day. Set key with 'quandl_api_key()'.", 
              call. = FALSE)
  }
  if (stringr::str_detect("alphavantage", get)) {
    if (is.null(tidyquant::av_api_key())) 
      stop("No Alpha Vantager API key detected. Set key with 'av_api_key()'.", 
           call. = FALSE)
  }
  if (stringr::str_detect("tiingo", get)) {
    if (is.null(riingo::riingo_get_token())) 
      stop("No Tiingo API key detected. Set key with 'tiingo_api_key()'.", 
           call. = FALSE)
  }
  if ("rblpapi" %in% get) {
    if (!requireNamespace("Rblpapi", quietly = TRUE)) {
      stop("Rblpapi must be installed to get data from Bloomberg.", 
           call. = FALSE)
    }
    Rblpapi::blpConnect()
  }
  if ("key.stats" %in% get) 
    stop("Yahoo Key Statistics has been discontinued.", call. = FALSE)
  if ("stock.prices.google" %in% get) 
    stop("Google Finance has been discontinued.", call. = FALSE)
  if (stringr::str_detect(get, "tiingo")) {
    ret <- tq_get_base(x, get, complete_cases = complete_cases, 
                       map = FALSE, ...)
    return(ret)
  }
  if (is.character(x) && length(x) == 1 && length(get) == 1) {
    ret <- tq_get_base(x, get, complete_cases = complete_cases, 
                       map = FALSE, ...)
    if (tibble::is_tibble(ret)) 
      ret <- ret %>% tibble::add_column(symbol = x, .before = 1)
  }
  else if (is.character(x)) {
    col_name <- names(x)
    if (is.null(col_name)) 
      col_name <- "symbol"
    x_tib <- tibble::tibble(symbol.. = x)
    ret <- tq_get_map(x = x_tib, get = get, complete_cases = complete_cases, 
                      ...)
    names(ret)[[1]] <- col_name[[1]]
  }
  else if (inherits(x, "data.frame")) {
    if (inherits(x, "grouped_df")) {
      warning("Ungrouping grouped data frame")
      x <- dplyr::ungroup(x)
    }
    col_name <- colnames(x)[[1]]
    names(x)[[1]] <- "symbol.."
    x_tib <- x %>% tibble::as_tibble()
    ret <- tq_get_map(x = x_tib, get = get, complete_cases = complete_cases, 
                      ...)
    names(ret)[[1]] <- col_name[[1]]
  }
  else {
    stop("x must be a single character, list of characters, or data frame of characters with the first column being the object to pass to tq_get.")
  }
  if (length(get) == 1 && (length(x) > 1 || is.data.frame(x))) {
    ret <- tryCatch({
      ret %>% tidyr::unnest(cols = dplyr::one_of(get))
    }, error = function(e) {
      warning("Returning as nested data frame.")
      ret
    })
  }
  return(ret)
}
