

library(portfolioBacktest)
library(ceRtainty)

data("profitSWG")
#
# Computing the CE values, for a RAC range of 0.5-4.0, and Power utility function.
#

# Obtaining the CE table
certainty(data = profitSWG, ival = 0.5, fval = 4, utility = "Power")$CE_values
# Obtaining the RAC vector
certainty(data=profitSWG,ival=.5,fval=4,utility="Power")$RAC
# Performing the CE plot
certainty(data=profitSWG,ival=.5,fval=4,utility="Power")$CE_plot()



# Computing and storing the CE values using Power utility function
# 
ces <- certainty(data = profitSWG, ival = 0.5, fval = 4, utility = "Power")

ces_values  <- ces$CE_values  # store CE table
ces_rac     <- ces$RAC        # store RAC vector

# Computing the RP values respect to SERENADE treatment
premium(tbase = "serenade",ce_data = ces_values, rac = ces_rac, utility = "Power")$PremiumRisk

# Computing the RP values in percentage respect to SERENADE treatment
premium(tbase = "serenade",ce_data = ces_values, rac = ces_rac, utility = "Power")$PremiumRiskPer100

# Plotting the RP absolute values
premium(tbase = "serenade",ce_data = ces_values, rac = ces_rac, utility = "Power")$RP_plot()




rac_generator(data = profitSWG$control, ini = 0.5, fin = 4.0)




library(portfolioBacktest)
data(dataset10)  # load dataset

# define your own portfolio function
uniform_portfolio <- function(dataset) {
  N <- ncol(dataset$adjusted)
  return(rep(1/N, N))
}

# do backtest
bt <- portfolioBacktest(list("Uniform" = uniform_portfolio), dataset10)

# show the summary
bt_sum <- backtestSummary(bt)
names(bt_sum)
bt_sum$performance_summary





# define your own portfolio function
quintile_portfolio <- function(data) {
  X <- diff(log(data$adjusted))[-1]  
  N <- ncol(X)
  ranking <- sort(colMeans(X), decreasing = TRUE, index.return = TRUE)$ix
  w <- rep(0, N)
  w[ranking[1:round(N/5)]] <- 1/round(N/5)
  return(w)
}

# do backtest
bt <- portfolioBacktest(list("Quintile" = quintile_portfolio), dataset10,
                        benchmark = c("uniform", "index"))

# see all performance measures available for the ranking
backtestSummary(bt)$performance

# show leaderboard
leaderboard <- backtestLeaderboard(bt, weights = list("Sharpe ratio"  = 6,
                                                      "max drawdown"  = 1,
                                                      "ROT bps"       = 1,
                                                      "cpu time"      = 1,
                                                      "failure rate"  = 1))
leaderboard$leaderboard_scores



# do backtest
bt <- portfolioBacktest(list("Quintile" = quintile_portfolio), dataset10,
                        benchmark = c("uniform", "index"))

# now we can obtain the table
bt_summary_median <- backtestSummary(bt)
summaryBarPlot(bt_summary_median, measures = c("max drawdown", "annual volatility"))
summaryBarPlot(bt_summary_median, measures = c("max drawdown", "annual volatility"), 
               type = "simple")



# do backtest
bt <- portfolioBacktest(list("Uniform" = uniform_portfolio), dataset10)

# show the backtest results in table
bt_tab <- backtestTable(bt)
bt_tab[c("Sharpe ratio", "max drawdown")]




library(portfolioBacktest)
data(SP500_symbols)
head(SP500_symbols)
# download data from internet
SP500_data <- stockDataDownload(stock_symbols = SP500_symbols, 
                                from = "2008-12-01", to = "2009-12-01")





data(dataset10)
# define your own portfolio function
quintile_portfolio <- function(data) {
  X <- diff(log(data$adjusted))[-1]  
  N <- ncol(X)
  ranking <- sort(colMeans(X), decreasing = TRUE, index.return = TRUE)$ix
  w <- rep(0, N)
  w[ranking[1:round(N/5)]] <- 1/round(N/5)
  return(w)
}

# do backtest
bt <- portfolioBacktest(list("Quintile" = quintile_portfolio), dataset10,
                        benchmark = c("uniform", "index"))

# Now we can plot
backtestBoxPlot(bt, "Sharpe ratio")
backtestBoxPlot(bt, "Sharpe ratio", type = "simple")



