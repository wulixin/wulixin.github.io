


saveWorkbook(wb,"//Users//wulixin//Desktop//A股多维度数据策略支持分析.xlsx",overwrite=TRUE)
openXL("//Users//wulixin//Desktop//A股多维度数据策略支持分析.xlsx")

library(officer)
## write to working directory
library(openxlsx)


hs <- createStyle(fontColour = "#ffffff", fgFill = "#4F80BD", halign = "center",
                  valign = "center", textDecoration = "Bold", border = "TopBottomLeftRight", textRotation = 45)

write.xlsx(iris, file = "//Users//wulixin//Desktop//writeXLSX4.xlsx", 
           borders = "rows", headerStyle = hs)

openXL("//Users//wulixin//Desktop//writeXLSX4.xlsx")

write.xlsx(iris, file = "//Users//wulixin//Desktop//writeXLSX5.xlsx", borders = "columns", headerStyle = hs)
openXL("//Users//wulixin//Desktop//writeXLSX5.xlsx")

write.xlsx(iris, "//Users//wulixin//Desktop//writeXLSXTable4.xlsx", asTable = TRUE, headerStyle = createStyle(textRotation = 45))
openXL("//Users//wulixin//Desktop//writeXLSXTable4.xlsx")



########     两个列表
l <- list(IRIS = iris, colClasses = df)
write.xlsx(l, file = "//Users//wulixin//Desktop//writeXLSX6.xlsx", borders = "columns", headerStyle = hs)
write.xlsx(l, file = "//Users//wulixin//Desktop//writeXLSXTable5.xlsx", asTable = TRUE, tableStyle = "TableStyleLight2")

openXL("//Users//wulixin//Desktop//writeXLSX6.xlsx")
openXL("//Users//wulixin//Desktop//writeXLSXTable5.xlsx")


wb <- write.xlsx(iris, "//Users//wulixin//Desktop//writeXLSX6.xlsx")
setColWidths(wb, sheet = 1, cols = 1:5, widths = 30)
saveWorkbook(wb, "//Users//wulixin//Desktop//writeXLSX6.xlsx", overwrite = TRUE)

openXL("//Users//wulixin//Desktop//writeXLSX6.xlsx")




## inspired by xtable gallery
require(ggplot2)

wb <- createWorkbook()

## read historical prices from yahoo finance
prices<-dat

prices$date <- as.Date(ymd(prices$trade_date))
close <- prices$close
prices$logReturns = c(0, log(close[2:length(close)]/close[1:(length(close) - 1)]))

## Create plot of price series and add to worksheet
ggplot(data = prices, aes(as.Date(date), as.numeric(close))) + geom_line(colour = "royalblue2") +
  labs(x = "Date", y = "Price", title = ticker) + 
  geom_area(fill = "royalblue1", alpha = 0.3) + 
  coord_cartesian(ylim = c(min(prices$close) - 1.5, max(prices$close) + 1.5))

## Add worksheet and write plot to sheet
addWorksheet(wb, sheetName = "CBA")
insertPlot(wb, sheet = 1, xy = c("J", 3))

## Histogram of log returns
ggplot(data = prices, aes(x = logReturns)) + geom_bar(binwidth = 0.0025) + labs(title = "Histogram of log returns")

## currency
class(prices$close) <- "currency"  ## styles as currency in workbook

## write historical data and histogram of returns
writeDataTable(wb, sheet = "CBA", x = prices)

insertPlot(wb, sheet = 1, startRow = 25, startCol = "J")

## Add conditional formatting to show where logReturn > 0.01 using default
## style
conditionalFormat(wb, sheet = 1, cols = 1:ncol(prices), 
                  rows = 2:(nrow(prices) +  1), rule = "$H2 > 0.01")

## style log return col as a percentage
logRetStyle <- createStyle(numFmt = "percentage")

addStyle(wb, 1, style = logRetStyle, rows = 2:(nrow(prices) + 1),
         cols = "H", gridExpand = TRUE)

## save workbook to working directory
saveWorkbook(wb, "//Users//wulixin//Desktop//stockPrice.xlsx", overwrite = TRUE)
openXL("//Users//wulixin//Desktop//stockPrice.xlsx")




hs <- createStyle(fontColour = "#ffffff", fgFill = "#4F80BD",
                  halign = "CENTER", textDecoration = "Bold",
                  border = "TopBottomLeftRight", borderColour = "#4F81BD")



## data.frame to write
df <- data.frame(Date = Sys.Date() - 0:4, Logical = c(TRUE, FALSE, TRUE, TRUE, FALSE),
                 Currency = paste("$", -2:2), Accounting = -2:2, hLink = "https://CRAN.R-project.org/",
                 Percentage = seq(-1, 1, length.out = 5), TinyNumber = runif(5)/1e+09, stringsAsFactors = FALSE)

class(df$Currency) <- "currency"
class(df$Accounting) <- "accounting"
class(df$hLink) <- "hyperlink"
class(df$Percentage) <- "percentage"
class(df$TinyNumber) <- "scientific"

## Formatting can be applied simply through the write functions global options
## can be set to further simplify things
options(openxlsx.borderStyle = "thin")
options(openxlsx.borderColour = "#4F81BD")

## create a workbook and add a worksheet
wb <- createWorkbook()
addWorksheet(wb, "writeData auto-formatting")

writeData(wb, 1, df, startRow = 2, startCol = 2)
writeData(wb, 1, df, startRow = 9, startCol = 2, borders = "surrounding")
writeData(wb, 1, df, startRow = 16, startCol = 2, borders = "rows")
writeData(wb, 1, df, startRow = 23, startCol = 2, borders = "columns")
writeData(wb, 1, df, startRow = 30, startCol = 2, borders = "all")

## to change the display text for a hyperlink column just write over those
## cells
writeData(wb, sheet = 1, x = paste("Hyperlink", 1:5), startRow = 17, startCol = 14)


## writing as an Excel Table

addWorksheet(wb, "writeDataTable")
writeDataTable(wb, 2, df, startRow = 2, startCol = 2)
writeDataTable(wb, 2, df, startRow = 9, startCol = 2, tableStyle = "TableStyleLight9")
writeDataTable(wb, 2, df, startRow = 16, startCol = 2, tableStyle = "TableStyleLight2")
writeDataTable(wb, 2, df, startRow = 23, startCol = 2, tableStyle = "TableStyleMedium21")

openXL(wb)  ## opens a temp version


## headerStyles
hs1 <- createStyle(fgFill = "#4F81BD", halign = "CENTER", textDecoration = "Bold",
                   border = "Bottom", fontColour = "blue")

writeData(wb, 1, df, startRow = 16, startCol = 10, headerStyle = hs1, borders = "rows",
          borderStyle = "medium")


## Create a new workbook
wb <- createWorkbook("Ayanami")

## Add some worksheets
addWorksheet(wb, "Sheet 1")
addWorksheet(wb, "Sheet 2")
addWorksheet(wb, "Sheet 3")

## Insert images
img <- system.file("extdata", "einstein.jpg", package = "openxlsx")
insertImage(wb, "Sheet 1", img, startRow = 5, startCol = 3, width = 6, height = 5)
insertImage(wb, 2, img, startRow = 2, startCol = 2)
insertImage(wb, 3, img, width = 15, height = 12, startRow = 3, startCol = "G", units = "cm")

## Save workbook
## Not run: 
saveWorkbook(wb, "//Users//wulixin//Desktop//insertImageExample.xlsx", overwrite = TRUE)
openXL("//Users//wulixin//Desktop//insertImageExample.xlsx")


