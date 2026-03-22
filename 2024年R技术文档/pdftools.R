

install.packages("pdftools")

library(pdftools)
library(httr)
library(tidyverse)

pdf_file <- file.path(R.home("doc"), "NEWS.pdf")
info <- pdf_info(pdf_file)
text <- pdf_text(pdf_file)
fonts <- pdf_fonts(pdf_file)
files <- pdf_attachments(pdf_file)

text
fonts
