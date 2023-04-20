library(limma)
library("readxl")
library(dplyr)

data <- read_excel("/Users/dylan/Desktop/d1.xlsx")
data2 <- read_excel("/Users/dylan/Desktop/d2.xlsx")
data = data[data$`sites of surgery`==1,]
total = inner_join(data, data2, by="AccessionNumber")
total <- as.data.frame(total)

