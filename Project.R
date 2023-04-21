library(limma)
library("readxl")
library(dplyr)

# read the two datasets
data <- read_excel("/Users/dylan/Desktop/d1.xlsx")
data2 <- read_excel("/Users/dylan/Desktop/d2.xlsx")

# select only liver surgery
data = data[data$`sites of surgery`==1,]

# merge datasets based on AccessionNumber of patients
total = inner_join(data, data2, by="AccessionNumber")
#total <- as.data.frame(total)

# -----------FIRST GROUP----------------- 899 samples
# Mutated, Pathogenic = Mutated, Presumed Pathogenic =
# variantdetected = Amplified 
# ---------------------------------------


# -----------SECOND GROUP---------------- 44525 samples
# Wild Type = variantnotdetected = Stable =
# Amplification Not Detected
# ---------------------------------------


# -----------EXCEPTIONS------------------
# Mutated, Presumed Benign (not considerated since it's only 1 sample)
# Mutated, Variant of Unknown Significance (may be genetic and not dangerous)
# High 1 sample, Low 6 sample. What to do with these? Mutation rate (?)
# Intermediate
# Indeterminate = indeterminate
# ---------------------------------------

# trasform into numeric class (1 for mutations, 0 for nonmutations)
total$TestResult[total$TestResult == "Mutated, Pathogenic"] <- 1
total$TestResult[total$TestResult == "Mutated, Presumed Pathogenic"] <- 1
total$TestResult[total$TestResult == "variantdetected"] <- 1
total$TestResult[total$TestResult == "Amplified"] <- 1

total$TestResult[total$TestResult == "Wild Type"] <- 0
total$TestResult[total$TestResult == "variantnotdetected"] <- 0
total$TestResult[total$TestResult == "Stable"] <- 0
total$TestResult[total$TestResult == "Amplification Not Detected"] <- 0
