library(limma)
library("readxl")
library(dplyr)
library("dplyr")
library("faux")
library("DataExplorer")
library("caret")
library("randomForest")

# read the two datasets
#data <- read_excel("/Users/dylan/Desktop/d1.xlsx")
#data2 <- read_excel("/Users/dylan/Desktop/d2.xlsx")

# select only liver surgery
#data = data[data$`sites of surgery`==1,]

# merge datasets based on AccessionNumber of patients
#total = inner_join(data, data2, by="AccessionNumber")
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

total <- read.csv("/Users/dylan/Desktop/total.csv")

# trasform into numeric class (1 for mutations, 0 for nonmutations)
total$TestResult[total$TestResult == "Mutated, Pathogenic"] <- 1
total$TestResult[total$TestResult == "Mutated, Presumed Pathogenic"] <- 1
total$TestResult[total$TestResult == "variantdetected"] <- 1
total$TestResult[total$TestResult == "Amplified"] <- 1

total$TestResult[total$TestResult == "Wild Type"] <- 0
total$TestResult[total$TestResult == "variantnotdetected"] <- 0
total$TestResult[total$TestResult == "Stable"] <- 0
total$TestResult[total$TestResult == "Amplification Not Detected"] <- 0

# for now simple 0 and 1 labels (delete all teh others)
total <- subset(total,TestResult!="indeterminate" 
                & TestResult!="Indeterminate"
                & TestResult!="Intermediate"
                & TestResult!="High"
                & TestResult!="Low"
                & TestResult!="Mutated, Variant of Unknown Significance"
                & TestResult!="Mutated, Presumed Benign")

#unique(total$TestResult)
#write.csv(total, file = "tw2f.csv", row.names = FALSE)

# remove categorical columns not useful
finalDataframe <- total[,!names(total) %in% c("PatientFirstName.x"
                                              , "AccessionNumber"
                                              , "PatientDOB.x"
                                              , "ClientSpecimenId.x"
                                              , "patient.ID"
                                              , "startd"
                                              , "endd"
                                              , "dcoff"
                                              , "Users"
                                              , "PatientFirstName.y"
                                              , "PatientLastName"
                                              , "PatientMiddleName"
                                              , "PatientGender"
                                              , "PatientDOB.y"
                                              , "ClientSpecimenId.y"
                                              , "Test"
                                              , "Technology"
                                              , "Conclusion"
                                              , "NGS_PercentMutated"
                                              , "NGS_ProteinChange"
                                              , "mucin.hist")]
# convert biomarkers name with numbers
finalDataframe$Biomarker <- as.numeric(as.factor(finalDataframe$Biomarker))
finalDataframe$os.event <- as.factor(finalDataframe$os.event)

# See how many NA's there are
#sum(is.na(finalDataframe)) --> output: 902 rows with NA's

# plots
plot_intro(finalDataframe)
plot_correlation(finalDataframe) # correlation between target variables and other features

# Recursive Feature Selection

# Define the control using a random forest selection function
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

# Features
x <- subset(finalDataframe, select = -c(os.event))
# target 
target <- finalDataframe$os.event

# Training: 80%; Test: 20%
set.seed(2021)
inTrain <- createDataPartition(target, p = .80, list = FALSE)[,1]
x_train <- x[ inTrain, ]
x_test  <- x[-inTrain, ]

y_train <- target[ inTrain]
y_test  <- target[-inTrain]


# Run RFE

result_rfe1 <- rfe(x = x_train, 
                   y = y_train, 
                   sizes = 10, 
                   rfeControl = control)
result_rfe1
predictors(result_rfe1) #print selected features