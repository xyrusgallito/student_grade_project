###=======================Libraries and packages
#use this!
suppressWarnings(if(!require("pacman")) install.packages("pacman"))
pacman::p_load('DataExplorer', 'dplyr', 'tidyverse', 'caret', 'rpart',
               'rpart.plot', 'mltools','data.table',  'tidymodels', 
               'vip', 'Hmisc', 'naniar', 'ggplot2', 'psych', 'flextable',
               'ConfusionTableR', 'Boruta', 'pROC')


#install.packages("tidymodels")
#install.packages("vip")
#install.packages("naniar")
#install.packages("DataExplorer")
#install.packages("ConfusionTableR")
# library(DataExplorer)
# library(dplyr)
# library(tidyverse)
# library(caret)
# library(rpart)
# library(rpart.plot)
# library(mltools)
# library(data.table)
# library(tidymodels)
# library(vip)
# library(Hmisc)
# library(naniar)
# library(ggplot2)
# library(psych)
# library(flextable)
# library(Boruta)
# library(ConfusionTableR)

df_raw <- read.csv("student_grades.csv")

#rename "Pass" to "Grade"
names(df_raw)[names(df_raw) == "Pass"] <- "Grade"

dat_f <- df_raw

###==================DATA GLIMPSE========================###
###======================START===========================###

dim(dat_f) #dimension of the dataset
head(dat_f) #upper part of the dataset
tail(dat_f) #lower part of the dataset
View(dat_f)

#using "Hmisc" package (if you want to use this, unload "psych" package first)
#describe(df_raw) 

describe(dat_f) #using "psych" package 

table_1 <- describe(dat_f)

table_1$vars[table_1$vars == "1"] <- "school"
table_1$vars[table_1$vars == "2"] <- "sex"
table_1$vars[table_1$vars == "3"] <- "age"
table_1$vars[table_1$vars == "4"] <- "address"
table_1$vars[table_1$vars == "5"] <- "famsize"
table_1$vars[table_1$vars == "6"] <- "Pstatus"
table_1$vars[table_1$vars == "7"] <- "Medu"
table_1$vars[table_1$vars == "8"] <- "Fedu"
table_1$vars[table_1$vars == "9"] <- "Mjob"
table_1$vars[table_1$vars == "10"] <- "Fjob"
table_1$vars[table_1$vars == "11"] <- "reason"
table_1$vars[table_1$vars == "12"] <- "guardian"
table_1$vars[table_1$vars == "13"] <- "traveltime"
table_1$vars[table_1$vars == "14"] <- "studytime"
table_1$vars[table_1$vars == "15"] <- "failures"
table_1$vars[table_1$vars == "16"] <- "schoolsup"
table_1$vars[table_1$vars == "17"] <- "famsup"
table_1$vars[table_1$vars == "18"] <- "paid"
table_1$vars[table_1$vars == "19"] <- "activities"
table_1$vars[table_1$vars == "20"] <- "nursery"
table_1$vars[table_1$vars == "21"] <- "higher"
table_1$vars[table_1$vars == "22"] <- "internet"
table_1$vars[table_1$vars == "23"] <- "romantic"
table_1$vars[table_1$vars == "24"] <- "famrel"
table_1$vars[table_1$vars == "25"] <- "freetime"
table_1$vars[table_1$vars == "26"] <- "goout"
table_1$vars[table_1$vars == "27"] <- "Dalc"
table_1$vars[table_1$vars == "28"] <- "Walc"
table_1$vars[table_1$vars == "29"] <- "health"
table_1$vars[table_1$vars == "30"] <- "absences"
table_1$vars[table_1$vars == "31"] <- "Grade"

# converting to a more organize table using flextable()
ft <- flextable(table_1)
ft <- add_header_lines(ft, 
                       values = "Table 1. Initial descriptive statistics of the dataset.")
ft <- autofit(ft)
ft

#saving the data as png file
save_as_image(ft, path = "Descriptive_table.png")

#only Age and Absences are numerical, all other features are categorical variables

vis_miss(dat_f)
#no missing data

###======================END=============================###

###===========Exploratory Data Analysis==================###
###======================START===========================###


#Data preprocessing

#Transform all the categorical variables to factor
#Since "age" and "absences" will be dealt with later (scale?)
names_EDA <- c("school", "sex", "address", "famsize", "Pstatus", "Medu", "Fedu",
               "Mjob", "Fjob", "reason", "guardian", "traveltime", "studytime",
               "failures", "schoolsup", "famsup", "paid", "activities", "nursery",
               "higher", "internet", "romantic", "famrel", "freetime", "goout", "Dalc",
               "Walc", "health", "Grade")

dat_f[,names_EDA] <- lapply(dat_f[,names_EDA] , factor)

# plot_intro(dat_RFE)
# plot_bar(dat_RFE)
# plot_correlation(dat_RFE)
# 
# #use these package later!

#data glimpse
plot_intro(dat_f, title = "Figure 1. Glimpse of the dataset.")
plot_bar(dat_f, by = "Grade", by_position = "dodge")

#correlation matrix
plot_correlation(df_raw, title = "Correlation Matrix.")

#Frequency distribution of Sex and Grade

dat_f %>%
  group_by(sex, Grade)%>%
  summarise(count = n())%>%
  ggplot(aes(x=sex,y=count, fill = Grade)) +
  geom_bar(stat = "identity") +
  geom_text(position = 'stack', aes(label = count), vjust = 5) + 
  scale_fill_manual(name = "Grade", labels = c("Fail", "Pass"), 
                    values=c("#599ad3", "#f9a65a")) +
  labs(title="Figure 2. Sex vs Grade", x ="Sex", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
#theme_classic(plot.title = element_text(hjust = 0.5))

#saving the data as png file
ggsave("Figure2.png")

#female tend to fail more than Male students
#there are 208 female students and 187 male students

#age and grade

#boxplot
ggplot(dat_f) +
  aes(x = Grade, y = age) +
  geom_boxplot(fill = "#599ad3") +
  scale_x_discrete(labels=c("1" = "Pass", "0" = "Fail")) +
  labs(title="Figure 3. Age vs Grade", y = "Age") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#saving the data as png file
ggsave("Figure3.png")

#updated distribution shown in a barplot
dat_f %>%
  group_by(age, Grade)%>%
  filter(age <= 19) %>% 
  summarise(count = n())%>%
  ggplot(aes(x=age,y=count, fill = Grade)) +
  geom_bar(stat = "identity") +
  geom_text(position = 'stack', aes(label = count), vjust = 5) + 
  scale_fill_manual(name = "Grade", labels = c("Fail", "Pass"), 
                    values=c("#599ad3", "#f9a65a")) +
  labs(title="Figure 4. Age vs Grade", x ="Age", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#saving the data as png file
ggsave("Figure4.png")

#the are more students passed between the age of 15 to 17 
#but starting from 18 to 19, more students failed
#gradually increase by age 20 and all failed between 21 and 22
#but take note that the number of students from age 19  to 22 are very small

#remove 20-22 ages from the data frame
dat_f <- dat_f[dat_f$age<=19,]

#convert age to categorical variable
dat_f$age <- as.factor(dat_f$age)

#absences and grade

#boxplot
ggplot(dat_f) +
  aes(x = Grade, y = absences) +
  geom_boxplot(fill = "#599ad3") +
  scale_x_discrete(labels=c("1" = "Pass", "0" = "Fail")) +
  labs(title="Figure 5. Absences vs Grade", y = "Absences") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#saving the data as png file
ggsave("Figure5.png")

#scatter plot
dat_f %>%
  group_by(absences, Grade)%>%
  summarise(count = n())%>%
  ggplot(aes(x=absences,y=count, fill = Grade)) +
  geom_point() +
  scale_fill_manual(name = "Grade", labels = c("Fail", "Pass"), 
                    values=c("#599ad3", "#f9a65a")) +
  geom_smooth() +
  labs(title="Figure 6. Abesences vs Grade", x ="Absences", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#saving the data as png file
ggsave("Figure6.png")

#Shapiro-Wilk normality test
shapiro.test(subset(dat_f, Grade == "1")$absences)
shapiro.test(subset(dat_f, Grade == "0")$absences)

#Wilcoxon Test
wilcox.test(dat_f$absences ~ dat_f$Grade)

#failures and grade

#barplot
dat_f %>%
  group_by(failures, Grade)%>%
  summarise(count = n())%>%
  ggplot(aes(x=failures,y=count, fill = Grade)) +
  geom_bar(stat = "identity") +
  geom_text(position = 'stack', aes(label = count), vjust = 5) + 
  scale_fill_manual(name = "Grade", labels = c("Fail", "Pass"), 
                    values=c("#599ad3", "#f9a65a")) +
  labs(title="Figure 7. Failures vs Grade", x ="Number of Failure", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#saving the data as png file
ggsave("Figure7.png")

####=====Hypothesis Testing

#####=====chi square test======###
#drop absences column
drop <- "absences"

dat_hypothesis = dat_f[,!(names(dat_f) %in% drop)]

table_test <- dat_hypothesis %>% 
  summarise_each(funs(chisq.test(., 
                                 dat_hypothesis$Grade)$p.value), -one_of("Grade"))

table_test <- format(table_test, scientific = FALSE) 
table_test2 = t(as.data.table(table_test))
table_test2 = as.data.frame.matrix(table_test2)

table_test2

###=======================END============================###

###==============This is for Feature Engineering=================###

###==============Start of Feature Engineering=================###

#save to new data frame
data_feat_engrng <- dat_f

#Remove school and address:
data_feat_engrng <- data_feat_engrng[, -which(colnames(data_feat_engrng) %in% c("address", "school"))]

###===================GLM=====================###
###==================START====================###
set.seed(9876)
GLM.model <- train(data = data_feat_engrng, Grade ~ ., method = "glm", 
                   trControl = trainControl(method = "cv",
                                            verboseIter = T, number = 10))
#0.6299426 or 63%
###==================END====================###

###===================Decision Tree (DT)=====================###
###========================START=============================###
set.seed(9876)
DT.model = train(Grade ~ ., 
                 data=data_feat_engrng, 
                 method="rpart", 
                 trControl = trainControl(method = "cv", 
                                          verboseIter = T, number = 10))

###========================END=============================###

###======================RANDOM FOREST(RF)=====================###
###==========================START=============================###

set.seed(9876)
RF.model <- train(data = data_feat_engrng, Grade ~ ., 
                  method = "rf", 
                  trControl = trainControl(method = "cv", 
                                           verboseIter = T, number = 10))
###==========================END=============================###

###======================Support Vector Machine(SVM)=====================###
###================================START=================================###

#svmLinear
set.seed(9876)
svm.model_linear <- train(data = data_feat_engrng, Grade ~ ., 
                          method = "svmLinear", 
                          trControl = trainControl(method = "cv", 
                                                   verboseIter = T, number = 10))

###================================END=================================###

###==============================XGBoost(XGB)============================###
###================================START=================================###

set.seed(9876)
xgboost.model <- train(Grade ~., data = data_feat_engrng, method = "xgbTree",
                       trControl = trainControl(method = "cv", 
                                                verboseIter = T,number = 10))

###================================END=================================###

###==============End of Feature Engineering=================###

###==========Feature Extraction/Selection========####

#correlation analysis coefficients

#not applicable since most of the features are categorical
#Correlation analysis is not applicable because 
#the majority of features are categorical.

#Boruta

#saving to new data frame, i will use dat_f
dat_boruta <- dat_f


set.seed(125)
boruta <- Boruta(Grade ~ ., data = dat_boruta, maxRuns = 200) # number of interations

print(boruta)

plot(boruta, las = 2, cex.axis = 0.7) # cex.axis is used to reduce the font size

plotImpHistory(boruta)

bor <- TentativeRoughFix(boruta)

print(bor)

plot(bor, las = 2, cex.axis = 0.7) # cex.axis is used to reduce the font size

imps <- attStats(bor)

imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
print(imps2[order(-imps2$meanImp), ])  # descending sort


####RFE
#saving to another dataframe for RFE
dat_RFE <- dat_f

# Define the control using a random forest selection function
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "cv", # cv
                      number = 10) # number of folds

# Features
x <- dat_RFE %>%
  select(-Grade) %>%
  as.data.frame()

# Target variable
y <- dat_RFE$Grade

# Training: 70%; Test: 30%
set.seed(125)
inTrain <- createDataPartition(y, p = .70, list = FALSE)[,1]

x_train <- x[ inTrain, ]
x_test  <- x[-inTrain, ]

y_train <- y[ inTrain]
y_test  <- y[-inTrain]

# Run RFE
result_rfe1 <- rfe(x = x_train, 
                   y = y_train, 
                   sizes = c(1:31),
                   rfeControl = control)

# Print the results
result_rfe1

#The top 5 variables (out of 25):
#  failures, higher, schoolsup, Fedu, goout

# Print the selected features
predictors(result_rfe1)

# Print the results visually
ggplot(data = result_rfe1, metric = "Accuracy") + theme_bw()
ggplot(data = result_rfe1, metric = "Kappa") + theme_bw()


#variable importance

varimp_data <- data.frame(feature = row.names(varImp(result_rfe1))[1:30],
                          importance = varImp(result_rfe1)[1:30, 1])

#plot for the RFE, update the figure number later
ggplot(data = varimp_data, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(title="Figure 8. Features Selected using RFE",
                                   x = "Features", y = "Variable Importance") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  #scale_fill_manual( values=c("#599ad3", "#f9a65a")) +
  theme_bw() + theme(legend.position = "none",
                     plot.title = element_text(hjust = 0.5),
                     axis.line = element_line(colour = "black"),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     panel.background = element_blank())

# Post prediction
postResample(predict(result_rfe1, x_test), y_test)

#Accuracy     Kappa 
#0.6581197 0.3123714 

###======================END=============================###



###==============This is for Selected Features==============###

###==============Start of Selected Features=================###

#These are the features selected for the models

# 1. failures
# 2. schoolsup
# 3. higher
# 4. goout
# 5. Fedu
# 6  Mjob

#Above selected features will be tested in the models once again:

#Grade ~ failures + schoolsup + Mjob + higher + goout + Fedu

#save the data to new dataframe:

dat_selected_features <- df_raw

#Include only features selected above

dat_selected_features <- dat_selected_features[, which(colnames(dat_selected_features) %in% 
                                                         c("failures", "higher",
                                                           "schoolsup", "Mjob",
                                                           "goout","Fedu", "Grade"))]


#Here we will use dat_selected_features dataframe from the feature_extraction_techniques.R

#let's transform all to factor
names_selected_features <- c(1:7)
dat_selected_features[,names_selected_features] <- lapply(dat_selected_features[,names_selected_features]
                                                          , factor)


str(dat_selected_features)
dim(dat_selected_features)

###===================GLM=====================###
###==================START====================###

set.seed(9876)
GLM.model_selected <- train(data = dat_selected_features, Grade ~ ., method = "glm", 
                            trControl = trainControl(method = "cv",
                                                     verboseIter = T, number = 10))

###==================END====================###

###===================Decision Tree (DT)=====================###
###========================START=============================###
set.seed(9876)
DT.model_selected = train(Grade ~ ., 
                          data=dat_selected_features, 
                          method="rpart", 
                          trControl = trainControl(method = "cv", 
                                                   verboseIter = T, number = 10))
###========================END=============================###

###======================RANDOM FOREST(RF)=====================###
###==========================START=============================###

set.seed(9876)
RF.model_selected <- train(data = dat_selected_features, Grade ~ ., 
                           method = "rf", 
                           trControl = trainControl(method = "cv", 
                                                    verboseIter = T, number = 10))
###==========================END=============================###

###======================Support Vector Machine(SVM)=====================###
###================================START=================================###

#svmLinear
set.seed(9876)
svm.model_linear_selected <- train(data = dat_selected_features, Grade ~ ., 
                                   method = "svmLinear", 
                                   trControl = trainControl(method = "cv", 
                                                            verboseIter = T, number = 10))

###================================END=================================###

###==============================XGBoost(XGB)============================###
###================================START=================================###

set.seed(9876)
xgboost.model_selected <- train(Grade ~., data = dat_selected_features, method = "xgbTree",
                                trControl = trainControl(method = "cv", 
                                                         verboseIter = T,number = 10))

###===================END=================================###  

###==============END of Selected Features=================###

###==============This is for Model Evaluation============###

#save to new data frame

dat_model_eval <- df_raw

#Include only features selected above

dat_model_eval <- dat_model_eval[, which(colnames(dat_model_eval) %in% 
                                           c("failures", "higher",
                                             "schoolsup", "Mjob",
                                             "goout","Fedu", "Grade"))]


#*_ME dataframe name denotes to Model Evaluation

#let's transform all to factor
names_ME <- c(1:7)
dat_model_eval[,names_ME] <- lapply(dat_model_eval[,names_ME], factor)

dat_model_eval2 <- one_hot(as.data.table(dat_model_eval))

###===================Splitting the data=====================###

#70/30
set.seed(9876)
index <- sample(1:length(dat_model_eval$Grade), 
                size = 0.70 * length(dat_model_eval$Grade))
train_set <- dat_model_eval[index, ]
test_set <- dat_model_eval[-index, ]

dim(train_set)
dim(test_set)

###===================GLM=====================###
###==================START====================###
#source
#https://www.guru99.com/r-generalized-linear-model.html

set.seed(9876)
GLM.model_ME <- train(data = train_set, Grade ~ ., method = "glm", 
                      trControl = trainControl(method = "cv",
                                               verboseIter = T, number = 10))

predict_GLM <- predict(GLM.model_ME, test_set, type = 'raw')

predicted_GLM <- cbind(data.frame(class_preds=predict_GLM), test_set)

ConfusionTableR::binary_visualiseR(train_labels = predicted_GLM$class_preds,
                                   truth_labels= predicted_GLM$Grade,
                                   class_label1 = "FAIL", 
                                   class_label2 = "PASS",
                                   quadrant_col1 = "#28ACB4", 
                                   quadrant_col2 = "#4397D2", 
                                   custom_title = "GLM Confusion Matrix", 
                                   text_col= "black") 

bin_cm_GLM <- ConfusionTableR::binary_class_cm(predicted_GLM$class_preds, 
                                               predicted_GLM$Grade)

glimpse(bin_cm_GLM$record_level_cm)

#ROC and AUC
pROC_obj_GLM <- roc(response=test_set$Grade, 
                    predictor= factor(predicted_GLM$class_preds,
                                      ordered = TRUE),
                    smoothed = TRUE,
                    # arguments for ci
                    ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                    # arguments for plot
                    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                    print.auc=TRUE, show.thres=TRUE,
                    main = "GLM ROC")


sens.ci_GLM <- ci.se(pROC_obj_GLM)
plot(sens.ci_GLM, type="shape", col="lightblue")

# definition shape.
plot(sens.ci_GLM, type="bars")

#MCC
#library(mltools)
install.packages("caret")
library(caret)

#calculating the confusion matrix
confusion_matrix <- confusionMatrix(table(predicted_GLM$class_preds, 
                                          predicted_GLM$Grade))

#calculating MCC
TP <- confusion_matrix$table[2, 2]
TN <- confusion_matrix$table[1, 1]
FP <- confusion_matrix$table[2, 1]
FN <- confusion_matrix$table[1, 2]

mcc_value <- ifelse((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN) == 0,
                    0,
                    (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN)))

#[1] 0.3230354

###==================END====================###
###===================Decision Tree (DT)=====================###
###========================START=============================###

set.seed(9876)
DT.model_ME = train(Grade ~ ., 
                    data=train_set, 
                    method="rpart", 
                    trControl = trainControl(method = "cv", 
                                             verboseIter = T, number = 10))

predict_DT <- predict(DT.model_ME, test_set, type = 'raw')

predicted_DT <- cbind(data.frame(class_preds=predict_DT), test_set)

ConfusionTableR::binary_visualiseR(train_labels = predicted_DT$class_preds,
                                   truth_labels= predicted_DT$Grade,
                                   class_label1 = "FAIL", 
                                   class_label2 = "PASS",
                                   quadrant_col1 = "#28ACB4", 
                                   quadrant_col2 = "#4397D2", 
                                   custom_title = "Decision Tree Confusion Matrix", 
                                   text_col= "black") 

bin_cm_DT <- ConfusionTableR::binary_class_cm(predicted_DT$class_preds, 
                                              predicted_DT$Grade)

library(dplyr)
glimpse(bin_cm_DT$record_level_cm)


#ROC and AUC
pROC_obj_DT <- roc(response=test_set$Grade, 
                   predictor= factor(predicted_DT$class_preds,
                                     ordered = TRUE),
                   smoothed = TRUE,
                   # arguments for ci
                   ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                   # arguments for plot
                   plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                   print.auc=TRUE, show.thres=TRUE,
                   main = "Decision Tree ROC")


sens.ci_DT <- ci.se(pROC_obj_DT)
plot(sens.ci_DT, type="shape", col="lightblue")
## Warning in plot.ci.se(sens.ci, type = "shape", col = "lightblue"): Low
## definition shape.
plot(sens.ci_DT, type="bars")

#MCC
#calculating the confusion matrix
confusion_matrix <- confusionMatrix(table(predicted_DT$class_preds, 
                                          predicted_DT$Grade))

#calculating MCC
TP <- confusion_matrix$table[2, 2]
TN <- confusion_matrix$table[1, 1]
FP <- confusion_matrix$table[2, 1]
FN <- confusion_matrix$table[1, 2]

mcc_value <- ifelse((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN) == 0,
                    0,
                    (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN)))

#[1] 0.2500093

###========================END=============================###

###======================RANDOM FOREST(RF)=====================###
###==========================START=============================###

set.seed(9876)
RF.model_ME <- train(data = train_set, Grade ~ ., 
                     method = "rf", 
                     trControl = trainControl(method = "cv", 
                                              verboseIter = T, number = 10))

predict_RF <- predict(RF.model_ME, test_set, type = 'raw')

predicted_RF <- cbind(data.frame(class_preds=predict_RF), test_set)


ConfusionTableR::binary_visualiseR(train_labels = predicted_RF$class_preds,
                                   truth_labels= predicted_RF$Grade,
                                   class_label1 = "FAIL", 
                                   class_label2 = "PASS",
                                   quadrant_col1 = "#28ACB4", 
                                   quadrant_col2 = "#4397D2", 
                                   custom_title = "Random Forest Confusion Matrix", 
                                   text_col= "black") 

bin_cm_RF <- ConfusionTableR::binary_class_cm(predicted_RF$class_preds, 
                                              predicted_RF$Grade)

glimpse(bin_cm_RF$record_level_cm)

#ROC and AUC
pROC_obj_RF <- roc(response=test_set$Grade, 
                   predictor= factor(predicted_RF$class_preds,
                                     ordered = TRUE),
                   smoothed = TRUE,
                   # arguments for ci
                   ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                   # arguments for plot
                   plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                   print.auc=TRUE, show.thres=TRUE,
                   main = "Random Forest ROC")


sens.ci_RF <- ci.se(pROC_obj_RF)
plot(sens.ci_RF, type="shape", col="lightblue")
## Warning in plot.ci.se(sens.ci, type = "shape", col = "lightblue"): Low
## definition shape.
plot(sens.ci_RF, type="bars")

#MCC

#calculating the confusion matrix
confusion_matrix <- confusionMatrix(table(predicted_RF$class_preds, 
                                          predicted_RF$Grade))

#calculating MCC
TP <- confusion_matrix$table[2, 2]
TN <- confusion_matrix$table[1, 1]
FP <- confusion_matrix$table[2, 1]
FN <- confusion_matrix$table[1, 2]

mcc_value <- ifelse((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN) == 0,
                    0,
                    (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN)))

#[1] 0.2682384
###==========================END=============================###

###======================Support Vector Machine(SVM)=====================###
###================================START=================================###

#svmLinear
set.seed(9876)
svm.model_ME <- train(data = train_set, Grade ~ ., 
                      method = "svmLinear", 
                      trControl = trainControl(method = "cv", 
                                               verboseIter = T, number = 10))

predict_SVM <- predict(svm.model_ME, test_set, type = 'raw')

predicted_SVM <- cbind(data.frame(class_preds=predict_SVM), test_set)


ConfusionTableR::binary_visualiseR(train_labels = predicted_SVM$class_preds,
                                   truth_labels= predicted_SVM$Grade,
                                   class_label1 = "FAIL", 
                                   class_label2 = "PASS",
                                   quadrant_col1 = "#28ACB4", 
                                   quadrant_col2 = "#4397D2", 
                                   custom_title = "SVM Confusion Matrix", 
                                   text_col= "black") 

bin_cm_SVM <- ConfusionTableR::binary_class_cm(predicted_SVM$class_preds, 
                                               predicted_SVM$Grade)

glimpse(bin_cm_SVM$record_level_cm)


#ROC and AUC
pROC_obj_SVM <- roc(response=test_set$Grade, 
                    predictor= factor(predicted_SVM$class_preds,
                                      ordered = TRUE),
                    smoothed = TRUE,
                    # arguments for ci
                    ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                    # arguments for plot
                    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                    print.auc=TRUE, show.thres=TRUE,
                    main = "Support Vector Machine ROC")


sens.ci_SVM <- ci.se(pROC_obj_SVM)
plot(sens.ci_SVM, type="shape", col="lightblue")

# definition shape
plot(sens.ci_SVM, type="bars")

#MCC
#calculating the confusion matrix
confusion_matrix <- confusionMatrix(table(predicted_SVM$class_preds, 
                                          predicted_SVM$Grade))

#calculating MCC
TP <- confusion_matrix$table[2, 2]
TN <- confusion_matrix$table[1, 1]
FP <- confusion_matrix$table[2, 1]
FN <- confusion_matrix$table[1, 2]

mcc_value <- ifelse((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN) == 0,
                    0,
                    (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN)))

#[1] 0.3601759

###================================END=================================###

###==============================XGBoost(XGB)============================###
###================================START=================================###

set.seed(9876)
xgboost.model_ME <- train(Grade ~., 
                          data = train_set, method = "xgbTree",
                          trControl = trainControl(method = "cv", 
                                                   verboseIter = T,number = 10))

predict_XGB <- predict(xgboost.model_ME, test_set, type = 'raw')

predicted_XGB <- cbind(data.frame(class_preds=predict_XGB), test_set)


ConfusionTableR::binary_visualiseR(train_labels = predicted_XGB$class_preds,
                                   truth_labels= predicted_XGB$Grade,
                                   class_label1 = "FAIL", 
                                   class_label2 = "PASS",
                                   quadrant_col1 = "#28ACB4", 
                                   quadrant_col2 = "#4397D2", 
                                   custom_title = "XGBoost Confusion Matrix", 
                                   text_col= "black") 

bin_cm_XGB <- ConfusionTableR::binary_class_cm(predicted_XGB$class_preds, 
                                               predicted_XGB$Grade)

glimpse(bin_cm_XGB$record_level_cm)


#ROC and AUC
pROC_obj_XGB <- roc(response=test_set$Grade, 
                    predictor= factor(predicted_XGB$class_preds,
                                      ordered = TRUE),
                    smoothed = TRUE,
                    # arguments for ci
                    ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                    # arguments for plot
                    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                    print.auc=TRUE, show.thres=TRUE,
                    main = "XGBoost ROC")


sens.ci_XGB <- ci.se(pROC_obj_XGB)
plot(sens.ci_XGB, type="shape", col="lightblue")

# definition shape.
plot(sens.ci_XGB, type="bars")

#MCC

#calculating the confusion matrix
confusion_matrix <- confusionMatrix(table(predicted_XGB$class_preds, 
                                          predicted_XGB$Grade))

#calculating MCC
TP <- confusion_matrix$table[2, 2]
TN <- confusion_matrix$table[1, 1]
FP <- confusion_matrix$table[2, 1]
FN <- confusion_matrix$table[1, 2]

mcc_value <- ifelse((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN) == 0,
                    0,
                    (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN)))

#[1] 0.3333413

###===================END=================================###

###==============END of Model Evaluation==================###

###==============This is for Parameter Tuning=============###
###==============START of Parameter Tuning================###

###===================Decision Tree=======================###
###=======================START===========================###


set.seed(9876)
grade_split <- dat_model_eval %>% 
  initial_split(prop = 0.70)
#grade_split <- initial_split(dat_model_eval)
train_dat <- training(grade_split)
test_dat <- testing(grade_split)

tune_spec <- 
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune()
  ) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

tune_spec

tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          levels = 5)

tree_grid %>% 
  count(tree_depth)

set.seed(234)
#set.seed(9876)
DT_train_folds <- vfold_cv(train_dat)


set.seed(345)
#set.seed(9876)
tree_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_formula(Grade ~ .)

tree_res <- 
  tree_wf %>% 
  tune_grid(
    resamples = DT_train_folds,
    grid = tree_grid
  )

tree_res

tree_res %>% 
  collect_metrics()

tree_res %>%
  collect_metrics() %>%
  mutate(tree_depth = factor(tree_depth)) %>%
  ggplot(aes(cost_complexity, mean, color = tree_depth)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  labs(title="Figure 9. Accuracy and ROC_AUC", y = "Age") +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

tree_res %>%
  show_best("accuracy")

best_tree <- tree_res %>%
  select_best("accuracy")

final_wf <- 
  tree_wf %>% 
  finalize_workflow(best_tree)

final_wf

final_tree <- 
  final_wf %>%
  fit(data = train_dat) 

final_tree

final_tree %>% 
  pull_workflow_fit() %>% 
  vip()

final_fit <- 
  final_wf %>%
  last_fit(grade_split) 

final_fit %>%
  collect_metrics()

final_fit %>%
  collect_predictions() %>% 
  roc_curve(Grade, .pred_0) %>% 
  autoplot()

final_tree <- extract_workflow(final_fit)
final_tree


final_tree %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE)

args(decision_tree)


grade_pred <- predict(final_tree, test_dat) %>% 
  bind_cols(predict(final_tree, test_dat, type="raw")) %>% 
  bind_cols(select(test_dat,Grade))

#Confusion Matrix
grade_pred %>% 
  conf_mat(truth = Grade, estimate = .pred_class)

# Evaluate other desired metrics

ConfusionTableR::binary_visualiseR(train_labels = grade_pred$.pred_class,
                                   truth_labels= grade_pred$Grade,
                                   class_label1 = "FAIL", 
                                   class_label2 = "PASS",
                                   quadrant_col1 = "#28ACB4", 
                                   quadrant_col2 = "#4397D2", 
                                   custom_title = "Decision Tree Confusion Matrix with parameter tuning", 
                                   text_col= "black")

bin_cm_DT2 <- ConfusionTableR::binary_class_cm(grade_pred$.pred_class, 
                                               grade_pred$Grade)

glimpse(bin_cm_DT2$record_level_cm)

#MCC

#calculating the confusion matrix
confusion_matrix <- confusionMatrix(table(grade_pred$.pred_class, 
                                          grade_pred$Grade))

#calculating MCC
TP <- confusion_matrix$table[2, 2]
TN <- confusion_matrix$table[1, 1]
FP <- confusion_matrix$table[2, 1]
FN <- confusion_matrix$table[1, 2]

mcc_value <- ifelse((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN) == 0,
                    0,
                    (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN)))

#[1] 0.2915241

###=======================END=============================###
###==============END of Parameter Tuning==================###






