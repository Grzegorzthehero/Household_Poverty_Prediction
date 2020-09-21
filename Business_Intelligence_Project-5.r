setwd("/Users/willemzents/Desktop/RWork/CS")

library(dplyr)
library(ggplot2)
library(mlbench)
library(caret)
library(party)
library(Boruta)
library(ordinalForest)
library(randomForest)
library(party)

train <- read.csv("train.csv", header = T)


# Data Preprocessing 
##### 
train[,c(3, 5:7, 24:95, 105:132)] <- lapply(train[,c(3, 5:7, 24:95, 105:132)], as.factor) # Converting binary variables to factors

train$Target <- as.ordered(train$Target) # Converting class variable to ordered vector

train$overcrowding <- as.numeric(train$overcrowding) # Converting numerical variables
train$meaneduc <- as.numeric(train$meaneduc)
train$dependency <- as.numeric(train$dependency)
train$edjefe <- as.integer(train$edjefe)
train$edjefa <- as.integer(train$edjefa)
train$bedrooms <- as.integer(train$bedrooms)

correlationMatrix <- cor(train[sapply(train, is.numeric)])
print(correlationMatrix)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
str(train)



train <- train %>% select (-c(SQBescolari, SQBage, SQBhogar_total, SQBedjefe, SQBhogar_nin, SQBovercrowding, SQBdependency, SQBmeaned, agesq, v18q1, rez_esc, tamhog, r4t3, v2a1, qmobilephone)) # Deleting attributes that we will not use and duplicates
#####

# Transforming separate dummies into categorical variables
#####
train <- train %>%
  mutate(wall = case_when(paredblolad == 1 ~ "brick",
         paredzocalo == 1 ~ "socket",
         paredpreb == 1 ~ "prefab",
         pareddes == 1 ~ "waste",
         paredmad == 1 ~ "wood",
         paredzinc == 1 ~ "zinc",
         paredfibras == 1 ~ "fiber",
         paredother == 1 ~ "other"
         ))
train$wall <- as.factor(train$wall)

train <- train %>%
  mutate(floor = case_when(pisomoscer == 1 ~ "ceramic",
                          pisocemento == 1 ~ "cement",
                          pisoother == 1 ~ "other",
                          pisonatur == 1 ~ "natural",
                          pisonotiene == 1 ~ "no",
                          pisomadera == 1 ~ "wood"
  ))
train$floor <- as.factor(train$floor)

train <- train %>%
  mutate(civilstatus = case_when(estadocivil1 == 1 ~ "<10",
                          estadocivil2 == 1 ~ "free_union",
                          estadocivil3 == 1 ~ "married",
                          estadocivil4 == 1 ~ "divorced",
                          estadocivil5 == 1 ~ "separated",
                          estadocivil6 == 1 ~ "widow",
                          estadocivil7 == 1 ~ "single"  ))
train$civilstatus <- as.factor(train$civilstatus)

train <- train %>%
  mutate(status = case_when(parentesco1 == 1 ~ "household_head",
                                 parentesco2 == 1 ~ "spouse",
                                 parentesco3 == 1 ~ "son_daughter",
                                 parentesco4 == 1 ~ "stepson_daughter",
                                 parentesco5 == 1 ~ "son_daughter_inlaw",
                                 parentesco6 == 1 ~ "grandson_daughter",
                                 parentesco7 == 1 ~ "parent",
                                 parentesco8 == 1 ~ "parent_inlaw",
                                 parentesco9 == 1 ~ "sibling",
                                 parentesco10 == 1 ~ "sibling_inlaw",
                                 parentesco11 == 1 ~ "other",
                                 parentesco12 == 1 ~ "other_nonfamily"  ))
train$status <- as.factor(train$status)

train <- train %>%
  mutate(wall_state = case_when(epared1 == 1 ~ "bad",
                            epared2 == 1 ~ "regular",
                            epared3 == 1 ~ "good"
                          ))
train$wall_state <- as.ordered(train$wall_state)
train$wall_state <- ordered(train$wall_state, levels = c("bad", "regular", "good"))

train <- train %>%
  mutate(roof_state = case_when(etecho1 == 1 ~ "bad",
                                etecho2 == 1 ~ "regular",
                                etecho3 == 1 ~ "good"
  ))
train$roof_state <- as.ordered(train$roof_state)
train$roof_state <- ordered(train$roof_state, levels = c("bad", "regular", "good"))

train <- train %>%
  mutate(floor_state = case_when(eviv1 == 1 ~ "bad",
                                eviv2 == 1 ~ "regular",
                                eviv3 == 1 ~ "good"
  ))
train$floor_state <- as.ordered(train$floor_state)
train$floor_state <- ordered(train$floor_state, levels = c("bad", "regular", "good"))

train <- train %>%
  mutate(roof = case_when(techozinc == 1 ~ "zinc",
                          techoentrepiso == 1 ~ "cement",
                          techocane == 1 ~ "natfiber",
                          techootro == 1 ~ "other"
  ))
train$roof <- as.factor(train$roof)


train <- train %>%
  mutate(energy_source = case_when(energcocinar1 == 1 ~ "no_kitchen",
                                   energcocinar2 == 1 ~ "electricity",
                                   energcocinar3 == 1 ~ "gas",
                                   energcocinar4 == 1 ~ "wood/charcoal"
                                   
  ))
train$energy_source <- as.factor(train$energy_source)


train <- train %>%
  mutate(rubbish = case_when(elimbasu1 == 1 ~ "tanker_truck",
                             elimbasu2 == 1 ~ "hollow/burried",
                             elimbasu3 == 1 ~ "burning",
                             elimbasu4 == 1 ~ "unoccupied_space",
                             elimbasu5 == 1 ~ "river/creek/sea",
                             elimbasu6 == 1 ~ "other"
                             
  ))
train$rubbish <- as.factor(train$rubbish)

train <- train %>%
  mutate(education = case_when( instlevel1 == 1 ~ "no_education",
                                instlevel2 == 1 ~ "incomplete_primary",
                                instlevel3 == 1 ~ "complete_primary",
                                instlevel4 == 1 ~ "incomplete_secondary",
                                instlevel5 == 1 ~ "complete_secondary",
                                instlevel6 == 1 ~ "incomplete_technical_secondary",
                                instlevel7 == 1 ~ "complete_technical_secondary",
                                instlevel8 == 1 ~ "undergraduate_graduate",
                                instlevel9 == 1 ~ "postgraduate"
  ))
train$education <- as.ordered(train$education)


train <- train %>%
  mutate(house = case_when( tipovivi1 == 1 ~ "own",
                            tipovivi2 == 1 ~ "own_istallments",
                            tipovivi3 == 1 ~ "rented",
                            tipovivi4 == 1 ~ "precarious",
                            tipovivi5 == 1 ~ "other"
  ))
train$house <- as.factor(train$house)

train <- train %>%
  mutate(region = case_when(lugar1 == 1 ~ "Central",
                             lugar2 == 1 ~ "Chorotega",
                             lugar3 == 1 ~ "Pacifico_Central",
                             lugar4 == 1 ~ "Brunca",
                             lugar5 == 1 ~ "Huetar_Atlantica",
                             lugar6 == 1 ~ "Huetar_Norte"
  ))
train$region <- as.factor(train$region)

train <- train %>%
  mutate(area = case_when( area1 == 1 ~ "urban",
                           area2 == 1 ~ "rural"
  ))
train$area <- as.factor(train$area)

train <- train %>%
  mutate(toilet = case_when( sanitario1 == 1 ~ "no_toilet",
                                sanitario2 == 1 ~ "sewer",
                                sanitario3 == 1 ~ "septic",
                                sanitario5 == 1 ~ "letrine",
                                sanitario6 == 1 ~ "other"
  ))
train$toilet <- as.factor(train$toilet)

sapply(train, class) # Checking classes 

#####

# Creating a new dataset with categorical variables instead of dummies
train_categorical <- train %>% select (-c(paredblolad,	paredzocalo,	paredpreb,	pareddes,	paredmad,	paredzinc,	paredfibras,	paredother,	pisomoscer,	pisocemento,	pisoother,	pisonatur,	pisonotiene,	pisomadera,	techozinc,	techoentrepiso,	techocane,	techootro, sanitario1,	sanitario2,	sanitario3, sanitario5,	sanitario6,	energcocinar1,	energcocinar2,	energcocinar3,	energcocinar4,	elimbasu1,	elimbasu2,	elimbasu3,	elimbasu4,	elimbasu5,	elimbasu6,	epared1,	epared2,	epared3,	etecho1,	etecho2,	etecho3,	eviv1,	eviv2,	eviv3, estadocivil1,	estadocivil2,	estadocivil3,	estadocivil4,	estadocivil5,	estadocivil6,	estadocivil7,	parentesco1,	parentesco2,	parentesco3,	parentesco4,	parentesco5,	parentesco6,	parentesco7,	parentesco8,	parentesco9,	parentesco10,	parentesco11,	parentesco12, instlevel1,	instlevel2,	instlevel3,	instlevel4,	instlevel5,	instlevel6,	instlevel7,	instlevel8,	instlevel9,	tipovivi1,	tipovivi2,	tipovivi3,	tipovivi4,	tipovivi5,	lugar1,	lugar2,	lugar3,	lugar4,	lugar5,	lugar6,	area1,	area2, female))

str(train_categorical)
summary(train_categorical)

# Correlation for categorical variables
#####
table1 <- table(train_categorical_clean1$roof_state, train_categorical_clean1$roof)
table2 <- table(train_categorical_clean1$floor_state, train_categorical_clean1$floor)
table3 <- table(train_categorical_clean1$wall_state, train_categorical_clean1$wall)

chisq.test(table1)
chisq.test(table2)
chisq.test(table3)
#####

# Deleting NAs
#####

sum(is.na(train_categorical))
train_clean <- na.omit(train)
train_categorical_clean <- na.omit(train_categorical)
str(train_categorical_clean)
#####


# Feature Selection
#####
# We select features using the Boruta algorithm

train_categorical_clean1 <- train_categorical_clean %>% select (-c(Id, idhogar, r4h2, r4m3, hogar_total, hhsize, bedrooms, r4h1, r4t1, r4t2))
str(train_categorical_clean1)

boruta_output <- Boruta(Target ~ ., data=train_categorical_clean1, doTrace=2) 

boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed")])  # collect Confirmed and Tentative variables
print(boruta_signif) 
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  # plot variable importance

# Subsetting the data with selected features
#####

train_subset1 <- train_categorical_clean %>% select (c(Target, meaneduc, hogar_nin, overcrowding, r4h3, r4m2, dependency, hogar_adul, edjefe, edjefa, r4m1, house, region))
dim(train_subset1)

summary(train_subset1)
train_subset2 <- train_categorical_clean %>% select (-c(male, planpri, noelec, status, dis, abastaguano, v14a, civilstatus, Id, idhogar))
dim(train_subset2)
#####


#Splitting data into training and test set
#####
set.seed(101) # Setting seed so that same sample can be reproduced in future also
nrow(train_subset1)
0.75*nrow(train_subset1)

sample <- sample(1:9483, 7112, replace = FALSE)
train_data <- train_subset1[sample, ]
test_data  <- train_subset1[-sample, ]
summary(test_data)
summary(train_data)

sample2 <- sample(1:9483, 7112, replace = FALSE)
train_data2 <- train_subset2[sample2, ]
test_data2  <- train_subset2[-sample2, ]
summary(test_data2)
summary(train_data2)

########

write.csv(train_data, file = "train_data.csv")
write.csv(test_data, file = "test_data.csv")
write.csv(train_data2, file = "train_data2.csv")
write.csv(test_data2, file = "test_data2.csv")

#######

RFO1 <- cforest(Target ~ ., data = train_data, control = cforest_unbiased(ntree = 500))  # Training the model 
RFO1_stats <- cforestStats(RFO1)
OOB_Confusion_Matrix <- table(train_data$Target, predict(RFO1,
                           OOB = TRUE))
Confusion_Matrix <-  table(train_data$Target, predict(RFO1, OOB = FALSE))
Confusion_Matrix

RFO2 <- cforest(Target ~ ., data = train_data2, control = cforest_unbiased(ntree = 500)) # Training the model 
cforestStats(RFO2)

summary(train_data$Target)
weightsvector <- c(1/573, 1/1186, 1/871, 1/4482)

table(train_data$Target)
weights=rep(1/sum(train_data$Target=="4"), nrow(train_data))
weights[train_data$Target=="3"] = 1/sum(train_data$Target=="3")
weights[train_data$Target=="2"] = 1/sum(train_data$Target=="2")
weights[train_data$Target=="1"] = 1/sum(train_data$Target=="1")

sum(weights)
table(weights)


RFO3 <- cforest(Target ~ ., data = train_data, control = cforest_unbiased(ntree = 500), weights = weights)  # Training the model with weights
RFO3_stats <- cforestStats(RFO3)
OOB_Confusion_Matrix3 <- table(train_data$Target, predict(RFO3, OOB = FALSE))
Confusion_Matrix4 <- table(test_data$Target, predict(RFO3, newdata = test_data))

RFO4 <- cforest(Target ~ ., data = train_data, control = cforest_unbiased(ntree = 200, mtry = 3), weights = weights)  # Training the model with weights
OOB_Confusion_Matrix4 <- table(train_data$Target, predict(RFO4, OOB = FALSE))
Confusion_Matrix5 <- table(test_data$Target, predict(RFO4, newdata = test_data))


confusionMatrix(Confusion_Matrix5, mode = "prec_recall")
confusionMatrix(OOB_Confusion_Matrix3, mode = "prec_recall")

# Variable importances
#####
source("http://www.ibe.med.uni-muenchen.de/organisation/mitarbeiter/070_drittmittel/janitza/rf_ordinal/novel_vims.txt")

ER_VI <- varimp(RFO4) # error rate based variable importance (standard measure)
RPS_VI <- varimpRPS(RFO4) # RPS-based variable importance (novel VIM)
MAE_VI <- varimpMAE(RFO4) # MAE-based variable importance (novel VIM) 
MSE_VI <- varimpMSE(RFO4) # MSE-based variable importance (existing VIM, but has not been used for ordinal response data) 

par(mfrow = c(1, 1))
barplot(ER_VI, ylab = "Error rate based variable importance", las = 2, col = "aquamarine4")
barplot(RPS_VI, ylab = "RPS-based feature importance", las = 2, col = "coral3")
barplot(MAE_VI, ylab = "MAE-based variable importance", las = 2, col = "goldenrod")
barplot(MSE_VI, ylab = "MSE-based variable importance", las = 2, col = "cornflowerblue")
mtext(side = 3, text = "Variable importance by different measures", outer = TRUE, line = -2)
#####

counts <- table(train_categorical_clean1$Target)
barplot(counts,
        main = "Distribution of Classes",
        xlab = "Class",
        ylab = "Number of Observations",
        col = "darkblue")

summary(train_data)

train_data$overcrowding