##################################
# Define tidy helper functions
##################################
# function to install packages
ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if(length(new.pkg))
    install.packages(mew.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#log transform
log_transform <- function(df) {
  for(i in 1:NCOL(df)) {
    if (min(all_data.num[,i], na.rm = T) <= 0){
      all_data.num[,i] <- all_data.num[,i] + abs(min(all_data.num[,i], na.rm = T)) + 1
      all_dat.num[,i] <- log(all_data.num[,i])
    }
  }
}

# select numeric features
numeric_feat <- function(df){
  df %>%
    keep(is.numeric)
}

# scale dataframe
myscale <- function(x) scale(x, center = T, scale = T)

# compute feature mean by hospital death catgory
grp_mean <- function(df, target, feature){
  df %>% select(target, feature) %>% group_by(as.factor(target)) %>% summarise(grp.mean=mean(feature,na.rm = T))
  
}

# Density plot of selected numeric feature
plot_feature <- function(df, target, feature){
  df %>% 
    ggplot(mapping = aes(feature, fill = as.factor(target))) + 
    geom_density(alpha = I(0.4)) +
    geom_vline(data=mu, aes(xintercept=grp.mean, color=as.factor(target)), linetype="dashed")
}


# running xgb model
xgbModel <- function(data, param){
  xgb.train(params = param
            ,eval_metric = 'aucpr'
            ,data = data
            ,nthread = 8
            ,nrounds = 200
            ,verbose = 1
            ,print_every_n = 100
            ,early_stopping_rounds = 15
            ,watchlist = watchlist
            ,maximize = TRUE)
}

#############################
# Load libraries and read in data
#############################
packages <- c("tidyverse","xgboost","ModelMetrics","Matrix","caret","mice","reshape2","h2o", "moments","e1071")
ipak(packages)


data <- read_csv("training_v2.csv") %>% mutate(data_categ = "training")
new.df <- read_csv("unlabeled.csv") %>% mutate(data_categ = "unlabeled")

# combine and clean together
all_data <- rbind(data, new.df)

# subset character features
data.is.char <- sapply(all_data, is.character)
data.cat <- all_data[,data.is.char]

# Transform character to categorical
data.cat$ethnicity <- as.factor(data.cat$ethnicity)
data.cat$gender <- as.factor(data.cat$gender)
data.cat$hospital_admit_source <- as.factor(data.cat$hospital_admit_source)
data.cat$icu_admit_source <- as.factor(data.cat$icu_admit_source)
data.cat$icu_type <- as.factor(data.cat$icu_type)
data.cat$icu_stay_type <- as.factor(data.cat$icu_stay_type)
data.cat$apache_3j_bodysystem <- as.factor(data.cat$apache_3j_bodysystem)
data.cat$apache_2_bodysystem <- as.factor(data.cat$apache_2_bodysystem)

summary(data.cat)

# one-hot encode categorical features replacing NAs with Other
df.cat <- data.cat %>%
  mutate(eth_AfricanAm = ifelse(ethnicity == "African American" | is.na(ethnicity),1,0),
         eth_Asian = ifelse(ethnicity == "Asian" | is.na(ethnicity),1,0),
         eth_Caucasian = ifelse(ethnicity == "Caucasian" | is.na(ethnicity),1,0),
         eth_Hispanic = ifelse(ethnicity == "Hispanic",1,0) | is.na(ethnicity),
         eth_NativeAm = ifelse(ethnicity == "Native American" | is.na(ethnicity),1,0),
         eth_Other=ifelse(ethnicity == "Other/Unknown" | is.na(ethnicity),1,0),
         
         Male = ifelse(gender == "M" | is.na(gender),1,0),
         hosp_admit_ED = ifelse(hospital_admit_source == "Emergency Department" | is.na(hospital_admit_source),1,0),
         hosp_admit_OR = ifelse(hospital_admit_source == "Operating Room" | is.na(hospital_admit_source),1,0),
         hosp_admit_Flr = ifelse(hospital_admit_source == "Floor" | is.na(hospital_admit_source),1,0),
         hosp_admit_DA = ifelse(hospital_admit_source == "Direct Admit" | is.na(hospital_admit_source),1,0),
         hosp_admit_RR = ifelse(hospital_admit_source == "Recovery Room" | is.na(hospital_admit_source),1,0),
         hosp_admit_Oth = ifelse(hospital_admit_source == "(Other)" | is.na(hospital_admit_source),1,0),
         icu_admit_AE = ifelse(icu_admit_source == "Accident & Emergency" | is.na(icu_admit_source),1,0),
         icu_admit_Flr = ifelse(icu_admit_source == "Floor" | is.na(icu_admit_source),1,0),
         icu_admit_OR = ifelse(icu_admit_source == "Operating Room / Recovery" | is.na(icu_admit_source),1,0),
         icu_admit_OH = ifelse(icu_admit_source == "Other Hospital",1,0),
         icu_admit_OI = ifelse(icu_admit_source == "Other ICU",1,0),
         icu_stay_admit = ifelse(icu_stay_type == "admit",1,0),
         icu_stay_readmit = ifelse(icu_stay_type == "readmit",1,0),
         icu_stay_trans = ifelse(icu_stay_type == "transfer",1,0),
         icu_type_MSICU = ifelse(icu_type  == "Med_Surg ICU",1,0),
         icu_type_MICU = ifelse(icu_type  == "MICU",1,0),
         icu_type_Neuro = ifelse(icu_type == "Neuro ICU",1,0),
         icu_type_CCU = ifelse(icu_type  == "CCU-CTICU",1,0),
         icu_type_SICU = ifelse(icu_type  == "SICU",1,0),
         icu_type_Cardiac = ifelse(icu_type  == "Cardiac ICU",1,0),
         icu_type_Oth = ifelse(icu_type == "(Other)" ,1,0),
         ap_3j_cardio = ifelse(apache_3j_bodysystem == "Cardiovascular"| is.na(apache_3j_bodysystem) ,1,0),
         ap_3j_neuro = ifelse(apache_3j_bodysystem == "Neurological"| is.na(apache_3j_bodysystem) ,1,0),
         ap_3j_sepsis = ifelse(apache_3j_bodysystem == "Sepsis"| is.na(apache_3j_bodysystem) ,1,0),
         ap_3j_respiratory = ifelse(apache_3j_bodysystem == "Gastrointestinal"| is.na(apache_3j_bodysystem) ,1,0),
         ap_3j_other = ifelse(apache_3j_bodysystem == "(Other)"| is.na(apache_3j_bodysystem) ,1,0),
         ap_2_cardio = ifelse(apache_2_bodysystem == "Cardiovascular"| is.na(apache_2_bodysystem) ,1,0),
         ap_2_neuro = ifelse(apache_2_bodysystem == "Neurologic"| is.na(apache_2_bodysystem) ,1,0),
         ap_2_respiratory = ifelse(apache_2_bodysystem == "Respiratory"| is.na(apache_2_bodysystem) ,1,0),
         ap_2_gastrp = ifelse(apache_2_bodysystem == "Gastrointestinal"| is.na(apache_2_bodysystem) ,1,0),
         ap_2_metabolic = ifelse(apache_2_bodysystem == "Metabolic"| is.na(apache_2_bodysystem) ,1,0),
         ap_2_other = ifelse(apache_2_bodysystem == "(Other)"| is.na(apache_2_bodysystem) ,1,0))

# subset one-hot encode features
df.cat.num <- numeric_feat(df.cat)   

# identify zero/near zero variance features
x = nearZeroVar(df.cat.num, saveMetrics = T)
cat_zeroVar <- x[x[,"zeroVar"] >0,] %>% row.names()
# remove zeroVar features from df.cat.num
df.cat.num <- df.cat.num[,!names(df.cat.num) %in% cat_zeroVar]


# NUMERIC FEATURES - select numeric features in the dataset
data.num <-numeric_feat(all_data) 

# ORDINAL FEATURES
ordinal_feat <- data.num %>% 
  select("gcs_eyes_apache","gcs_motor_apache","gcs_verbal_apache") %>%
  mutate(gcs_eyes_apache_1 = ifelse(gcs_eyes_apache == 1,1,0),
         gcs_eyes_apache_2 = ifelse(gcs_eyes_apache == 2,1,0),
         gcs_eyes_apache_3 = ifelse(gcs_eyes_apache == 3,1,0),
         gcs_eyes_apache_4 = ifelse(gcs_eyes_apache == 4,1,0),
         gcs_motor_apache_1 = ifelse(gcs_motor_apache == 1,1,0),
         gcs_motor_apache_2 = ifelse(gcs_motor_apache == 2,1,0),
         gcs_motor_apache_3 = ifelse(gcs_motor_apache == 3,1,0),
         gcs_motor_apache_4 = ifelse(gcs_motor_apache == 4,1,0),
         gcs_motor_apache_5 = ifelse(gcs_motor_apache == 5,1,0),
         gcs_motor_apache_6 = ifelse(gcs_motor_apache == 6,1,0),
         gcs_verbal_apache_1 = ifelse(gcs_verbal_apache == 1,1,0),
         gcs_verbal_apache_2 = ifelse(gcs_verbal_apache == 2,1,0),
         gcs_verbal_apache_3 = ifelse(gcs_verbal_apache == 3,1,0),
         gcs_verbal_apache_4 = ifelse(gcs_verbal_apache == 4,1,0),
         gcs_verbal_apache_5 = ifelse(gcs_verbal_apache == 5,1,0)) %>%
  select(-c("gcs_eyes_apache","gcs_motor_apache","gcs_verbal_apache"))

#replace NAs with zero
ordinal_feat[is.na(ordinal_feat)] <- 0

# impute missing values in height and weight by its median
data.num$height[is.na(data.num$height)] <- median(data.num$height, na.rm = T)
data.num$weight[is.na(data.num$weight)] <- median(data.num$weight, na.rm = T)
data.num$bmi[is.na(data.num$bmi)] <- median(data.num$bmi, na.rm = T)

# remaining non ordinal numeric features
remaining.data.num <- data.num %>% 
  mutate(bmi_healthy = ifelse(bmi <25, 1,0),
         bmi_mod_risk = ifelse(bmi <= 29, 1,0),
         bmi_high_risk = ifelse(bmi >=30, 1,0),
         creatinine_xablow = ifelse(creatinine_apache <= 1.1, 1,0),
         creatinine_ablow = ifelse(creatinine_apache <= 1.29, 1,0),
         creatinine_normal = ifelse(creatinine_apache <= 1.4, 1,0),
         creatinine_abhigh = ifelse(creatinine_apache <= 3.4, 1,0),
         creatinine_xabhigh = ifelse(creatinine_apache > 3.4, 1,0),
         potassium_xablow = ifelse(d1_potassium_min <= 2.5, 1,0),
         potassium_ablow = ifelse(d1_potassium_min <= 3.4, 1,0),
         potassium_normal = ifelse(d1_potassium_min >= 3.5 && d1_potassium_max <= 5.4, 1,0),
         potassium_abhigh = ifelse(d1_potassium_max <= 6.9, 1,0),
         potassium_xabhigh = ifelse(d1_potassium_max >= 7, 1,0),
         sodium_xablow = ifelse(sodium_apache <= 110, 1,0),
         sodium_ablow = ifelse(sodium_apache <= 129, 1,0),
         sodium_normal = ifelse(sodium_apache <= 149, 1,0),
         sodium_abhigh = ifelse(sodium_apache <= 179, 1,0),
         sodium_xabhigh = ifelse(sodium_apache >= 180, 1,0),
         resprate_xablow = ifelse(resprate_apache <= 5, 1,0),
         resprate_ablow = ifelse(resprate_apache <= 11, 1,0),
         resprate_normal = ifelse(resprate_apache <= 24, 1,0),
         resprate_abhigh = ifelse(resprate_apache <= 49, 1,0),
         resprate_xabhigh = ifelse(resprate_apache >= 50, 1,0),
         heart_rate_xablow = ifelse(heart_rate_apache <= 39, 1,0),
         heart_rate_ablow = ifelse(heart_rate_apache <= 54, 1,0),
         heart_rate_normal = ifelse(heart_rate_apache <= 109, 1,0),
         heart_rate_abhigh = ifelse(heart_rate_apache <= 179, 1,0),
         heart_rate_xabhigh = ifelse(heart_rate_apache <= 180, 1,0),
         ph_apache_xablow = ifelse(ph_apache < 7.15, 1,0),
         ph_apache_ablow = ifelse(ph_apache <= 7.32, 1,0),
         ph_apache_normal = ifelse(ph_apache <= 7.49, 1,0),
         ph_apache_abhigh = ifelse(ph_apache <= 7.69, 1,0),
         ph_apache_xabhigh = ifelse(ph_apache >= 7.7, 1,0),
         map_apache_xablow = ifelse(map_apache <= 49, 1,0),
         map_apache_ablow = ifelse(map_apache <= 69, 1,0),
         map_apache_normal = ifelse(map_apache <= 109, 1,0),
         map_apache_abhigh = ifelse(map_apache <= 159, 1,0),
         map_apache_xabhigh = ifelse(map_apache >= 160, 1,0),
         hematocrit_apache_xablow = ifelse(hematocrit_apache <= 20, 1,0),
         hematocrit_apache_ablow = ifelse(hematocrit_apache <= 29.9, 1,0),
         hematocrit_apache_normal = ifelse(hematocrit_apache <= 45.9, 1,0),
         hematocrit_apache_abhigh = ifelse(hematocrit_apache <= 50.9, 1,0),
         hematocrit_apache_xabhigh = ifelse(hematocrit_apache >= 60, 1,0),
         temp_apache_xablow = ifelse(temp_apache <= 29, 1,0),
         temp_apache_ablow = ifelse(temp_apache <= 35.9, 1,0),
         temp_apache_normal = ifelse(temp_apache <= 38.4, 1,0),
         temp_apache_abhigh = ifelse(temp_apache <= 40.9, 1,0),
         temp_apache_xabhigh = ifelse(temp_apache >= 41,1, 0),
         wbc_apache_xablow = ifelse(wbc_apache < 1, 1,0),
         wbc_apache_ablow = ifelse(wbc_apache <= 2.9, 1,0),
         wbc_apache_normal = ifelse(wbc_apache <= 14.9, 1,0),
         wbc_apache_abhigh = ifelse(wbc_apache <= 39.9, 1,0),
         wbc_apache_xabhigh = ifelse(wbc_apache >= 40, 1,0),
         age_44_below = ifelse(age <= 44,1,0),
         age_45_to_54 = ifelse(age >= 45 && age <= 54,1,0),
         age_55_to_64 = ifelse(age >= 55 && age <= 64,1,0),
         age_65_to_74 = ifelse(age >= 65 && age <= 74,1,0),
         age_75_above = ifelse(age >= 75,1,0)) %>%
  select(-c("gcs_eyes_apache","gcs_motor_apache","gcs_verbal_apache","age","wbc_apache","temp_apache","hematocrit_apache","map_apache","ph_apache","heart_rate_apache","resprate_apache","sodium_apache","d1_potassium_min","creatinine_apache","bmi"))

# identify zero/near zero variance features
x2 = nearZeroVar(remaining.data.num, saveMetrics = T)
cat_zeroVar2 <- x2[x2[,"zeroVar"] >0,] %>% row.names()
# remove zeroVar features from remaining.num
remaining.num <- remaining.data.num[,!names(remaining.data.num) %in% cat_zeroVar2]

# separate binary features - this will not be tranformed
binary_features <- sapply(remaining.num, function(x) ifelse((nlevels(as.factor(x)) == 2), 'bin', NA)) 
binary_features <- names(binary_features[!is.na(binary_features)])

bin.feature_df <- remaining.num[, names(remaining.num) %in% binary_features] 
#replace NAs with zero
bin.feature_df[is.na(bin.feature_df)] <- 0

# numeric features for transformation 
num.features <- remaining.num[, !names(remaining.num) %in% names(bin.feature_df)] %>% select(-c("encounter_id","patient_id","hospital_id","icu_id"))

# log remaining num features
for (i in 1:NCOL(num.features)) {
  if(min(num.features[,i], na.rm = T) <= 0) {
    # shift minimum to be one
    num.features[,i] <- num.features[,i] + abs(min(num.features[,i], na.rm = T)) + 1
  } 
    num.features[,i] <- log(num.features[,i])
}

 #scale the numeric features after log tranformation
num.features.log_scaled <- as.data.frame(apply(num.features,2, myscale))


# COMBINE ALL FEATURES
all_data_clean <- cbind(bin.feature_df, ordinal_feat, df.cat.num,num.features.log_scaled)

# separate training from unlabeled
training <- all_data_clean[ 1:91713,]
unlabeled <- all_data_clean[91714:131021,]

set.seed(12345)
index <- createDataPartition(training$hospital_death, p=0.9, list=FALSE)
df.train_test <- training[index,]
df.validation <-  training[-index,]

#split further train_test to 0.9 train and 0.1 test 
set.seed(12345)
index2 <- createDataPartition(df.train_test$hospital_death, p=0.9, list=FALSE)
df.train <- df.train_test[index2,]
df.test <-  df.train_test[-index2,]

table(as.factor(df.train$hospital_death))
table(as.factor(df.test$hospital_death))
table(as.factor(df.validation$hospital_death))

# Create output vector
train.label <- df.train$hospital_death
test.label <- df.test$hospital_death
validation.label <- df.validation$hospital_death

# One hot encoding to generate sparse matrix
options(na.action='na.pass')
sparse.train <- sparse.model.matrix(hospital_death ~. -1, data = df.train)
sparse.test <- sparse.model.matrix(hospital_death ~. -1, data =  df.test)
sparse.validation <- sparse.model.matrix(hospital_death ~. -1, data =  df.validation)                                    

# Weight rows by the inverse of frequency to counteract the bias on the imbalance class
class.freq.train <- table(df.train$hospital_death) %>% prop.table
row.wt.train <- ifelse(df.train$hospital_death == 0, 1/class.freq.train[1], 1/class.freq.train[2])

# Create matrix for xgboosting
dtrain <- xgb.DMatrix(data=sparse.train, label = train.label, weight = row.wt.train)
dtest <- xgb.DMatrix(data=sparse.test, label = test.label)
dvalidation <- xgb.DMatrix(data=sparse.validation, label = validation.label)

# Random parameter search
watchlist <- list(train = dtrain, test = dtest)

set.seed(12345)

best.param = list
best.aucpr.train = 0
best.aucpr.test = 0

# Random parameter search maximising CV aucpr
for (iter in 1:3) {
  set.seed(12345)
  param = list(
    objective = 'binary:logistic',
    max_depth = sample(3:15,1),
    eta = round(runif(1, 0.001, 0.1), 4),
    gamma = round(runif(1,2,20), 0),
    subsample = round(runif(1, 0.5, 0.8),2),
    colsample_bytree = round(runif(1,0.3,0.6), 2),
    min_child_weight = sample(1:7, 1),
    max_delta_step = sample(1:10,1),
    alpha = round(runif(1,1,100),1),
    lambda = round(runif(1,1,100),1)
  )
  
  cat("Iteration", iter, " for random search. \n")
  
  
  # run model on train
  xgb.model = xgb.cv(params = param
                     ,data = dtrain
                     ,eval_metric = 'aucpr'
                     ,nthread = 10
                     ,nrounds = 100
                     ,verbose = 1
                     ,print_every_n = 10
                     ,early_stopping_rounds = 15
                     ,maximize = T
                     ,nfold = 5
                     ,stratified = T)
  
  train.aucpr <- xgb.model$evaluation_log[xgb.model$niter]$train_aucpr_mean
  test.aucpr <- xgb.model$evaluation_log[xgb.model$niter]$test_aucpr_mean
  
  # Check that model is not overfitted
  if((test.aucpr > best.aucpr.test)&(abs(test.aucpr - train.aucpr) < 0.5)){
    best.aucpr.train = train.aucpr
    best.aucpr.test  = test.aucpr
    best.param       = param
  }
  
  cat("", sep = "\n\n")
  
}

best.param
best.aucpr.train
best.aucpr.test

# Train the model with the new hyperparameters
watchlist <- list(train = dtrain, test = dtest)
set.seed(12345)
xgb.Model <- xgbModel(dtrain, best.param)

# Get the important features
names = dimnames(sparse.train)[[2]]
importance_matrix <- xgb.importance(names, model=xgb.Model)[1:20]
important_features <- importance_matrix %>% select(Feature,Gain)
important_features

# Calculate predictions
pred.train <- predict(xgb.Model, dtrain)
pred.test <- predict(xgb.Model, dtest)
pred.validation <- predict(xgb.Model, dvalidation)

# Identify threshold that optimises F1 score
best.threshold = 0
best.f1 = 0
best.precision = 0
best.recall = 0

for (i in 1:100){
  pred.prob <- pred.test
  threshold <- i/100
  pred.label <- ifelse(pred.prob >= threshold, 1,0)
  
  #calculate model metrics
  precision <- ModelMetrics::precision(as.numeric(test.label), as.numeric(pred.label))
  recall <- ModelMetrics::recall(as.numeric(test.label), as.numeric(pred.label)) 
  f1_score <- ModelMetrics::f1Score(as.numeric(test.label), as.numeric(pred.label))
  
  #evaluate f1 score
  if(f1_score > best.f1) {
    best.f1 <- f1_score
    best.threshold <- threshold
    best.recall <- recall
    best.precision <- precision
  }
  
}

best.threshold
best.f1
best.recall
best.precision

# Generate predicted labels
pred.label.train <- ifelse(pred.train >= best.threshold, 1, 0)
pred.label.test <- ifelse(pred.test >= best.threshold, 1, 0)
pred.label.validation <- ifelse(pred.validation >= best.threshold, 1, 0)

# Generate model performance metrics

test.cm <- caret::confusionMatrix(as.factor(pred.label.test), as.factor(test.label), positive = "1")
xgb.auc.test <- ModelMetrics::auc(as.numeric(test.label), as.numeric(pred.label.test))
xgb.gini.test <- (2*xgb.auc.test) - 1

validation.cm <- caret::confusionMatrix(as.factor(pred.label.validation), as.factor(validation.label), positive = "1")
xgb.auc.validation <- ModelMetrics::auc(as.numeric(validation.label), as.numeric(pred.label.validation))
xgb.gini.validation <- (2*xgb.auc.validation) - 1


# SCORE UNLABELED DATA
glimpse(unlabeled)

# sparse and DMatrix on new data
newdata.label  <- unlabeled$hospital_death
sparse.newdata <- sparse.model.matrix(hospital_death ~. -1, data =  unlabeled)
dm.new <- xgb.DMatrix(data=sparse.newdata, label = newdata.label)


# Predict new labels
pred.newdata <- predict(xgb.Model, dm.new) %>% data.frame()

# attach encounter_id
pred.newdata.id <- cbind(new.df$encounter_id,pred.newdata) %>% data.frame()
colnames(pred.newdata.id) <- c("encounter_id","hospital_death")

head(pred.newdata.id,5)
write.csv(pred.newdata.id, file = "solution30.csv",row.names = F)
