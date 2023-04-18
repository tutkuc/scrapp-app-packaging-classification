#Project - 2 

#packages 
library(dplyr)
library(splitstackshape)
library(nnet) #multinom log reg
library(e1071) #naive bayes?
library(data.table)
library(formattable)
library(tidyr)
library(ggplot2)
library(gplots)
library(utiml) #multi label
library(mldr) #to make dataframes mldr object
library(C50) #c50 decision tree
library(naniar) #missingness plot
library(randomForest)
library(mltest)
library(ROCR) #classification metrics
library(jtools)

#read data
df <- read.csv("df.csv", na.strings=c(""," ","NA"))
df$X <- NULL

###############################
###Exploratory Data Analysis###
###############################

#aggregate less frequent groups
by_type <- df %>% 
  group_by(product_type_id) %>%
  summarise(freq = length(product_type_id))
type_levels_to_agg <- simplify2array(by_type[by_type$freq<=nrow(df)*0.001,1])

by_cont <- df %>% 
  group_by(container_id) %>%
  summarise(freq = length(container_id))
cont_levels_to_agg <- simplify2array(by_cont[by_cont$freq<=nrow(df)*0.001,1])


#function to aggregate certain levels in a certain column
to_aggregate <- function(df, levels, column){
  df_return <- df
  for (i in 1:length(levels)){
    df_return[df_return[column]==levels[i],column] = "aggregate"
  }
  return(df_return)
}

#create the data frame with less levels 
#df_aggregate <- to_aggregate(df, cat_levels_to_agg, "product_category_id")
df_aggregate <- to_aggregate(df, cont_levels_to_agg, "container_id")
df_aggregate <- to_aggregate(df_aggregate, type_levels_to_agg, "product_type_id")


#get rid of size_unit = NA
#19871 -> 19087 rows
df_size_complete <- df_aggregate[!(is.na(df_aggregate$size_unit)),] 

#fix size unit labels
df_size_unit <- df_size_complete
df_size_unit[df_size_unit$size_unit=="fl oz", "size_unit"] <- "fl_oz"
df_size_unit[df_size_unit$size_unit=="mL", "size_unit"] <- "ml"

#factorize columns
cat_cols <- c("product_category_id", "product_type_id", "container_id", "size_unit",
              "material_1", "packaging_part_1",
              "material_2", "packaging_part_2",
              "material_3", "packaging_part_3",
              "material_4", "packaging_part_4",
              "material_5", "packaging_part_5",
              "material_6", "packaging_part_6",
              "material_7", "packaging_part_7")
df_size_unit[cat_cols] <- lapply(df_size_unit[cat_cols], factor) 
df[cat_cols] <- lapply(df[cat_cols], factor) 

#check for uniqueness in packaging columns
packaging_levels <- c("6pr", "aer", "apl", "bag", "bbk", "bdr", "bkg", "bkt", "bli", "bnd", "box", "brp", "btl", "can",
                      "cap", "cfp", "clm", "crd", "crk", "crr", "cse", "ctr", "cup", "dnz", "egg", "env", "flm", "fol",
                      "ftc", "glt", "gtc", "hdl", "hng", "jar", "jug", "lbl", "lid", "net", "pad", "pch", "pki", "pkt", 
                      "plt", "pmp", "pol", "pot", "ppn", "ppr", "rol", "slv", "stk", "str", "stw", "tag", "tbe", "tbg",
                      "tie", "tin", "tnk", "tpe", "tra", "trs", "tub", "win", "wrp")

cols_packaging <- c("packaging_part_1", "packaging_part_2", "packaging_part_3", 
                    "packaging_part_4", "packaging_part_5", "packaging_part_6", 
                    "packaging_part_7")

n_parts <- rep(NA, nrow(df_size_unit))

for (i in 1:nrow(df_size_unit)){
  true_packaging <- df_size_unit[i,cols_packaging]
  true_packaging <- true_packaging[!is.na(true_packaging)]
  n_parts[i] <- length(unique(true_packaging))
}
i_non_unique <- which(!n_parts==df_size_unit$n_parts)
df_unique <- df_size_unit[-i_non_unique,]


#the last dataset before modelling
df_final <- df_unique


#some statistics of the final data
cat("The mean of n_parts:", mean(df_final$n_parts))
cat("The variance of n_parts:", var(df_final$n_parts))

#############
### Plots ###
#############

#plot categories
by_cat <- df %>%
  group_by(product_category_id) %>%
  summarise(freq = length(product_category_id))
ggplot(data=by_cat, aes(x=reorder(product_category_id, -freq), y=freq)) +
  geom_bar(stat="identity") +
  # geom_vline(xintercept = 11.5, linetype="dashed",
  #            color = "blue", size=.5) +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1)) +
  labs(y= "number of products", x = "product categories") +
  theme(text = element_text(size=9))


#plot type
ggplot(data=by_type, aes(x=reorder(product_type_id, -freq), y=freq)) +
  geom_bar(stat="identity") +
  geom_vline(xintercept = 31.5, linetype="dashed", 
             color = "blue", size=.5)  +
  theme(axis.text.x = element_text(angle = 37, vjust = 1, hjust=1)) +
  labs(y= "number of products ", x = "product types") +
  theme(text = element_text(size=8)) 

#plot container
ggplot(data=by_cont, aes(x=reorder(container_id, -freq), y=freq)) +
  geom_bar(stat="identity") +
  geom_vline(xintercept = 41.5, linetype="dashed", 
             color = "blue", size=.5)  +
  theme(axis.text.x = element_text(angle = 37, vjust = 1, hjust=1)) +
  labs(y= "number of products ", x = "container types") +
  theme(text = element_text(size=8)) 

#plot size unit
by_size_unit <- df_final %>% 
  group_by(size_unit) %>%
  summarise(freq = length(size_unit))
ggplot(data=by_size_unit, aes(x=reorder(size_unit, -freq), y=freq)) +
  geom_bar(stat="identity")


#histogram of number of parts 
ggplot(df_final, aes(x=n_parts)) + 
  geom_histogram(color="black", fill="white", binwidth = 1)+
  labs(y="number of products",x="number of packaging parts") +
  geom_vline(xintercept = mean(df_final$n_parts), linetype="dashed", 
             color = "blue", size=.5)

#plot missingness
vis_miss(df_final[c(6:23)])+
  theme(axis.text.x = element_text(angle = -40, vjust = 10, hjust=1)) 

#remove unnecessary variables 
rm(by_cont, by_type, type_levels_to_agg, by_cat, cont_levels_to_agg, by_size_unit)
rm(df, df_aggregate, df_size_complete, df_size_unit, df_unique)
rm(cat_cols, cols_packaging, true_packaging, i_non_unique)


#########################
### No of Parts Model ###
#########################


### 1. Data Preparation ###
#Train-test split the data, to be used in modelling n_parts and packaging_parts
set.seed(0)
df_train <- stratified(df_final, c('product_category_id', 'product_type_id', 'container_id', 'size_unit', 'n_parts'), 0.7)
df_train <- as.data.frame(df_train)
df_test <- df_final[!(df_final$product_id %in% df_train$product_id),]


### 2. Model Fitting ###
#regular poisson
m_poisson <-glm(n_parts ~ product_category_id + product_type_id + container_id + size_unit,
                data = df_train, family = poisson(link="log"))
y_pred_poisson <- predict(m_poisson, df_test, type="response")
sum(round(y_pred_poisson)==df_test[,"n_parts"]) #3953 out of 5628 = 0.6998759


########################
### Packaging Models ###
########################

### 1. Data Preparation ###
packaging_levels <- c("6pr", "aer", "apl", "bag", "bbk", "bdr", "bkg", "bkt", "bli", "bnd", "box", "brp", "btl", "can",
                      "cap", "cfp", "clm", "crd", "crk", "crr", "cse", "ctr", "cup", "dnz", "egg", "env", "flm", "fol",
                      "ftc", "glt", "gtc", "hdl", "hng", "jar", "jug", "lbl", "lid", "net", "pad", "pch", "pki", "pkt", 
                      "plt", "pmp", "pol", "pot", "ppn", "ppr", "rol", "slv", "stk", "str", "stw", "tag", "tbe", "tbg",
                      "tie", "tin", "tnk", "tpe", "tra", "trs", "tub", "win", "wrp")
material_levels <- c("bd", "bh", "bi", "bk", "bo", "bw", "cb", "cc", "cl", "cp", "gb", "gc", "gg", "go", "ma", "mf",
                     "mo", "ms", "oc", "ok", "om", "or", "ot", "ou", "ow", "p1", "p2", "p3", "p4", "p5", "p6", "p7",
                     "pf", "pm", "pu")
cols_packaging <- c("packaging_part_1", "packaging_part_2", "packaging_part_3", 
                    "packaging_part_4", "packaging_part_5", "packaging_part_6", 
                    "packaging_part_7")

df_binary_packaging <- df_final[c(6:9)]
df_binary_packaging[packaging_levels] <- 0

n_parts <- rep(NA, nrow(df_final))

for (i in 1:nrow(df_final)){
  true_packaging <- df_final[i,cols_packaging]
  true_packaging <- true_packaging[!is.na(true_packaging)]
  df_binary_packaging[i,unique(true_packaging)] <- 1
  n_parts[i] <- length(unique(true_packaging))
}


mldr_packaging <- mldr_from_dataframe(df_binary_packaging, labelIndices = c(5:69))
set.seed(0)
d_split <- create_holdout_partition(mldr_packaging, c(train=0.7, test=0.3))


### 2. Models ###
# 1. Binary Relevance

#Random forest
set.seed(123)
packaging_br_rf <- br(d_split$train, "RF")
preds_br_rf_train <- predict(packaging_br_rf, d_split$train)
preds_br_rf_test <- predict(packaging_br_rf, d_split$test)

results_br_rf_train <- multilabel_evaluate(d_split$train, preds_br_rf_train, c("example-based", "macro-F1"))
results_br_rf_test <- multilabel_evaluate(d_split$test, preds_br_rf_test, c("example-based", "macro-F1"))

#Decision Tree
set.seed(13)
packaging_br_dt <- br(d_split$train, "C5.0")
preds_br_dt_train <- predict(packaging_br_dt, d_split$train)
preds_br_dt_test <- predict(packaging_br_dt, d_split$test)

results_br_dt_train <- multilabel_evaluate(d_split$train, preds_br_dt_train, c("example-based", "macro-F1"))
results_br_dt_test <- multilabel_evaluate(d_split$test, preds_br_dt_test, c("example-based", "macro-F1"))


#Naive-Bayes
set.seed(123)
packaging_br_nb <- br(d_split$train, "NB")
preds_br_nb_train <- predict(packaging_br_nb, d_split$train)
preds_br_nb_test <- predict(packaging_br_nb, d_split$test)

results_br_nb_train <- multilabel_evaluate(d_split$train, preds_br_nb_train, c("example-based", "macro-F1"))
results_br_nb_test <- multilabel_evaluate(d_split$test, preds_br_nb_test, c("example-based", "macro-F1"))


#2. Classifier Chains

#Random forest
set.seed(123)
packaging_cc_rf <- cc(d_split$train, "RF")
preds_cc_rf_train <- predict(packaging_cc_rf, d_split$train)
preds_cc_rf_test <- predict(packaging_cc_rf, d_split$test)

results_cc_rf_train <- multilabel_evaluate(d_split$train, preds_cc_rf_train, c("example-based", "macro-F1"))
results_cc_rf_test <- multilabel_evaluate(d_split$test, preds_cc_rf_test, c("example-based", "macro-F1"))

#Decision Tree
set.seed(13)
packaging_cc_dt <- cc(d_split$train, "C5.0")
preds_cc_dt_train <- predict(packaging_cc_dt, d_split$train)
preds_cc_dt_test <- predict(packaging_cc_dt, d_split$test)

results_cc_dt_train <- multilabel_evaluate(d_split$train, preds_cc_dt_train, c("example-based", "macro-F1"))
results_cc_dt_test <- multilabel_evaluate(d_split$test, preds_cc_dt_test, c("example-based", "macro-F1"))


#Naive-Bayes
set.seed(123)
packaging_cc_nb <- cc(d_split$train, "NB")
preds_cc_nb_train <- predict(packaging_cc_nb, d_split$train)
preds_cc_nb_test <- predict(packaging_cc_nb, d_split$test)

results_cc_nb_train <- multilabel_evaluate(d_split$train, preds_cc_nb_train, c("example-based", "macro-F1"))
results_cc_nb_test <- multilabel_evaluate(d_split$test, preds_cc_nb_test, c("example-based", "macro-F1"))

save(preds_br_dt_test,preds_br_dt_train, preds_cc_dt_test, preds_cc_dt_train,
     preds_br_rf_test,preds_br_rf_train, preds_cc_rf_test, preds_cc_rf_train,
     preds_br_nb_test,preds_br_nb_train, preds_cc_nb_test, preds_cc_nb_train,
     file = "packaging_preds.RData")

save(results_br_dt_test,results_br_dt_train, results_cc_dt_test, results_cc_dt_train,
     results_br_rf_test,results_br_rf_train, results_cc_rf_test, results_cc_rf_train,
     results_br_nb_test,results_br_nb_train, results_cc_nb_test, results_cc_nb_train,
     file = "packaging_results.RData")

rm(preds_br_dt_test,preds_br_dt_train, preds_cc_dt_test, preds_cc_dt_train,
   preds_br_rf_test,preds_br_rf_train, preds_cc_rf_test, preds_cc_rf_train,
   preds_br_nb_test,preds_br_nb_train, preds_cc_nb_test, preds_cc_nb_train)

rm(results_br_dt_test,results_br_dt_train, results_cc_dt_test, results_cc_dt_train,
   results_br_rf_test,results_br_rf_train, results_cc_rf_test, results_cc_rf_train,
   results_br_nb_test,results_br_nb_train, results_cc_nb_test, results_cc_nb_train)

#######################
### Material Models ###
#######################

### 1. Data preparation ###
to_single_column <- function(df){
  
  cols_same <- c("product_id", "barcode", "brand", "name", "size", "size_unit", 
                 "product_category_id", "product_type_id", "container_id", "n_parts", 
                 "packaging_parts", "materials")
  cols_packaging <- c("packaging_part_1", "packaging_part_2", "packaging_part_3", 
                      "packaging_part_4", "packaging_part_5", "packaging_part_6", 
                      "packaging_part_7")
  cols_material <- c("material_1", "material_2", "material_3", "material_4", 
                     "material_5", "material_6", "material_7")
  
  df["packaging_parts"] <- NA
  df["materials"] <- NA
  df_return <- df[0,cols_same]
  
  temp = 1
  for (i in 1:nrow(df)){
    
    current_row <- df[i,cols_same]
    current_packaging <- df[i,cols_packaging]
    current_packaging <- current_packaging[!is.na(current_packaging)]
    current_material <- df[i, cols_material]
    current_material <- current_material[!is.na(current_material)]
    
    
    for (j in 1:current_row$n_parts){
      
      df_return <- rbind(df_return[,cols_same], df[i,cols_same])
      df_return[temp, "packaging_parts"] <- current_packaging[j]
      df_return[temp, "materials"] <- current_material[j]
      
      temp = temp + 1
    }
  }
  return(df_return)
}

df_final_long <- to_single_column(df_final)
#factorize new columns
temp <- c("packaging_parts", "materials")
df_final_long[temp] <- lapply(df_final_long[temp], factor) 
set.seed(0)
sample <- sample(c(TRUE, FALSE), nrow(df_final_long), replace=TRUE, prob=c(0.7,0.3))
df_train_long  <- df_final_long[sample, ]
df_test_long   <- df_final_long[!sample, ]


### 2. Model Fitting ###
#Multinomial Logstic Regression#
set.seed(0)
material_model_mnlr <-multinom(materials ~ size_unit + container_id + product_type_id
                               + product_category_id + packaging_parts, data = df_train_long, MaxNWts=10000, maxit=1000)

#Naive Bayes Classification#
set.seed(120)  
material_model_nb <- naiveBayes(formula = materials ~ size_unit + container_id + product_type_id
                                + product_category_id + packaging_parts, data = df_train_long)


#C50 Classification#
material_model_C50 = C5.0(formula = materials ~ size_unit + container_id + product_type_id
                          + product_category_id + packaging_parts, data = df_train_long)


### 3. Results ###
#MNLR
y_pred_mlnr_mat_train_probs <- predict(material_model_mnlr, df_train_long, type="probs")
y_pred_mlnr_mat_test_probs <- predict(material_model_mnlr, df_test_long, type='probs')
y_pred_mlnr_mat_train_response <- predict(material_model_mnlr, df_train_long)
y_pred_mlnr_mat_test_response <- predict(material_model_mnlr, df_test_long)
save(y_pred_mlnr_mat_train_probs, y_pred_mlnr_mat_test_probs,
     y_pred_mlnr_mat_train_response, y_pred_mlnr_mat_test_response, file = "mlnr_material_results.RData")
rm(y_pred_mlnr_mat_train_probs, y_pred_mlnr_mat_test_probs,
   y_pred_mlnr_mat_train_response, y_pred_mlnr_mat_test_response)


#NB
y_pred_nb_mat_train_probs <- predict(material_model_nb, df_train_long, type="raw")
y_pred_nb_mat_test_probs <- predict(material_model_nb, df_test_long, type='raw')
y_pred_nb_mat_train_response <- predict(material_model_nb, df_train_long)
y_pred_nb_mat_test_response <- predict(material_model_nb, df_test_long)
save(y_pred_nb_mat_train_probs, y_pred_nb_mat_test_probs,
     y_pred_nb_mat_train_response, y_pred_nb_mat_test_response, file = "nb_material_results.RData")
rm(y_pred_nb_mat_train_probs, y_pred_nb_mat_test_probs,
   y_pred_nb_mat_train_response, y_pred_nb_mat_test_response)



#C50
y_pred_C50_mat_train_probs = predict(material_model_C50, df_train_long, type = 'prob')
y_pred_C50_mat_test_probs = predict(material_model_C50, df_test_long, type = 'prob')
y_pred_C50_mat_train_response = predict(material_model_C50, df_train_long)
y_pred_C50_mat_test_response = predict(material_model_C50, df_test_long)
save(y_pred_C50_mat_train_probs, y_pred_C50_mat_test_probs, 
     y_pred_C50_mat_train_response, y_pred_C50_mat_test_response, file = "C50_material_results.RData")
rm(y_pred_C50_mat_train_probs, y_pred_C50_mat_test_probs, 
   y_pred_C50_mat_train_response, y_pred_C50_mat_test_response)



###############
### RESULTS ### 
###############

### Packaging ###
#packaging results in a dataframe
load("packaging_results.RData")
df_packaging_results <- data.frame(matrix(NA, nrow=12, ncol=7))

#assign results to dataframe
df_packaging_results[1,] <- round(results_br_dt_train,4)
df_packaging_results[3,] <- round(results_br_rf_train,4)
df_packaging_results[5,] <- round(results_br_nb_train,4)
df_packaging_results[2,] <- round(results_br_dt_test,4)
df_packaging_results[4,] <- round(results_br_rf_test,4)
df_packaging_results[6,] <- round(results_br_nb_test,4)
df_packaging_results[7,] <- round(results_cc_dt_train,4)
df_packaging_results[9,] <- round(results_cc_rf_train,4)
df_packaging_results[11,] <- round(results_cc_nb_train,4)
df_packaging_results[8,] <- round(results_cc_dt_test,4)
df_packaging_results[10,] <- round(results_cc_rf_test,4)
df_packaging_results[12,] <- round(results_cc_nb_test,4)

colnames(df_packaging_results) <- names(results_br_dt_test)
write.csv(df_packaging_results,"results_packaging.csv", row.names = FALSE)


### Material Results ###

#load results
load("mlnr_material_results.RData")
load("nb_material_results.RData")
load("C50_material_results.RData")

#average metrics
#C50-test
table_C50_test <- ml_test(true=df_test_long$materials, predicted=y_pred_C50_mat_test_response,output.as.table = TRUE )
list_C50_test <- ml_test(true=df_test_long$materials, predicted=y_pred_C50_mat_test_response)
df_C50_test <- as.data.frame(table_C50_test)
df_C50_small_test <- df_C50_test[c("precision","recall","F1")]
C50_means_test <- colMeans(df_C50_small_test, na.rm=TRUE) 
#C50-train
table_C50_train <- ml_test(true=df_train_long$materials, predicted=y_pred_C50_mat_train_response,output.as.table = TRUE )
list_C50_train <- ml_test(true=df_train_long$materials, predicted=y_pred_C50_mat_train_response)
df_C50_train <- as.data.frame(table_C50_train)
df_C50_small_train <- df_C50_train[c("precision","recall","F1")]
C50_means_train <- colMeans(df_C50_small_train, na.rm=TRUE) 
#NB-test
table_nb_test <- ml_test(true=df_test_long$materials, predicted=y_pred_nb_mat_test_response,output.as.table = TRUE )
list_nb_test <- ml_test(true=df_test_long$materials, predicted=y_pred_nb_mat_test_response)
df_nb_test <- as.data.frame(table_nb_test)
df_nb_small_test <- df_nb_test[c("precision","recall","F1")]
nb_means_test <- colMeans(df_nb_small_test, na.rm=TRUE) 
#NB-train
table_nb_train <- ml_test(true=df_train_long$materials, predicted=y_pred_nb_mat_train_response,output.as.table = TRUE )
list_nb_train <- ml_test(true=df_train_long$materials, predicted=y_pred_nb_mat_train_response)
df_nb_train <- as.data.frame(table_nb_train)
df_nb_small_train <- df_nb_train[c("precision","recall","F1")]
nb_means_train <- colMeans(df_nb_small_train, na.rm=TRUE) 
#MN-LR-test
table_mlnr_test <- ml_test(true=df_test_long$materials, predicted=y_pred_mlnr_mat_test_response,output.as.table = TRUE )
list_mlnr_test <- ml_test(true=df_test_long$materials, predicted=y_pred_mlnr_mat_test_response)
df_mlnr_test <- as.data.frame(table_mlnr_test)
df_mlnr_small_test <- df_mlnr_test[c("precision","recall","F1")]
mlnr_means_test <- colMeans(df_mlnr_small_test, na.rm=TRUE) 
#MN-LR-train
table_mlnr_train <- ml_test(true=df_train_long$materials, predicted=y_pred_mlnr_mat_train_response,output.as.table = TRUE )
list_mlnr_train  <- ml_test(true=df_train_long$materials, predicted=y_pred_mlnr_mat_train_response)
df_mlnr_train  <- as.data.frame(table_mlnr_train)
df_mlnr_small_train <- df_mlnr_train[c("precision","recall","F1")]
mlnr_means_train  <- colMeans(df_mlnr_small_train, na.rm=TRUE) 

#dataframe results
df_material_results <- data.frame(matrix(NA, nrow=6, ncol=4))
df_material_results[1,] <- c(round(list_mlnr_train$accuracy,4),round(mlnr_means_train,4))
df_material_results[2,] <- c(round(list_mlnr_test$accuracy,4),round(mlnr_means_test,4))
df_material_results[3,] <- c(round(list_C50_train$accuracy,4),round(C50_means_train,4))
df_material_results[4,] <- c(round(list_C50_test$accuracy,4),round(C50_means_test,4))
df_material_results[5,] <- c(round(list_nb_train$accuracy,4),round(nb_means_train,4))
df_material_results[6,] <- c(round(list_nb_test$accuracy,4),round(nb_means_test,4))

colnames(df_material_results) <- c("accuracy","precision","recall","F1" )
write.csv(df_material_results,"results_material.csv", row.names = FALSE)


### Number of Parts ###

#load packaging predictions
load("packaging_preds.RData")
#packaging part model -> n_parts
binary_pred <- as.bipartition(preds_br_rf_test)
pred_n_parts <- rowSums(binary_pred)
#count of models and data
by_part_poisson <- as.data.frame(table(round(y_pred_poisson)))
by_part_pred <- as.data.frame(table(pred_n_parts))
by_part_data <- as.data.frame(table(df_test$n_parts))

ggplot(by_part_data, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", width=0.5) +
  geom_point(data = by_part_pred, 
             mapping = aes(x = pred_n_parts, y = Freq), color="red") +
  labs(y= "count", x = "number of parts") +
  geom_point(data =  by_part_poisson, 
             mapping = aes(x = Var1, y = Freq), color="blue") +
  annotate("text", x=5.85, y=2200, label= "Poisson regression", color="blue") + 
  annotate("text", x=5.85, y=2400, label= "RF-packaging model", color="red")
