#functions

max_val <- 16
min_val <- 1
denormalize <- function(x) {
  x * (max_val - min_val) + min_val
}

factorizator <- function(D){
  library(dplyr)
  D_factorized <- D%>%
    mutate(across(.cols = -all_of(c("AGE","HHNBPERS","HHNBPERS_0_13")), .fns = as.factor)) %>%
    mutate(across(.cols = all_of(c('Q119INCOME','HATLEVEL3')), .fns = ~ factor(.x, levels = unique(.x), ordered = TRUE)))
  D_factorized$Q119INCOME <- factor(D_factorized$Q119INCOME,levels=as.character(1:16))
  
  return(D_factorized)
}

weightedKappa <- function(data,lev=levels(data$obs),model=NULL){
  y_true <- data$obs
  y_pred <- data$pred
  all_class <- union(y_true, y_pred)
  all_class <- sort(all_class)
  newtable <- table(factor(y_true, all_class), factor(y_pred, all_class))
  conf_mat <- caret::confusionMatrix(newtable)
  
  k_linear <- CohenKappa(conf_mat$table,weights = "Equal-Spacing")
  return(c(weightedKappa=k_linear))
}

normalize <- function(x) {
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
}

m_prep <- function(I,inc=T,remove_first_dummy=F){
  data= factorizator(I$dataset)
  
  #1 transform income and educ. level in numerical variables:
  if (inc){
    data$Q119INCOME <- as.numeric(as.character(data$Q119INCOME))
  }
  data$HATLEVEL3 <- as.numeric(as.character(data$HATLEVEL3))
  
  #2 transform binary in dummies:
  data <- data %>%
    mutate(male = ifelse(q101SEX == "1", 1, 0)) %>%
    select(-q101SEX) # Remove the original binary column
  
  #3 Convert Categorical (Not Ordered) Variables to One-Hot Encoding Columns:
  library(fastDummies)
  categorical_vars <- c("BIRTHPLACE", "DISTRICT", "CITIZEN", "MAINSTAT", "JOBSTAT", "JOBISCO2", "FT_PT", "NACE_class", "LGQUEST", "INTMETHOD", "HHTYPE", "migration")
  data <- prenorm <- dummy_cols(data, select_columns = categorical_vars, remove_selected_columns = TRUE,remove_first_dummy = remove_first_dummy)
  
  #4 normalize all the data:
  if (inc){
    data <- data %>% mutate(across(everything(), normalize)) 
  } else {
    data <- data %>% mutate(across(.cols=-Q119INCOME, normalize)) 
  }
  return(list("prep_tot"=data,"non_normalized"=prenorm))
}
