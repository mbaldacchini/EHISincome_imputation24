# algorithms funcitons - Mauro Baldacchini - baldacchinimauro@gmail.com
source("needed_functions.R")
# Random Forest: -----------------------
#   DEFAULT HYPERPARAMETER VERSION
rf_std_impute <- function(I){
  D <- I$dataset
  D <- factorizator(D)
  rf <- missForest(D)
  rf_data <- rf$ximp
  imp <- as.numeric(rf_data$Q119INCOME[I$indexes])
  
  all_class <- union(I$original,imp)
  all_class <- sort(all_class)
  newtable <- table(factor(I$original, all_class), factor(imp, all_class))
  conf_mat <- caret::confusionMatrix(newtable)
  
  k_linear <- CohenKappa(conf_mat$table,weights = "Equal-Spacing")
  k_squared <- CohenKappa(conf_mat$table,weights = "Fleiss-Cohen")
  performance <- conf_mat$byClass
  macro_f1 <- mean(performance[,"F1"])
  resul <- list("imp_dataset" = rf_data, "oob"=rf$OOBerror, "imp_values"=imp, "wk_linear"=k_linear, "wk_squared"=k_squared, "macro_F1_score" = macro_f1  ,"performance" = performance, "original_values"=I$original,"conf_matrix"=conf_mat$table)
  return(resul)
}

# CROSS VALIDATED VERSION -----------------------

rf_cv_impute <- function(I,m_list = 5:15,n_tree = 600){
  D <- I$dataset
  D <- factorizator(D)
  D_train <- D[complete.cases(D),]
  
  tunegrid <- expand.grid(mtry=m_list)
  
  
  fitControl <- trainControl(method = "cv",
                             number = 5,
                             summaryFunction = weightedKappa)
  
  
  fit <-  train(Q119INCOME ~., data=D_train,
                method="rf", 
                metric = "weightedKappa",
                tuneGrid=tunegrid,
                trControl = fitControl,
                ntree = n_tree)
  m_best <- fit$bestTune$mtry
  
  rf <- missForest(D,mtry = m_best , ntree = n_tree)
  rf_data <- rf$ximp
  imp <- as.numeric(rf_data$Q119INCOME[I$indexes])
  
  
  all_class <- union(I$original,imp)
  all_class <- sort(all_class)
  newtable <- table(factor(I$original, all_class), factor(imp, all_class))
  conf_mat <- caret::confusionMatrix(newtable)
  
  k_linear <- CohenKappa(conf_mat$table,weights = "Equal-Spacing")
  k_squared <- CohenKappa(conf_mat$table,weights = "Fleiss-Cohen")
  
  performance <- conf_mat$byClass
  macro_f1 <- mean(performance[,"F1"])
  
  resul <- list("imp_dataset" = rf_data, "oob"=rf$OOBerror, "imp_values"=imp, "wk_linear"=k_linear, "wk_squared"=k_squared, "macro_F1_score" = macro_f1  ,"performance" = performance, "original_values"=I$original,"conf_matrix"=conf_mat$table,"best_params"=fit$bestTune)
  return(resul)
}
## XGBoost, CROSS VALIDATED VERSION (for the default parameters we used this function with the default grid) -----------------------

xgb_cv_impute <- function(I,tune_grid = expand.grid(
  nrounds = seq(from = 200, to = 600, by = 100),
  eta = c( 0.05, 0.1, 0.3),
  max_depth = seq(2,12,by=2),
  gamma =0
  ,colsample_bytree = 1,
  min_child_weight = 1:2,
  subsample = 1
)){
  i <- m_prep(I)
  D <- i$non_normalized
  tr <- D[complete.cases(D),]
  te <- D[!complete.cases(D),]
  
  input_x <- as.matrix(select(tr , -Q119INCOME))
  input_y <- tr$Q119INCOME
  
  tune_control <- caret::trainControl(
    method = "cv", 
    number = 5, 
    verboseIter = FALSE, 
    allowParallel = TRUE 
    ,summaryFunction = weightedKappa
  )
  
  xgb_tune <- caret::train(
    x = input_x,
    y = input_y,
    trControl = tune_control,
    tuneGrid = tune_grid,
    method = "xgbTree",
    metric = "weightedKappa",
    verbose = TRUE
  )
  
  d <- factorizator(I$dataset)
  imp_data <- mixgb(d,xgb.params = list(max_depth = xgb_tune$bestTune$max_depth,
                                        gamma = xgb_tune$bestTune$gamma,
                                        eta = xgb_tune$bestTune$eta,
                                        min_child_weight = xgb_tune$bestTune$min_child_weight ,
                                        subsample = 1,
                                        colsample_bytree = 1),
                    nrounds = xgb_tune$bestTune$nrounds,
                    m=1)
  
  imp_1 <- as.numeric(imp_data[[1]]$Q119INCOME[I$indexes])
  conf_mat <- caret::confusionMatrix(table(factor(I$original, sort(union(I$original,imp_1))),
                                           factor(imp_1, sort(union(I$original,imp_1)))))
  k_linear <- CohenKappa(conf_mat$table,weights = "Equal-Spacing") 
  k_squared <- CohenKappa(conf_mat$table,weights = "Fleiss-Cohen")
  
  performance <- conf_mat$byClass
  macro_f1 <- mean(performance[,"F1"])
  
  resul <- list("imp_dataset" = imp_data[[1]],  "imp_values"=imp_1,
                "wk_linear"=k_linear, "wk_squared"=k_squared,
                "macro_F1_score" = macro_f1  ,"performance" = performance,
                "original_values"=I$original,"conf_matrix"=conf_mat$table,
                "best_params"=xgb_tune$bestTune)
}

# kNN -----------------------
# DEFAULT HYPERPARAMETER VERSION

knn_std_impute <- function(I){
  library(caret)
  library(VIM)
  i_prep <- m_prep(I)
  d_prep <- i_prep$prep_tot
  
  d_imp <- kNN(d_prep)
  
  imp <- round(denormalize(d_imp$Q119INCOME[I$indexes]))
  
  all_class <- sort(union(I$original,imp))
  newtable <- table(factor(I$original, all_class), factor(imp, all_class))
  conf_mat <- caret::confusionMatrix(newtable)
  
  k_linear <- CohenKappa(conf_mat$table,weights = "Equal-Spacing")
  k_squared <- CohenKappa(conf_mat$table,weights = "Fleiss-Cohen")
  performance <- conf_mat$byClass
  macro_f1 <- mean(performance[,"F1"])
  
  resul <- list("imp_dataset" = d_imp, "imp_values_1-16"=imp, "wk_linear"=k_linear, "wk_squared"=k_squared, "macro_F1_score" = macro_f1  ,"performance" = performance, "original_values"=I$original,"conf_matrix"=conf_mat$table)
  return(resul)
}

## CROSS VALIDATED VERSION -----------------------

knn_cv_impute <- function(I,k_grid=1:30){
  library(caret)
  library(VIM)
  i_prep <- m_prep(I)
  d_prep <- i_prep$prep_tot
  #complete cases
  cc <- d_prep[-I$indexes,]
  #cv:
  trControl <- trainControl(method  = "cv",
                            number  = 5)
  
  fit <- train(Q119INCOME~ .,
               method     = "knn",
               tuneGrid   = expand.grid(k = k_grid),
               trControl  = trControl,
               metric     = "RMSE",
               data       = cc)
  
  best_k_nn <- as.integer(fit$bestTune)
  #kNN with best k:
  d_imp <- kNN(d_prep, k = best_k_nn)
  imp <- round(denormalize(d_imp$Q119INCOME[I$indexes]))
  d <- data.frame(I$original,imp)
  
  k_squared <-(irr::kappa2(d,weight = 'squared'))$value
  
  all_class <- sort(union(I$original,imp))
  newtable <- table(factor(I$original, all_class), factor(imp, all_class))
  conf_mat <- caret::confusionMatrix(newtable)
  
  k_linear <- CohenKappa(conf_mat$table,weights = "Equal-Spacing")
  performance <- conf_mat$byClass
  macro_f1 <- mean(performance[,"F1"])
  resul <- list("imp_dataset" = d_imp, "imp_values_1-16"=imp,"best_k" = best_k_nn, "wk_linear"=k_linear, "wk_squared"=k_squared, "macro_F1_score" = macro_f1  ,"performance" = performance, "original_values"=I$original,"conf_matrix"=conf_mat$table)
  return(resul)
}
# SVM
# CROSS VALIDATED RADIAL KERNEL -----------------------
svm_rad_impute <- function(I,k=5, cost_values= c(0.1,0.5,0.8,1,3,5,10), gamma_values= c(0.001, 0.01, 0.1, 1, 10)){
  preproc <-  m_prep(I,inc = F)
  data_cc <- preproc$prep_tot[-I$indexes,]
  
  trControl <- trainControl(method = "cv", number = k, search = "grid")
  tuneGridRadial <- expand.grid(C = cost_values, sigma = gamma_values)
  
  #cv
  svmRadial <- train(Q119INCOME ~ .,
                     data = data_cc,
                     method = "svmRadial",
                     trControl = trControl,
                     tuneGrid = tuneGridRadial)
  #apply SVM with best values for the hyper param:
  best <- svmRadial$bestTune
  model_final <- svm(Q119INCOME ~ ., data = data_cc, kernel = "radial", cost = best$C, gamma = best$sigma)
  
  #test on the NA's
  test_final <- preproc$prep_tot[I$indexes,] %>% select(-Q119INCOME)
  predictions_final_ord <- predict(model_final, test_final)
  predictions_final <- as.numeric(predictions_final_ord)
  
  d <- data.frame(I$original,predictions_final)
  k_squared <-(irr::kappa2(d,weight = 'squared'))$value
  
  all_class <- sort(as.numeric(union(I$original,predictions_final)))
  newtable <- table(factor(I$original, all_class), factor(predictions_final, all_class))
  conf_mat <- caret::confusionMatrix(newtable)
  
  k_linear <- CohenKappa(conf_mat$table,weights = "Equal-Spacing")
  performance <- conf_mat$byClass
  macro_f1 <- mean(performance[,"F1"])
  full_income_imp <- preproc$prep_tot
  full_income_imp[I$indexes,"Q119INCOME"] <- predictions_final_ord
  
  res <-  list( "imp_values"=predictions_final,"full_inc_distrib" = full_income_imp ,"best_params" = best, "wk_linear"=k_linear, "wk_squared"=k_squared, "macro_F1_score" = macro_f1  ,"performance" = performance, "original_values"=I$original,"conf_matrix"=conf_mat$table)
  return(res)
  
}

# CROSS VALIDATED SIGMOID KERNEL -----------------------

svm_sig_impute <- function(I, k=5, cost_values= c(0.1,0.5,0.8,1,3,5,10), gamma_values= c(0.001, 0.01, 0.1, 1, 10), coef0_values=0:2) {
  preproc <-  m_prep(I, inc = FALSE)
  data_cc <- preproc$prep_tot[-I$indexes,]
  
  folds <- createFolds(data_cc$Q119INCOME, k = k, list = TRUE, returnTrain = TRUE)
  results <- data.frame(C = double(), Gamma = double(), Coef0 = double(), RMSE = double())
  
  for (cost in cost_values) {
    for (gamma in gamma_values) {
      for (coef0 in coef0_values) {
        rmse_values <- c()
        
        for (i in 1:k) {
          train_index <- folds[[i]]
          test_index <- setdiff(seq_len(nrow(data_cc)), train_index)
          
          train_data <- data_cc[train_index, ]
          test_data <- data_cc[test_index, ]
          
          model <- svm(Q119INCOME ~ ., data = train_data, kernel = "sigmoid", cost = cost, gamma = gamma, coef0 = coef0)
          predictions <- predict(model, test_data)
          
          rmse <- sqrt(mean((as.numeric(predictions) - as.numeric(test_data$Q119INCOME))^2))
          rmse_values <- c(rmse_values, rmse)
        }
        
        avg_rmse <- mean(rmse_values)
        results <- rbind(results, data.frame(C = cost, Gamma = gamma, Coef0 = coef0, RMSE = avg_rmse))
      }
    }
  }
  
  best_params <- results[which.min(results$RMSE), ]
  
  # Train the final model with the best parameters
  model_final <- svm(Q119INCOME ~ ., data = data_cc, kernel = "sigmoid", cost = best_params$C, gamma = best_params$Gamma, coef0 = best_params$Coef0)
  
  test_final <- preproc$prep_tot[I$indexes, ] %>% select(-Q119INCOME)
  predictions_final_ord <- predict(model_final, test_final)
  predictions_final <- as.numeric(predictions_final_ord)
  
  all_class <- sort(as.numeric(union(I$original, predictions_final)))
  newtable <- table(factor(I$original, all_class), factor(predictions_final, all_class))
  conf_mat <- caret::confusionMatrix(newtable)
  
  k_linear <- CohenKappa(conf_mat$table,weights = "Equal-Spacing") 
  k_squared <- CohenKappa(conf_mat$table,weights = "Fleiss-Cohen")
  performance <- conf_mat$byClass
  macro_f1 <- mean(performance[,"F1"])
  full_income_imp <- preproc$prep_tot
  full_income_imp[I$indexes, "Q119INCOME"] <- predictions_final_ord
  
  res <- list("imp_values" = predictions_final, "full_inc_distrib" = full_income_imp, "best_params" = best_params,
              "wk_linear" = k_linear, "wk_squared" = k_squared, "macro_F1_score" = macro_f1,
              "performance" = performance, "original_values" = I$original, "conf_matrix" = conf_mat$table)
  
  return(res)
}

# MICE -----------------------
mice_impute <- function(I,m=5){ #I is the file i_MAR...  created in dataset_generation.R
  D <- I$dataset
  D <- factorizator(D)
  mice <- mice(D,m=m,printFlag = F)
  k_lin_values <- c()
  k_sq_values <- c()
  for (i in 1:m) {
    imp <- as.numeric(mice$imp$Q119INCOME[[i]])
    all_class <- sort( union(I$original,imp))
    newtable <- table(factor(I$original, all_class), factor(imp, all_class))
    conf_mat <- caret::confusionMatrix(newtable)
    
    k_lin_values <- c(k_lin_values,(round(CohenKappa(conf_mat$table,weights = "Equal-Spacing"),3)))
    k_sq_values <- c(k_sq_values,(round(CohenKappa(conf_mat$table,weights = "Fleiss-Cohen"),3)))
  }
  k_squared <- max(k_sq_values)
  k_linear <- max(k_lin_values)
  
  chosen_one <- which.max(k_lin_values)
  
  imp_f <-as.numeric(mice$imp$Q119INCOME[[chosen_one]])
  
  all_class_f <- sort(union(I$original,imp_f))
  newtable_f <- table(factor(I$original, all_class_f), factor(imp_f, all_class_f))
  conf_mat_f <- caret::confusionMatrix(newtable_f)
  performance <- conf_mat_f$byClass
  
  macro_f1 <- mean(performance[,"F1"])
  
  tot_dataset <- D
  tot_dataset$Q119INCOME[I$indexes] <- imp_f
  resul <- list("imp_dataset" = tot_dataset, "imp_values"=imp_f, "wk_linear"=k_linear, "wk_squared"=k_squared, "macro_F1_score" = macro_f1  ,"performance" = performance, "original_values"=I$original,"conf_matrix"=conf_mat_f$table)
  return(resul)
}
