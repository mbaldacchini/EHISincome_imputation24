# missing data generation algorithms

# MCAR --------------------------------------------------------------------
gen_MCAR <- function(D, #dataset with complete cases
                     inc= "Q119INCOME",
                     mdr = 5 #missing data ratio, default = 5, but can be chosen as much as possible (in [1,100])
){ 
  stopifnot(mdr %in% c(1:100))
  mdr=mdr/100
  inc <- as.character(inc)
  D <- data.frame(D)
  num_units <- nrow(D) #total number of units
  na_number <- round(num_units*mdr) #number of NA to generate, based on mdr
  
  indexes <- sample(1:num_units, na_number, replace=F)
  original_val <- D[indexes,inc]
  D[indexes,inc] <- NA
  D <- factorizator(D)
  result <- list("dataset"=D, "indexes"=indexes,"original"=original_val)
  return(result)
}


# MAR and MNAR -------------------------------------------------------------

gen_MAR_MNAR <- function(D,
                         and=T, #if False, the conditions will be merged using the OR operator
                         condition_vars, #a vector containing one or more variables used for the condition
                         condition_exprs, #a vector containing one or more the conditions, must have the same length of condition_vars
                         inc='Q119INCOME', #the income variable name
                         mdr=5 #the missing data ratio ( must be in [0,100])
) { 
  if (length(condition_vars) != length(condition_exprs)) {
    stop("Vectors condition_vars and condition_exprs must be of the same lenghts.")
  }
  if (!all(c(inc, condition_vars) %in% colnames(D))) {
    stop("inc and condition_var variables must be columns of the Dframe.")
  }  
  stopifnot(mdr %in% c(1:100))
  stopifnot(length(condition_exprs)!=0)#raise error if you don't insert a cause for missing generation
  mdr=mdr/100
  inc <- as.character(inc)
  D <- data.frame(D)
  num_units <- nrow(D)
  
  if (and) {
    combined_condition <- rep(TRUE, num_units)
    for (i in seq_along(condition_vars)) {
      condition_var <- condition_vars[i]
      condition_expr <- condition_exprs[i]
      combined_condition <- combined_condition & eval(parse(text = paste0("D[[\"", condition_var, "\"]]", condition_expr)))
    }
  } else {
    combined_condition <- rep(FALSE, num_units)
    for (i in seq_along(condition_vars)) {
      condition_var <- condition_vars[i]
      condition_expr <- condition_exprs[i]
      combined_condition <- combined_condition | eval(parse(text = paste0("D[[\"", condition_var, "\"]]", condition_expr)))
    }
  }
  condition_indices <- which(combined_condition) #defines the indexes
  num_condition_indices <- length(condition_indices)
  
  min_required_units <- ceiling(num_units * mdr)
  
  if (num_condition_indices < min_required_units) { #check if we have enough units
    stop("Not enoguh unit to reach the desired missing data ratio. Lower it down or chose more conditions")
  }
  
  selected_indices <- sample(condition_indices, min_required_units, replace=F) #randomly selects the indexes
  original_val <- D[selected_indices, inc]  
  D[selected_indices, inc] <- NA
  result <- list("dataset"=D, "indexes"=selected_indices,"original"=original_val)
  return(result)
}








# INCOME IMPUTATION
# datasets creation
setwd("P:/income imputation work/r CODE/income imputation")
# 1. import data ----------------------------------------------------------

load("P:/income imputation work/r CODE/income_analysis.RData")
dati_imp <- inc_an
datini <- dati_cr
datini$Q119INCOME[datini$Q119INCOME==17] <- NA

# 2. variable selection and complete cases --------------------------------
dati_imp$Q119INCOME[dati_imp$Q119INCOME==17] <- NA
variables <- c("AGE", "q101SEX", "BIRTHPLACE", "DISTRICT", "CITIZEN", "Q119INCOME", 
               "MAINSTAT", "JOBSTAT", "JOBISCO2", "FT_PT", "NACE_class", "HATLEVEL3", 
               "LGQUEST", "INTMETHOD", "HHTYPE", "HHNBPERS", "HHNBPERS_0_13", "migration")
variables_noinc <- c("AGE", "q101SEX", "BIRTHPLACE", "DISTRICT", "CITIZEN", 
                     "MAINSTAT", "JOBSTAT", "JOBISCO2", "FT_PT", "NACE_class", "HATLEVEL3", 
                     "LGQUEST", "INTMETHOD", "HHTYPE", "HHNBPERS", "HHNBPERS_0_13", "migration")
index_all_sdc <- (complete.cases(inc_an[,variables_noinc]))
dati_imp <- dati_imp[,variables]

dati_imp <- dati_imp[complete.cases(dati_imp),]
dim(dati_imp) #2954 units


rm(list=setdiff(ls(), c("dati_imp","variables","dati_cr","dati","datini")))



sum(is.na(dati_imp$Q119INCOME))

# 3. missing values data sets creation ------------------------------------

# I will create missing values following the MCAR, MAR and MNAR missing pattern in different proportions: 5, 10, 15, 20, 25 and 30 %. 

## 3.1 MCAR datasets generation ------------------------------------------
set.seed(23)
i_MCAR_5 <- gen_MCAR(dati_imp,mdr=5)
d_MCAR_5 <- i_MCAR_5$dataset

i_MCAR_10 <- gen_MCAR(dati_imp,mdr=10)
d_MCAR_10 <- i_MCAR_10$dataset

i_MCAR_15 <- gen_MCAR(dati_imp,mdr=15)
d_MCAR_15 <- i_MCAR_15$dataset

i_MCAR_20 <- gen_MCAR(dati_imp,mdr=20)
d_MCAR_20 <- i_MCAR_20$dataset

i_MCAR_25 <- gen_MCAR(dati_imp,mdr=25)
d_MCAR_25 <- i_MCAR_25$dataset

i_MCAR_30 <- gen_MCAR(dati_imp,mdr=30)
d_MCAR_30 <- i_MCAR_30$dataset

## 3.2 MAR datasets generation ------------------------------------------
#since we noted that the major NAs appears for young people and female, we will create MAR income NAs from the younger woman

i_MAR_5 <- gen_MAR_MNAR(dati_imp,condition_vars = c("q101SEX","AGE"),condition_exprs = c("==2","<=30"),and=F,mdr=5)
d_MAR_5 <- i_MAR_5$dataset

i_MAR_10 <- gen_MAR_MNAR(dati_imp,condition_vars = c("q101SEX","AGE"),condition_exprs = c("==2","<=30"),and=F,mdr=10)
d_MAR_10 <- i_MAR_10$dataset

i_MAR_15 <- gen_MAR_MNAR(dati_imp,condition_vars = c("q101SEX","AGE"),condition_exprs = c("==2","<=30"),and=F,mdr=15) #and = F, not enough young women in the dataset to reach the 15% NA
d_MAR_15 <- i_MAR_15$dataset

i_MAR_20 <- gen_MAR_MNAR(dati_imp,condition_vars = c("q101SEX","AGE"),condition_exprs = c("==2","<=30"),and=F,mdr=20)
d_MAR_20 <- i_MAR_20$dataset

i_MAR_25 <- gen_MAR_MNAR(dati_imp,condition_vars = c("q101SEX","AGE"),condition_exprs = c("==2","<=30"),and=F,mdr=25)
d_MAR_25 <- i_MAR_25$dataset

i_MAR_30 <- gen_MAR_MNAR(dati_imp,condition_vars = c("q101SEX","AGE"),condition_exprs = c("==2","<=30"),and=F,mdr=30)
d_MAR_30 <- i_MAR_30$dataset

# tests:
prop.table(table(d_MAR_10$Q119INCOME,useNA = 'ifany'))
prop.table(table(d_MAR_25$Q119INCOME,useNA = 'ifany'))
prop.table(table(d_MCAR_25$Q119INCOME,useNA = 'ifany'))
prop.table(table(d_MAR_30$Q119INCOME[d_MAR_30$q101SEX==2],useNA = 'ifany'))
prop.table(table(d_MAR_30$Q119INCOME[d_MAR_30$q101SEX==2 & d_MAR_30$AGE<=30 ],useNA = 'ifany'))
prop.table(table(d_MAR_30$Q119INCOME[d_MAR_30$AGE<=30 ],useNA = 'ifany'))


prop.table(table(d_MAR_25$q101SEX[!is.na(d_MAR_25$Q119INCOME) ]))

prop.table(table(d_MAR_30$q101SEX[is.na(d_MAR_30$Q119INCOME) ]))
prop.table(table(d_MAR_10$q101SEX[is.na(d_MAR_10$Q119INCOME) ]))

prop.table(table(dati_imp$q101SEX))
prop.table(table(dati_cr$q101SEX))
prop.table(table(datini$q101SEX[!is.na(datini$Q119INCOME)]))


## 3.3 MNAR datasets generation ------------------------------------------

# for the MNAR generations, we are going to remove data from the lower and upper quartile:
summary(dati_imp$Q119INCOME)

# i.e. lower than income class 6 or greater than 12


i_MNAR_5 <- gen_MAR_MNAR(dati_imp,condition_vars = c("Q119INCOME","Q119INCOME"),condition_exprs = c("<=6",">=12"),and=F,mdr=5)
d_MNAR_5 <- i_MNAR_5$dataset

i_MNAR_10 <- gen_MAR_MNAR(dati_imp,condition_vars = c("Q119INCOME","Q119INCOME"),condition_exprs = c("<=6",">=12"),and=F,mdr=10)
d_MNAR_10 <- i_MNAR_10$dataset

i_MNAR_15 <- gen_MAR_MNAR(dati_imp,condition_vars = c("Q119INCOME","Q119INCOME"),condition_exprs = c("<=6",">=12"),and=F,mdr=15) 
d_MNAR_15 <- i_MNAR_15$dataset

i_MNAR_20 <- gen_MAR_MNAR(dati_imp,condition_vars = c("Q119INCOME","Q119INCOME"),condition_exprs = c("<=6",">=12"),and=F,mdr=20)
d_MNAR_20 <- i_MNAR_20$dataset

i_MNAR_25 <- gen_MAR_MNAR(dati_imp,condition_vars = c("Q119INCOME","Q119INCOME"),condition_exprs = c("<=6",">=12"),and=F,mdr=25)
d_MNAR_25 <- i_MNAR_25$dataset

i_MNAR_30 <- gen_MAR_MNAR(dati_imp,condition_vars = c("Q119INCOME","Q119INCOME"),condition_exprs = c("<=6",">=12"),and=F,mdr=30)
d_MNAR_30 <- i_MNAR_30$dataset

prop.table(table(d_MNAR_30$Q119INCOME,useNA='ifany'))
prop.table(table(d_MNAR_5$Q119INCOME,useNA='ifany'))


# MISSING DATA TESTS ------------------------------------------------------


# install.packages("naniar")
library(naniar)
mcar_test(d_MCAR_5)
mcar_test(d_MCAR_10)
mcar_test(d_MCAR_15)
mcar_test(d_MCAR_20)
mcar_test(d_MCAR_25)
mcar_test(d_MCAR_30)

mcar_test(d_MAR_5)
mcar_test(d_MAR_10)
mcar_test(d_MAR_15)
mcar_test(d_MAR_20)
mcar_test(d_MAR_25)
mcar_test(d_MAR_30)

mcar_test(d_MNAR_5)
mcar_test(d_MNAR_10)
mcar_test(d_MNAR_15)
mcar_test(d_MNAR_20)
mcar_test(d_MNAR_25)
mcar_test(d_MNAR_30)


