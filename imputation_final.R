# Final imputation 
load("P:/income imputation work/r CODE/income imputation/everything.RData")

#selected method: Random Forest
variables <- c("AGE", "q101SEX", "BIRTHPLACE", "DISTRICT", "CITIZEN", "Q119INCOME", "MAINSTAT", "JOBSTAT", "JOBISCO2", "FT_PT", "NACE_class", "HATLEVEL3", "LGQUEST", "INTMETHOD", "HHTYPE", "HHNBPERS", "HHNBPERS_0_13", "migration")



#  removing the FLAGGED unit in 1st class ---------------------------
# from the low income individuals analysis, we saw that some people who declared to have hh income lower than 1000, might be lying (in the sense that misunderstood the question and chose the wrong class). For that reason we will consider them as MAR NAs
library(naniar)
mcar_test(final_NAs) 
mcar_test(final_IDNWTA)
#both non MCAR

final_data_flag <- final_data_tot

flagged_id <- which(final_data_flag$Q119INCOME==1 & final_data_flag$HHNBPERS > 2 & final_data_flag$HHTYPE!= 40 )
final_data_flag$Q119INCOME[flagged_id] #ok

final_data_flag$Q119INCOME[flagged_id] = NA #ok


# IDNWTA and NA


# IDNWTA
#rf_cv_best

final_IDNWTA_flag <- final_data_flag[!is.na(final_data_flag$Q119INCOME),]
final_IDNWTA_flag$Q119INCOME[final_IDNWTA_flag$Q119INCOME==17] <- NA
final_IDNWTA_flag$Q119INCOME <- factor(final_IDNWTA_flag$Q119INCOME,levels=as.character(1:16))


final_IDNWTA_flag_imp <- missForest(final_IDNWTA_flag, ntree=600, mtry=14)

barplot(prop.table(table(final_IDNWTA_flag$Q119INCOME)),ylim=c(0,0.15))
barplot(prop.table(table(final_IDNWTA_flag_imp$ximp$Q119INCOME)),ylim=c(0,0.15))
final_IDNWTA_flag_imp$OOBerror


# NA

library(dplyr)

final_NAs_flag <- final_data_flag %>% filter(is.na(Q119INCOME) | Q119INCOME != 17)
table(final_NAs_flag$Q119INCOME,useNA='ifany')
final_NAs_flag$Q119INCOME <- factor(final_NAs_flag$Q119INCOME,levels=as.character(1:16))

final_NAs_flag_imp <- missForest(final_NAs_flag, ntree=600, mtry=12)

# check the results:
barplot(prop.table(table(final_IDNWTA_flag_imp$ximp$Q119INCOME)))
barplot(prop.table(table(final_NAs_flag_imp$ximp$Q119INCOME)))

table(final_NAs_flag_imp$ximp$Q119INCOME[is.na(final_NAs_flag$Q119INCOME)])
barplot(table(final_NAs_flag_imp$ximp$Q119INCOME[is.na(final_NAs_flag$Q119INCOME)]))

table(final_IDNWTA_flag_imp$ximp$Q119INCOME[is.na(final_IDNWTA_flag$Q119INCOME)])
barplot(table(final_IDNWTA_flag_imp$ximp$Q119INCOME[is.na(final_IDNWTA_flag$Q119INCOME)]))


dim(final_NAs_flag)
prop.table(table(final_NAs_flag$Q119INCOME,useNA='ifany'))
dim(final_IDNWTA_flag)
prop.table(table(final_IDNWTA_flag$Q119INCOME,useNA='ifany'))



# merging the flagged results ----------------------------------------------------
# NON income variables, taken from IDNWTA imputation.
# Income merged from the two imputations.


final_NAs_flag_imputed <- final_NAs_flag_imp$ximp[is.na(final_NAs_flag$Q119INCOME),]
dim(final_NAs_flag_imputed)

final_data_tot_imp_flag <- rbind(final_NAs_flag_imputed,final_IDNWTA_flag_imp$ximp)

dim(final_data_tot_imp_flag)


barplot(prop.table(table(final_data_tot_imp_flag$Q119INCOME)))

valori <- c("Less than 1 000","1 000 to 1 499","1 500 to 1 999","2 000 to 2 499","2 500 to 2 999","3 000 to 3 499","3 500 to 3 999","4 000 to 4 499","4 500 to 4 999","5 000 to 5 999","6 000 to 6 999","7 000 to 7 999","8 000 to 8 999","9 000 to 9 999","10 000 to 12 500","More than 12 500", "I do not wish to answer","NA")
valori_t <- c("Less than 1 000","1 000 to 1 499","1 500 to 1 999","2 000 to 2 499","2 500 to 2 999","3 000 to 3 499","3 500 to 3 999","4 000 to 4 499","4 500 to 4 999","5 000 to 5 999","6 000 to 6 999","7 000 to 7 999","8 000 to 8 999","9 000 to 9 999","10 000 to 12 500","More than 12 500")

ggplot(datini, aes(x = valore, y = proporzione.Freq, fill = valore)) +
  geom_bar(stat = "identity", color = "white",fill="#6e97ad") +
  scale_y_continuous(limits = c(0, 15), expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 50, hjust=1, size = 10),
        legend.position = "none") +
  labs(x = "Euros", y = "%", title = "original Income Values") 

datini_t_flag <- data.frame(
  valore = factor(valori_t,levels = valori_t),
  proporzione = prop.table(table(final_data_tot_imp_flag$Q119INCOME))*100
)

ggplot(datini_t_flag, aes(x = valore, y = proporzione.Freq, fill = valore)) +
  geom_bar(stat = "identity", color = "white",fill="#6e97ad") +
  scale_y_continuous(limits = c(0, 15), expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 50, hjust=1, size = 10),
        legend.position = "none") +
  labs(x = "Euros", y = "%", title = "Income Values") 

datini$Dataset <- "Original"
datini_t_flag$Dataset <- "Imputed"
datini_no <- datini[-(17:18),]

ggplot(datini_t_flag, aes(x = valore, y = proporzione.Freq)) +
  geom_bar(stat = "identity", position = "dodge",fill="#6B95AA") +
  scale_y_continuous(limits = c(0, 15), expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 50, hjust=1, size = 10),
        legend.position = "top") +
  labs(x = "Euros", y = "%",title = 'Final HH income imputation') 


complete_cases <- data.frame(
  valore = factor(valori_t,levels = valori_t),
  proporzione = prop.table(table(final_data$Q119INCOME))*100
)
complete_cases$Dataset <- "Complete cases"


combined_data_2 <- bind_rows(complete_cases, datini_t_flag)

ggplot(combined_data_2, aes(x = valore, y = proporzione.Freq, fill = Dataset)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  scale_y_continuous(limits = c(0, 15), expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 50, hjust=1, size = 10),
        legend.position = "top") +
  labs(x = "Euros", y = "%") +
  scale_fill_manual(values = c("Complete cases" = "#6B95AA", "Imputed" = "#F04E37"))  


combined_data_3 <- bind_rows(complete_cases, datini_t)

ggplot(combined_data_3, aes(x = valore, y = proporzione.Freq, fill = Dataset)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  scale_y_continuous(limits = c(0, 15), expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 50, hjust=1, size = 10),
        legend.position = "top") +
  labs(x = "Euros", y = "%") +
  scale_fill_manual(values = c("Complete cases" = "#6B95AA", "Imputed" = "#F04E37"))  


final_NAs_flag_imp$OOBerror
final_IDNWTA_flag_imp$OOBerror
barplot(prop.table(table(final_data$Q119INCOME) )*100)

chisq.test(cbind(table(final_data$Q119INCOME),(table(final_data_tot_imp_flag$Q119INCOME))))

