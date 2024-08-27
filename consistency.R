#  Consistency checking for the results from FLAGGED data removed------------------------------------------------
colSums(is.na(final_data))
# we need to check:
# - that the MAINSTAT imputed values are consistent with the JOBSTAT, JOBISCO2, FT_PT and NACE_class; DONE
# - HHNBPERS and HHNBPERS_0_13, who refers to number of people, are integers; They are not, but easy fix with round()
# - HHTYPE is consistent with HHNBPERS and HHNBPERS_0_13... ?

#quick recap: MAINSTAT: Main activity status
# 10 = Employed
# 20 = Unemployed
# 30 = Retired
# 40 = Unable to work due to longstanding health problems
# 50 = Student, pupil
# 60 = Fulfilling domestic tasks
# 70 = Compulsory military or civilian service
# 80 = Other
# -1 = Not stated

#all the MAINSTAT different from 10 (employed) must have a value equal to -2 (not applicable) to the variables JOBSTAT, JOBISCO2, FT_PT and NACE_class.

#JOBSTAT: Status in employment in main job
# 11 = Self-employed person with employees
# 12 = Self-employed person without employees
# 20 = Employee
# 30 = Family worker (unpaid)
# -1 = Not stated
# -2 = Not applicable

#JOBISCO: Profession (single-digit ISCO classification)
# 1 Managers
# 2 Professionals
# 3 Technicians and associate professionals
# 4 Administrative support workers
# 5 Service and sales workers
# 6 Skilled agricultural, forestry and fishery workers
# 7 Craft and related trade workers, Plant and machine operators and assemblers and Elementary occupations
# 0 Armed forces occupations
# -2 Not applicable

table(final_NAs_flag$MAINSTAT,useNA='ifany') #67 NAs_flag
table(final_NAs_flag_imp$ximp$MAINSTAT,useNA='ifany') #they have been inserted all in the statuses different from employed. So, for these units, JOBSTAT, JOBISCO2, FT_PT and NACE_class has to be equal to -2.
ind_NA_flag_mainstat <- is.na(final_NAs_flag$MAINSTAT)
final_NAs_flag_imp$ximp[ind_NA_flag_mainstat, c("MAINSTAT", "JOBSTAT", "JOBISCO2", "FT_PT", "NACE_class")] #perfect

ind_NA_flag_jobstat <- is.na(final_NAs_flag$JOBSTAT)
final_NAs_flag_imp$ximp[ind_NA_flag_jobstat, c("MAINSTAT", "JOBSTAT", "JOBISCO2", "FT_PT", "NACE_class")] #perfect

ind_NA_flag_ftpt <- is.na(final_NAs_flag$FT_PT)
final_NAs_flag_imp$ximp[ind_NA_flag_ftpt, c("MAINSTAT", "JOBSTAT", "JOBISCO2", "FT_PT", "NACE_class")] #perfect

ind_NA_flag_nace <- is.na(final_NAs_flag$NACE_class )
final_NAs_flag_imp$ximp[ind_NA_flag_nace, c("MAINSTAT", "JOBSTAT", "JOBISCO2", "FT_PT", "NACE_class")] #perfect

#however we still might check the consistencies between JOBISCO and NACE. Since they are small numbers, probably we can avoid it.


#Now, let's do the same for the IDNWTA_flag:
table(final_IDNWTA_flag$MAINSTAT,useNA='ifany') #59 NAs_flag
table(final_IDNWTA_flag_imp$ximp$MAINSTAT,useNA='ifany') #they have been inserted all in the statuses different from employed. So, for these units, JOBSTAT, JOBISCO2, FT_PT and NACE_class has to be equal to -2.
ind_IDNWTA_flag_mainstat <- is.na(final_IDNWTA_flag$MAINSTAT)
final_IDNWTA_flag_imp$ximp[ind_IDNWTA_flag_mainstat, c("MAINSTAT", "JOBSTAT", "JOBISCO2", "FT_PT", "NACE_class")] #perfect

ind_IDNWTA_flag_jobstat <- is.na(final_IDNWTA_flag$JOBSTAT)
# final_IDNWTA_flag[ind_IDNWTA_flag_jobstat, c("MAINSTAT", "JOBSTAT", "JOBISCO2", "FT_PT", "NACE_class")]
final_IDNWTA_flag_imp$ximp[ind_IDNWTA_flag_jobstat, c("MAINSTAT", "JOBSTAT", "JOBISCO2", "FT_PT", "NACE_class")] #perfect

ind_IDNWTA_flag_ftpt <- is.na(final_IDNWTA_flag$FT_PT)
final_IDNWTA_flag_imp$ximp[ind_IDNWTA_flag_ftpt, c("MAINSTAT", "JOBSTAT", "JOBISCO2", "FT_PT", "NACE_class")] #perfect

ind_IDNWTA_flag_nace <- is.na(final_IDNWTA_flag$NACE_class )
final_IDNWTA_flag_imp$ximp[ind_IDNWTA_flag_nace, c("MAINSTAT", "JOBSTAT", "JOBISCO2", "FT_PT", "NACE_class")]
#perfect


#now we need to check the number of people in the households:
#HHTYPE: type of household
# 10 = One-person household
# 21 = Lone parent with at least one child aged less than 25
# 22 = Lone parent with all children aged 25 or more
# 31 = Couple without any children
# 32 = Couple with at least one child aged less than 25
# 33 = Couple with all children aged 25 or more (if hhnbpers >= 3 but hhnbpers_0_13 != 0: error)
# 40 = Other type of household
# -1 = Not stated

ind_NAs_flag_pers <- is.na(final_NAs_flag$HHNBPERS )
final_NAs_flag_imp$ximp$HHNBPERS <- round(final_NAs_flag_imp$ximp$HHNBPERS)
final_NAs_flag_imp$ximp$HHNBPERS_0_13 <- round(final_NAs_flag_imp$ximp$HHNBPERS_0_13)
final_NAs_flag_imp$ximp[ind_NAs_flag_pers, c("HHNBPERS","HHNBPERS_0_13","HHTYPE","Q119INCOME","AGE")]
#  Inconsistent units:
# 1482, 3010 (same UNIT AS 1647 IN IDNWTA_flag GROUP)

ind_NAs_flag_pers13 <- is.na(final_NAs_flag$HHNBPERS_0_13 )
final_NAs_flag_imp$ximp[ind_NAs_flag_pers13, c("HHNBPERS","HHNBPERS_0_13","HHTYPE","Q119INCOME")]
#  Inconsistent units:
# 5 is bad because it cannot be coliving, since the number estimated is the number of children, that should have been 0 and not 1.
# 2585, 3010

ind_IDNWTA_flag_pers <- is.na(final_IDNWTA_flag$HHNBPERS )
final_IDNWTA_flag_imp$ximp$HHNBPERS <- round(final_IDNWTA_flag_imp$ximp$HHNBPERS)
final_IDNWTA_flag_imp$ximp$HHNBPERS_0_13 <- round(final_IDNWTA_flag_imp$ximp$HHNBPERS_0_13)
final_IDNWTA_flag_imp$ximp[ind_IDNWTA_flag_pers, c("HHNBPERS","HHNBPERS_0_13","HHTYPE","Q119INCOME")]
#  Inconsistent units:
#  1647 (same UNIT AS 1482 IN NAs_flag GROUP, however here is better: 9 ppl estimated against 8. Both wrong but closer to a possible real one)
# 2781

ind_IDNWTA_flag_pers13 <- is.na(final_IDNWTA_flag$HHNBPERS_0_13 )
final_IDNWTA_flag_imp$ximp[ind_IDNWTA_flag_pers13, c("HHNBPERS","HHNBPERS_0_13","HHTYPE","Q119INCOME")]
#  Inconsistent units:
# 1472 
# overall, less number of inconsistencies in the IDNWTA dataset. Those values will be used for the complete dataset. (except for income)