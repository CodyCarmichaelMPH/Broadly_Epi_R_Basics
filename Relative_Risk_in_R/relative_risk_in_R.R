#Install packages if needed. 
#install.packages("dplyr")
#install.packages("epiR")


## Call in libraries
library(dplyr)
### Only needed for the 2nd method
library(epiR)


smoking_survey <- read.csv("smoking_survey.csv")



# Clean and format Data. 


## Since we're only worried about smoking status and a diagnosis of lung cancer for now, let's eliminate other fields

smoking_survey <- dplyr::select(smoking_survey, smoking_status, diagnosis_codes)



## Now we need to identify who has our diagnoses of interest; Mark lung_cancer "yes" if they have C34.90, C96.29, otherwise "no". 

has_lung_cancer <- function(codes) {
  if (grepl("C34.90", codes) || grepl("C96.29", codes)) {
    return("yes")
  } else {
    return("no")
  }
}

# Apply this function to each row in the diagnosis_codes column
smoking_survey$lung_cancer <- apply(smoking_survey, 1, function(x) has_lung_cancer(x['diagnosis_codes']))


## Now we can drop the diagnosis_codes as we have what we need to proceed. 

smoking_survey <- dplyr::select(smoking_survey, smoking_status, lung_cancer)



#Calculate Relative Risk


## first convert to factors and organize, if not organized, R will make a "best guess".  
smoking_survey$lung_cancer <- factor(smoking_survey$lung_cancer, levels = c("yes", "no"))
smoking_survey$smoking_status <- factor(smoking_survey$smoking_status, levels = c("smoker", "non-smoker"))

## Next we can create a table to look at our columns.

smoking_table <- table(smoking_survey$smoking_status, smoking_survey$lung_cancer)




## Now we can calculate relative risk: 

risk_exposed <- smoking_table[1,1] / sum(smoking_table[1,])
risk_unexposed <- smoking_table[2,1] / sum(smoking_table[2,])
relative_risk <- risk_exposed / risk_unexposed




# We've done it by hand, now we can try it using the EpiR package! 

result <- epiR::epi.2by2(dat = smoking_table, 
                         method = "cohort.count", 
                         outcome = "as.columns")



# The result object contains RR and other useful statistics
print(result)

