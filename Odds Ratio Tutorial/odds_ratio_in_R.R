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


#format variables to factors for eventual evaluation and organization.
smoking_survey$lung_cancer <- factor(smoking_survey$lung_cancer, levels = c("yes", "no"))
smoking_survey$smoking_status <- factor(smoking_survey$smoking_status, levels = c("smoker", "non-smoker"))




#Calculate Odds Ratio


## Next we can create a table to look at our columns.

smoking_table <- table(smoking_survey$smoking_status, smoking_survey$lung_cancer)


# OR = (exposed with event / exposed without event) / (not exposed with event / not exposed without event)
odds_ratio <- (smoking_table[1,1] / smoking_table[1,2]) / (smoking_table[2,1] / smoking_table[2,2])





## Now to get some bonus stats again with the Epi R package! 

# Calculate Odds Ratio with epiR
results <- epiR::epi.2by2(dat = smoking_table, method = "cohort.count", conf.level = 0.95, outcome = "as.columns")

