# Code written by Cody Micah Carmichael, MPH, CPH for BroadlyEpi.com 
#Please visit BroadlyEpi.com for tutorials or other services. 

#set your working directory if you haven't already, and pull file into your project folder!

#setwd()

#install.packages("pacman")
pacman::p_load(
  readxl, 
  zoo, # We'll need this and tidyr later for some special functions
  tidyr
)



#Reads in XSLX file, directs read in to Table 2, and extracts all cells between B3 and T151.
Table_2_DataFrame <- readxl::read_xlsx(path =  "00_Suicide_2018_NOT_CLEANED.xlsx", range ="Table 2!B3:T151")


#now remove it to make life easier. to do this, we're dropping the first line, or better termed, keeping all except the first line.
#Make sure to not run this more than once. 
Table_2_DataFrame <- Table_2_DataFrame[-1,]

#rename columns to agegroup0-15 to transfer later, short intermediate step.
colnames(Table_2_DataFrame) <- c('RegYear','ASAB','AllAges', "AgeGroup1", "AgeGroup2", "AgeGroup3", "AgeGroup4", "AgeGroup5","AgeGroup6", "AgeGroup7", "AgeGroup8",
                     "AgeGroup9", "AgeGroup10", "AgeGroup11", "AgeGroup12", "AgeGroup13", "AgeGroup14", "AgeGroup15", "AgeGroup16")




#Now, to make life easier, for every NA year, we take the previous year and add it down the table as a value. 
Table_2_DataFrame$RegYear <- zoo::na.locf(zoo::na.locf(Table_2_DataFrame$RegYear),fromLast=TRUE)

#works to make long
Table_2_DataFrame <- Table_2_DataFrame %>%
  tidyr::pivot_longer(cols = starts_with('AgeGroup'))

# Drop - to NA so we can preserve no counts while also switching to num
Table_2_DataFrame$value[Table_2_DataFrame$value == "-"] <- NA


#replace AgeGroups
Table_2_DataFrame["name"][Table_2_DataFrame["name"] == "AgeGroup1"] <- "Under 15"
Table_2_DataFrame["name"][Table_2_DataFrame["name"] == "AgeGroup2"] <- "15-19"
Table_2_DataFrame["name"][Table_2_DataFrame["name"] == "AgeGroup3"] <- "20-24"
Table_2_DataFrame["name"][Table_2_DataFrame["name"] == "AgeGroup4"] <- "25-29"
Table_2_DataFrame["name"][Table_2_DataFrame["name"] == "AgeGroup5"] <- "30-34"
Table_2_DataFrame["name"][Table_2_DataFrame["name"] == "AgeGroup6"] <- "35-39"
Table_2_DataFrame["name"][Table_2_DataFrame["name"] == "AgeGroup7"] <- "40-44"
Table_2_DataFrame["name"][Table_2_DataFrame["name"] == "AgeGroup8"] <- "45-49"
Table_2_DataFrame["name"][Table_2_DataFrame["name"] == "AgeGroup9"] <- "50-54"
Table_2_DataFrame["name"][Table_2_DataFrame["name"] == "AgeGroup10"] <- "55-59"
Table_2_DataFrame["name"][Table_2_DataFrame["name"] == "AgeGroup11"] <- "60-64"
Table_2_DataFrame["name"][Table_2_DataFrame["name"] == "AgeGroup12"] <- "65-69"
Table_2_DataFrame["name"][Table_2_DataFrame["name"] == "AgeGroup13"] <- "70-74"
Table_2_DataFrame["name"][Table_2_DataFrame["name"] == "AgeGroup14"] <- "75-79"
Table_2_DataFrame["name"][Table_2_DataFrame["name"] == "AgeGroup15"] <- "80-84"
Table_2_DataFrame["name"][Table_2_DataFrame["name"] == "AgeGroup16"] <- "85+"

#Or if you want a quicker way about it, try creating a list of the year groups and age groups and iterate down them. I will cover that in later article.

names(Table_2_DataFrame)[names(Table_2_DataFrame) == 'name'] <- 'AgeGroup'
names(Table_2_DataFrame)[names(Table_2_DataFrame) == 'AllAges'] <- 'AllAgesCounts'
names(Table_2_DataFrame)[names(Table_2_DataFrame) == 'value'] <- 'Counts'
Table_2_DataFrame$ASAB <- as.factor(Table_2_DataFrame$ASAB)
Table_2_DataFrame$Counts <- as.numeric(Table_2_DataFrame$Counts)




#write.csv(Table_2_DataFrame, file = "Table2.csv")
