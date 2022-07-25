#Load in your working directory, where your files are stored for this exercise. Uncomment line below and fill () with "your/file/location", whatever that may be.
#setwd()
#use pacman to read in libraries. If you need to install pacman, use:
# install.packages("pacman")
#In the console.
pacman::p_load(
  readxl
)

# read in selected cells
Table_3_DataFrame <- readxl::read_xlsx(path =  "00_Suicide_2018_NOT_CLEANED.xlsx", range ="Table 3!B4:E52")



# write out CSVs
write.csv(Table_3_DataFrame, file = "Table3.csv")
