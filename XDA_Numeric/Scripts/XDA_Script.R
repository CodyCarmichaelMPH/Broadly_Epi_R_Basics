pacman::p_load(
  tidyr,
  ggplot2,
  plotly
)

# XDA on non-text data
Rate_Per_100k <- read.csv("table3.csv")

#We have an index called X which we don't need, so we can remove it.

Rate_Per_100k <- Rate_Per_100k[,-1]

# We want to create 3 seperate DataFrames for Male, Female, and Total. Since a lot will be reptition, let's use something called a function.

Core_Stats <- function(DataFrame, colname){
  
  #for these calculations, we need to pull from the list of values, transform them into a numeric vector, then run the calculations
  Avg <- mean(as.numeric(unlist(DataFrame[colname])))
  Median <- median(as.numeric(unlist(DataFrame[colname])))
  StD <- sd(as.numeric(unlist(DataFrame[colname])))
  Variance <- var(as.numeric(unlist(DataFrame[colname])))
  Min <- min(DataFrame[colname])
  Max <- max(DataFrame[colname])
  Category <- colname
  
  Core_Stat_Block <- data.frame(Category,Min, Max, Avg, Median, StD, Variance)
  return(Core_Stat_Block)
}


#Let's test this out!
Female_CS <- Core_Stats(Rate_Per_100k, "Female")
Male_CS <- Core_Stats(Rate_Per_100k, "Male")
Total_CS <- Core_Stats(Rate_Per_100k, "Total")

#Now row bind (rbind) them to one DataFrame. Can use for tables later
######Create KableTable for Website with this
Stats_DataFrame <- rbind(Female_CS, Male_CS, Total_CS)

#and remove unneeded DFs
rm(Female_CS, Male_CS, Total_CS)





#Now for the rest, we can turn our data a bit longer by creating an umbrella AGAB (Assigned Gender at Birth) column with values.

Rate_Per_100k <- Rate_Per_100k %>%
  tidyr::pivot_longer(cols = c("Male", "Female", "Total"))

#rename columns
colnames(Rate_Per_100k) <- c("RegYear", "AGAB", "Count")


#Create Boxplot
BoxPlot <- ggplot2::ggplot(data = Rate_Per_100k) +
  geom_boxplot(mapping = aes(x = AGAB, y = Count)) +
  labs(title = "Suicides per 100,000, 1970-2018 by Assigned Gender at Birth",
         subtitle = "Median based Box Plot",
         caption = "Data source: NISRA.gov.uk")

#Round Count Values to 3 decimal places
Rate_Per_100k$Count <- round(Rate_Per_100k$Count, 3)

#Create interactive graph using Plotly
plotly::plot_ly(data = Rate_Per_100k, x = ~RegYear, y = ~Count, color = ~AGAB) %>%
  add_lines()  %>% 
  layout( title = "Interactive Timeseries: Suicides by Sex and Year, 1970-2018" , xaxis = list(title = 'Registration Year'),yaxis = list(title = 'Count'))
