## First, install packages if you don't already have them 
### For Static Graphs
install.packages("ggplot2")

### For Interactive Graphs
install.packages("plotly")

### For some basic data manipulation
install.packages("dplyr")

## Now load in the libraries. 
library(ggplot2)
library(plotly)
library(dplyr)

## The remainder of our work will be in Base R. 


# Part 1: Data read-in and formatting. 
## First we can use read.csv to open the practice file. 
Dis_Data <- read.csv("Data/disease_dataset.csv")


## We're going to make a quick function
Group_By_Distance <- function(df, Disease_String){
  # Filter the data for the specified disease
  df <- df[df$Disease == Disease_String,]
  # Group by distance from Well A and summarize
  grouped_DF <- df %>%
    dplyr::group_by(Distance.from.Well.A) %>%
    dplyr::summarize(count_A = n())
  return(grouped_DF)
}


## Create a Distance X Count for Flu, and Cholera 
Flu_Distance <- Group_By_Distance(Dis_Data, "Influenza")
Cholera_Distance <- Group_By_Distance(Dis_Data, "Cholera")

## If we just want to do things quick, cor will provide quick correlation. 
cor(Flu_Distance$Distance.from.Well.A, Flu_Distance$count_A)
cor(Cholera_Distance$Distance.from.Well.A, Cholera_Distance$count_A)
cor(NED_Distance$Distance.from.Well.A, NED_Distance$count_A)
# Create the scatterplot showing the trend
Flu_Static <- ggplot(Flu_Distance, aes(x = Distance.from.Well.A, y = count_A)) +
  geom_point() +  # Add scatterplot points
  geom_smooth(method = "lm", se = FALSE) +  # Add a linear regression line without confidence interval
  scale_x_continuous(limits = c(0, 25),breaks = 0:25) +  # Set X-axis breaks every 1 mile
  scale_y_continuous(limits = c(0, 25), breaks = 0:25) +  # Set Y-axis limits and breaks every 1 mile
  labs(title = "Correlation between Distance from Well A and Count of Influenza Cases",
       x = "Distance from Well A (miles)",
       y = "Count of Influenza Cases") +
  theme_minimal()


Cholera_Static <- ggplot(Cholera_Distance, aes(x = Distance.from.Well.A, y = count_A)) +
  geom_point() +  # Add scatterplot points
  geom_smooth(method = "lm", se = FALSE) +  # Add a linear regression line without confidence interval
  scale_x_continuous(limits = c(0, 25),breaks = 0:25) +  # Set X-axis breaks every 1 mile
  scale_y_continuous(limits = c(0, 25), breaks = 0:25) +  # Set Y-axis limits and breaks every 1 mile
  labs(title = "Correlation between Distance from Well A and Count of Cholera Cases",
       x = "Distance from Well A (miles)",
       y = "Count of Cholera Cases") +
  theme_minimal()






# Create the plotly scatterplot
Cholera_Interactive <- plot_ly(Cholera_Distance, x = ~Distance.from.Well.A, y = ~count_A, type = 'scatter', mode = 'markers') %>%
  add_markers() %>%
  add_lines(
    x = ~Distance.from.Well.A, 
    y = ~fitted(lm(count_A ~ Distance.from.Well.A, data = Cholera_Distance))
  ) %>%
  layout(
    title = "Correlation between Distance from Well A and Count of Cholera Cases",
    xaxis = list(title = "Distance from Well A (miles)", dtick = 1, range = c(0,25)),
    yaxis = list(title = "Count of Cholera Cases", dtick = 1, range = c(0, 25))
  )

# Render the plot
Cholera_Interactive








