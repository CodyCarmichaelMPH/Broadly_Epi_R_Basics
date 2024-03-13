
install.packages("tidyverse")

library(tidyverse)


 df <- read.csv("Belfast_Suicide_Counts.csv")



# Remove 'X' from year column names
names(df)[-1] <- sub("X", "", names(df)[-1])

# Reshape to long format
long_df <- df %>% 
  pivot_longer(
    cols = -Assembly.Area, 
    names_to = "Year", 
    values_to = "Count"
  )

# Spread to wide format
wide_df <- long_df %>% 
  pivot_wider(
    names_from = Assembly.Area, 
    values_from = Count
  )




## The easy way

t_test_result <- t.test(wide_df$`Belfast North`, wide_df$`Belfast South`)





# Calculate means and standard deviations
mean_north <- mean(wide_df$`Belfast North`)
mean_south <- mean(wide_df$`Belfast South`)
sd_north <- sd(wide_df$`Belfast North`)
sd_south <- sd(wide_df$`Belfast South`)

# Calculate sample sizes
n_north <- length(wide_df$`Belfast North`)
n_south <- length(wide_df$`Belfast South`)

# Calculate standard error of the difference
se_difference <- sqrt(sd_north^2 / n_north + sd_south^2 / n_south)

# Compute the t-statistic
t_statistic <- (mean_north - mean_south) / se_difference

# Calculate degrees of freedom (approximation for unequal variances)
df <- ((sd_north^2 / n_north + sd_south^2 / n_south)^2) /
  ((sd_north^4 / (n_north^2 * (n_north - 1))) + (sd_south^4 / (n_south^2 * (n_south - 1))))

# Calculate the p-value
p_value <- 2 * pt(-abs(t_statistic), df)

# Print results
cat("T-statistic:", t_statistic, "\nP-value:", p_value, "\n")



