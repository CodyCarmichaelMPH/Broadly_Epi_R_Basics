# Install and load necessary packages
install.packages("deSolve")
install.packages("plotly")
library(deSolve)
library(plotly)

# Define the SIR model
sir_model <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -beta * S * I / N
    dI <- beta * S * I / N - gamma * I
    dR <- gamma * I
    
    return(list(c(dS, dI, dR)))
  })
}

# Set initial conditions and parameters
initial_conditions <- c(S = 999, I = 1, R = 0)  # Assuming a total population N of 1000
parameters <- c(beta = 0.3, gamma = 0.1, N = 1000)  # Added N to parameters
times <- seq(0, 50, by = 1)  # Time from 0 to 50 days

# Solve the SIR model
sir_solution <- ode(y = initial_conditions, times = times, func = sir_model, parms = parameters)

# Plot the results with Plotly
df <- as.data.frame(sir_solution)

plot_ly(df, x = ~time) %>%
  add_trace(y = ~S, name = 'Susceptible', mode = 'lines', type = 'scatter', line = list(color = 'blue')) %>%
  add_trace(y = ~I, name = 'Infected', mode = 'lines', type = 'scatter', line = list(color = 'red')) %>%
  add_trace(y = ~R, name = 'Recovered', mode = 'lines', type = 'scatter', line = list(color = 'green')) %>%
  layout(title = 'SIR Model Simulation',
         xaxis = list(title = 'Time'),
         yaxis = list(title = 'Population'))
