# Load necessary packages
library(deSolve)
library(plotly)

# Define the SEIR model
seir_model <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -beta * S * I / N
    dE <- beta * S * I / N - sigma * E
    dI <- sigma * E - gamma * I
    dR <- gamma * I
    return(list(c(dS, dE, dI, dR)))
  })
}

# Set initial conditions and parameters
N <- 1000  # Total population
initial_conditions <- c(S = 999, E = 0, I = 1, R = 0)  # S, E, I, R initial conditions
parameters <- c(beta = 0.5, gamma = 0.1, sigma = 1/5)  # beta, gamma, sigma parameters
times <- seq(0, 50, by = 1)  # Time from 0 to 50 days

# Solve the SEIR model
seir_solution <- ode(y = initial_conditions, times = times, func = seir_model, parms = parameters)

# Plot the results with Plotly
fig <- plot_ly() %>%
  add_trace(x = seir_solution[,"time"], y = seir_solution[,"S"], type = 'scatter', mode = 'lines', name = 'Susceptible', line = list(color = 'blue')) %>%
  add_trace(x = seir_solution[,"time"], y = seir_solution[,"E"], type = 'scatter', mode = 'lines', name = 'Exposed', line = list(color = 'orange')) %>%
  add_trace(x = seir_solution[,"time"], y = seir_solution[,"I"], type = 'scatter', mode = 'lines', name = 'Infected', line = list(color = 'red')) %>%
  add_trace(x = seir_solution[,"time"], y = seir_solution[,"R"], type = 'scatter', mode = 'lines', name = 'Recovered', line = list(color = 'green')) %>%
  layout(title = 'SEIR Model Simulation',
         xaxis = list(title = 'Time'),
         yaxis = list(title = 'Population'))

# Display the plot
fig
