# Group_assignment
Group assignment

Example code: 
library(slidingwindow)
# Loading data
data <- read_data_frame("C:/Users/xiaodu/Downloads/Example_window.csv")
data <- combo_axis(data, x, y, z)

# applying loop function
data <- window_loop(data, window_size = 100, window_function = window_mean)
data$mean <- data$result

# Plot the results
plot <- plot_accelaration(data, plot_mean = TRUE)
print(plot)
