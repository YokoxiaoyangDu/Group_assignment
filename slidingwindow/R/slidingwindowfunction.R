#' Load a file and convert the time column.
#'
#' @param file_path Path to the CSV file
#' @return a data frame
read_data_frame <- function(file_path) {
  temp <- read.csv(file_path)
  temp$time <- as.POSIXct(temp$time, origin = "1970-01-01")
  return(temp)
}

data_acceleration<- read_data_frame("C:/Users/xiaodu/Downloads/Example_window.csv")

#' Calculate Euclidean norm
#' @param data dataset
#' @param x x axis
#' @param y y axis
#' @param z z axis
#' @return a data frame
combo_axis <- function(data,x,y,z) {
  temp <- data
  temp$acceleration<- sqrt(temp$x^2 + temp$y^2 + temp$z^2)
  return(temp)
}


#' calculate mean of the window
#' @param window window value
#' @return mean of the window
window_mean <- function(window) {
  return(mean(window))
}

#' Assign binary +1 or -1
#' @param window window value
#' @return +1 or -1 given calculation
window_binary <- function(window) {
  diff <- window[length(window)] - window[1]
  if (diff > 0)
    return(1)
   else
    return(-1)
}


#' Apply iteration on the functions
#' @param data data frame
#' @param window_size size of the sliding window
#' @param window_function functions created above
#' @return a data frame
window_loop <- function(data, window_size, window_function, ...) {
  n <- nrow(data)
  result <- rep(NA, n)

  for (i in 1:(n - window_size + 1)) {
    window <- data$acceleration[i:(i + window_size - 1)]
    result[i + window_size - 1] <- window_fun(window, ...)
  }

  data$result <- result
  return(data)
}


#' Plot acceleration results
#' @param data data frame
#' @param use_mean show mean results
#' @param use_binary show binary results
#' @return a data frame
plot_accelaration <- function(data,use_mean = TRUE,use_binary = TRUE) {
  figure <- ggplot2::ggplot(data, ggplot2::aes(x = time)) +
    ggplot2::geom_line(ggplot2::aes(y = acceleration), color = "blue") +
    ggplot2::labs(x = "Time", y = "Acceleration", title = "Acceleration Time Series")

  if (use_mean && "mean" %in% names(data)) {
    figure <- figure + ggplot2::geom_line(ggplot2::aes(y = mean), color = "green")
  }

  if (use_binary && "binary" %in% names(data)) {
    figure <- figure + ggplot2::geom_line(ggplot2::aes(y = binary), color = "red")
  }

  return(figure)
}
