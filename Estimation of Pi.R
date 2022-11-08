#2 dimentional estimate

estimate_pi_2d <- function(seed , iterations ){
  # set seed for reproducibility
  set.seed(seed)
  
  # generate the (x, y) points
  x <- runif(n = iterations, min = 0, max = 1)
  y <- runif(n = iterations, min = 0, max = 1)
  
  # calculate 
  sum_sq_xy <- sqrt(x^2 + y^2) 
  
  # see how many points are within circle
  index_within_circle <- which(sum_sq_xy <= 1)
  points_within_circle = length(index_within_circle)
  
  # estimate pi
  pi_est <- 4 * points_within_circle / iterations
  return(pi_est)
}

estimate_pi_2d(seed = 100 , iterations = 1000)
estimate_pi_2d(seed = 100 , iterations = 3000)
estimate_pi_2d(seed = 100 , iterations = 5000)
#
#
#
#
#
#
#
#
#
#
#
#3 dimentional estimate

estimate_pi_3d <- function(seed , iterations ){
  # set seed for reproducibility
  set.seed(seed)
  
  # generate the (x, y) points
  x <- runif(n = iterations, min = 0, max = 1)
  y <- runif(n = iterations, min = 0, max = 1)
  z <- runif(n = iterations, min = 0, max = 1)
  
  # calculate 
  sum_sq_xy <- sqrt(x^2 + y^2 + z^2) 
  
  # see how many points are within circle
  index_within_circle <- which(sum_sq_xy <= 1)
  points_within_circle = length(index_within_circle)
  
  # estimate pi
  pi_est <- 6 * points_within_circle / iterations
  return(pi_est)
}

estimate_pi_3d(seed = 100 , iterations = 10000)
estimate_pi_3d(seed = 100 , iterations = 30000)
estimate_pi_3d(seed = 100 , iterations = 50000)
#
#
#
#
#
#
#
#
#4 dimension estimate
estimate_pi_4d <- function(seed , iterations ){
  # set seed for reproducibility
  set.seed(seed)
  
  # generate the (x, y) points
  x <- runif(n = iterations, min = 0, max = 1)
  y <- runif(n = iterations, min = 0, max = 1)
  z <- runif(n = iterations, min = 0, max = 1)
  q <- runif(n = iterations, min = 0, max = 1)
  
  # calculate 
  sum_sq_xy <- sqrt(x^2 + y^2 + z^2 + q^2) 
  
  # see how many points are within circle
  index_within_circle <- which(sum_sq_xy <= 1)
  points_within_circle = length(index_within_circle)
  
  # estimate pi
  pi_est <- (32 * points_within_circle / iterations)^0.5
  return(pi_est)
}

estimate_pi_4d(seed = 100 , iterations = 10000)
estimate_pi_4d(seed = 100 , iterations = 30000)
estimate_pi_4d(seed = 100 , iterations = 50000)
#
#
#
#
#
#
#
# 5d estimation
estimate_pi_5d <- function(seed , iterations ){
  # set seed for reproducibility
  set.seed(seed)
  
  # generate the (x, y) points
  x <- runif(n = iterations, min = 0, max = 1)
  y <- runif(n = iterations, min = 0, max = 1)
  z <- runif(n = iterations, min = 0, max = 1)
  q <- runif(n = iterations, min = 0, max = 1)
  e <- runif(n = iterations, min = 0, max = 1)
  
  # calculate 
  sum_sq_xy <- sqrt(x^2 + y^2 + z^2 + q^2 + e^2) 
  
  # see how many points are within circle
  index_within_circle <- which(sum_sq_xy <= 1)
  points_within_circle = length(index_within_circle)
  
  # estimate pi
  pi_est <- (60 * points_within_circle / iterations)^0.5
  return(pi_est)
}

estimate_pi_5d(seed = 100 , iterations = 10000)
estimate_pi_5d(seed = 100 , iterations = 30000)
estimate_pi_5d(seed = 100 , iterations = 50000)
#
#
#
#
#
#
#6 dimension estimation

estimate_pi_6d <- function(seed , iterations ){
  # set seed for reproducibility
  set.seed(seed)
  
  # generate the (x, y) points
  x <- runif(n = iterations, min = 0, max = 1)
  y <- runif(n = iterations, min = 0, max = 1)
  z <- runif(n = iterations, min = 0, max = 1)
  q <- runif(n = iterations, min = 0, max = 1)
  e <- runif(n = iterations, min = 0, max = 1)
  f <- runif(n = iterations, min = 0, max = 1)
  
  # calculate 
  sum_sq_xy <- sqrt(x^2 + y^2 + z^2 + q^2 + e^2 + f^2) 
  
  # see how many points are within circle
  index_within_circle <- which(sum_sq_xy <= 1)
  points_within_circle = length(index_within_circle)
  
  # estimate pi
  pi_est <- (384 * points_within_circle / iterations)^(1/3)
  return(pi_est)
}

estimate_pi_6d(seed = 100 , iterations = 10000)
estimate_pi_6d(seed = 100 , iterations = 30000)
estimate_pi_6d(seed = 100 , iterations = 50000)
#
#
#
#
#
#
#
# 7 dimensional analysis

estimate_pi_7d <- function(seed , iterations ){
  # set seed for reproducibility
  set.seed(seed)
  
  # generate the (x, y) points
  x <- runif(n = iterations, min = 0, max = 1)
  y <- runif(n = iterations, min = 0, max = 1)
  z <- runif(n = iterations, min = 0, max = 1)
  q <- runif(n = iterations, min = 0, max = 1)
  e <- runif(n = iterations, min = 0, max = 1)
  f <- runif(n = iterations, min = 0, max = 1)
  o <- runif(n = iterations, min = 0, max = 1)
  
  # calculate 
  sum_sq_xy <- sqrt(x^2 + y^2 + z^2 + q^2 + e^2 + f^2 + o^2) 
  
  # see how many points are within circle
  index_within_circle <- which(sum_sq_xy <= 1)
  points_within_circle = length(index_within_circle)
  
  # estimate pi
  pi_est <- (840 * points_within_circle / iterations)^(1/3)
  return(pi_est)
}

estimate_pi_7d(seed = 100 , iterations = 10000)
estimate_pi_7d(seed = 100 , iterations = 30000)
estimate_pi_7d(seed = 100 , iterations = 50000)


#
#
#
#
#
#
# plot
#
#
#
#
#
# function that estimateds pi and stores each estimated value for each iteration
estimate_pi_data <- function(seed, iterations) {
  set.seed(seed)
  
  # store generated points in a data frame
  df <- data.frame(x <- runif(n = iterations, min = -1, max = 1),
                   y <- runif(n = iterations, min = -1, max = 1))
  
  # add a column to index the number of iterations
  df$iteration <- 1:iterations
  
  # add column to identify if point falls within circle
  df$points_within_circle <- ifelse(sqrt(x^2 + y^2) <= 1, 1, 0) 
  
  # estimate pi
  df$pi_est <- 4 * cumsum(df$points_within_circle) / df$iteration
  
  return(df)
}
#
#
#
#
#
#
#
library(ggplot2)
pi_data <- estimate_pi_data(seed = 100, iterations = 50000)
pi_data
# make plot showing estimated pi as number of points increases
ggplot(pi_data, aes(x = iteration, y = pi_est)) +
  geom_line(col = "blue") +
  geom_hline(yintercept = pi) +
  ylim(c(3, 3.5)) +
  labs(title = expression(paste("Approximation of ", pi)),
       x = "number of points",
       y = expression(paste("estimated value of ",pi)))
#
#
#
#
#
#
#
# function that estimateds pi and stores each estimated value for each iteration
estimate_pi_data3d <- function(seed, iterations) {
  set.seed(seed)
  
  # store generated points in a data frame
  df <- data.frame(x <- runif(n = iterations, min = -1, max = 1),
                   y <- runif(n = iterations, min = -1, max = 1), 
                   z <- runif(n = iterations, min = -1, max = 1))
  
  # add a column to index the number of iterations
  df$iteration <- 1:iterations
  
  # add column to identify if point falls within circle
  df$points_within_circle <- ifelse(sqrt(x^2 + y^2 + z^2) <= 1, 1, 0) 
  
  # estimate pi
  df$pi_est <- 6 * cumsum(df$points_within_circle) / df$iteration
  
  return(df)
}
#
#
# plot
#
#
pi_data <- estimate_pi_data3d(seed = 100, iterations = 50000)
pi_data
# make plot showing estimated pi as number of points increases
ggplot(pi_data, aes(x = iteration, y = pi_est)) +
  geom_line(col = "blue") +
  geom_hline(yintercept = pi) +
  ylim(c(3, 3.5)) +
  labs(title = expression(paste("Approximation of ", pi)),
       x = "number of points",
       y = expression(paste("estimated value of ",pi)))
#
#
#
#
#
#
#
estimate_pi_data4d <- function(seed, iterations) {
  set.seed(seed)
  
  # store generated points in a data frame
  df <- data.frame(x <- runif(n = iterations, min = -1, max = 1),
                   y <- runif(n = iterations, min = -1, max = 1), 
                   z <- runif(n = iterations, min = -1, max = 1), 
                   e <- runif(n = iterations, min = -1, max = 1))
  
  # add a column to index the number of iterations
  df$iteration <- 1:iterations
  
  # add column to identify if point falls within circle
  df$points_within_circle <- ifelse(sqrt(x^2 + y^2 + z^2 + e^2) <= 1, 1, 0) 
  
  # estimate pi
  df$pi_est <- (32 * cumsum(df$points_within_circle) / df$iteration)^0.5
  
  return(df)
}
#
#
#
# plot
#
#
pi_data <- estimate_pi_data4d(seed = 100, iterations = 50000)
pi_data
# make plot showing estimated pi as number of points increases
ggplot(pi_data, aes(x = iteration, y = pi_est)) +
  geom_line(col = "blue") +
  geom_hline(yintercept = pi) +
  ylim(c(3, 3.5)) +
  labs(title = expression(paste("Approximation of ", pi)),
       x = "number of points",
       y = expression(paste("estimated value of ",pi)))
#
#
#


#
#
#
#
estimate_pi_data5d <- function(seed, iterations) {
  set.seed(seed)
  
  # store generated points in a data frame
  df <- data.frame(x <- runif(n = iterations, min = -1, max = 1),
                   y <- runif(n = iterations, min = -1, max = 1), 
                   z <- runif(n = iterations, min = -1, max = 1), 
                   e <- runif(n = iterations, min = -1, max = 1),
                   w <- runif(n = iterations, min = -1, max = 1))
  
  # add a column to index the number of iterations
  df$iteration <- 1:iterations
  
  # add column to identify if point falls within circle
  df$points_within_circle <- ifelse(sqrt(x^2 + y^2 + z^2 + e^2 + w^2) <= 1, 1, 0) 
  
  # estimate pi
  df$pi_est <- (60 * cumsum(df$points_within_circle) / df$iteration)^0.5
  
  return(df)
}
#
#
#
# plot
#
#
pi_data <- estimate_pi_data5d(seed = 100, iterations = 50000)
pi_data
# make plot showing estimated pi as number of points increases
ggplot(pi_data, aes(x = iteration, y = pi_est)) +
  geom_line(col = "blue") +
  geom_hline(yintercept = pi) +
  ylim(c(3, 3.5)) +
  labs(title = expression(paste("Approximation of ", pi)),
       x = "number of points",
       y = expression(paste("estimated value of ",pi)))
#
#
#
#
#
#
#
estimate_pi_data6d <- function(seed, iterations) {
  set.seed(seed)
  
  # store generated points in a data frame
  df <- data.frame(x <- runif(n = iterations, min = -1, max = 1),
                   y <- runif(n = iterations, min = -1, max = 1), 
                   z <- runif(n = iterations, min = -1, max = 1), 
                   e <- runif(n = iterations, min = -1, max = 1), 
                   w <- runif(n = iterations, min = -1, max = 1),
                   r <- runif(n = iterations, min = -1, max = 1))
  
  # add a column to index the number of iterations
  df$iteration <- 1:iterations
  
  # add column to identify if point falls within circle
  df$points_within_circle <- ifelse(sqrt(x^2 + y^2 + z^2 + e^2 + w^2 + r^2) <= 1, 1, 0) 
  
  # estimate pi
  df$pi_est <- (384 * cumsum(df$points_within_circle) / df$iteration)^(1/3)
  
  return(df)
}
#
#
#
# plot
#
#
pi_data <- estimate_pi_data6d(seed = 100, iterations = 50000)
pi_data
# make plot showing estimated pi as number of points increases
ggplot(pi_data, aes(x = iteration, y = pi_est)) +
  geom_line(col = "blue") +
  geom_hline(yintercept = pi) +
  ylim(c(3, 3.5)) +
  labs(title = expression(paste("Approximation of ", pi)),
       x = "number of points",
       y = expression(paste("estimated value of ",pi)))
#
#
#
#
#
#
#
estimate_pi_data7d <- function(seed, iterations) {
  set.seed(seed)
  
  # store generated points in a data frame
  df <- data.frame(x <- runif(n = iterations, min = -1, max = 1),
                   y <- runif(n = iterations, min = -1, max = 1), 
                   z <- runif(n = iterations, min = -1, max = 1), 
                   e <- runif(n = iterations, min = -1, max = 1),
                   w <- runif(n = iterations, min = -1, max = 1),
                   r <- runif(n = iterations, min = -1, max = 1),
                   i <- runif(n = iterations, min = -1, max = 1))
  
  # add a column to index the number of iterations
  df$iteration <- 1:iterations
  
  # add column to identify if point falls within circle
  df$points_within_circle <- ifelse(sqrt(x^2 + y^2 + z^2 + e^2) <= 1, 1, 0) 
  
  # estimate pi
  df$pi_est <- (840 * cumsum(df$points_within_circle) / df$iteration)^(1/3)
  
  return(df)
}
#
#
#
# plot
#
#
pi_data <- estimate_pi_data7d(seed = 100, iterations = 50000)
pi_data
# make plot showing estimated pi as number of points increases
ggplot(pi_data, aes(x = iteration, y = pi_est)) +
  geom_line(col = "blue") +
  geom_hline(yintercept = pi) +
  ylim(c(3, 3.5)) +
  labs(title = expression(paste("Approximation of ", pi)),
       x = "number of points",
       y = expression(paste("estimated value of ",pi)))
#
#



