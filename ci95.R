ci95 <- function(x) {
   # Function to calculate 95% confidence intervals for set of numbers
   t.value <- qt(0.975, length(x)-1)
   standard.error <- sqrt(var(x) / length(x))
   ci <- t.value * standard.error
   
   # Return the lower and upper 95% CI around the mean
   return(c(mean(x)-ci, mean(x)+ci))
}