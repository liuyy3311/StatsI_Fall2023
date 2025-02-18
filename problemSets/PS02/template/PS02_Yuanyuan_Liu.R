#Question1
#a
# Define the observed data
observed <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, ncol = 3,byrow = TRUE)
rownames(observed) <- c("Upper Class", "Lower Class")
colnames(observed) <- c("Not Stopped", "Bribe requesed","Stopped/given warnning")
observed
# Calculate the expected frequencies
row_sums <- rowSums(observed)
col_sums <- colSums(observed)
total_sum <- sum(observed)
for (i in 1:2) {
  for (j in 1:3) {
    expected[i, j] <- (row_sums[i] * col_sums[j]) / total_sum
  }
}
expected
# Run Chi square test
chi_squared <- sum(((observed - expected)^2) / expected)
chi_squared

#b
df <- (nrow(observed) - 1) * (ncol(observed) - 1)
p_value <- pchisq(chi_squared,df,lower.tail = FALSE)
p_value
if(p_value<0.1){
  cat("under the significance level a=0.1,we can reject null hypothsis,the officer were more likely to solicit a bribe from drivers depending on their class.\n")
}else{
  cat("under the significance level a=0.1,we fail to reject null hypothsis,the officer were less likely to solicit a bribe from drivers depending on their class.\n")
}

#c
# Calculate the standardized residuals for each cell
residuals <- (observed - expected) / sqrt(expected*(1 - row_sums/total_sum) * (1 - col_sums/total_sum))
# Create an empty result table
result_table <- matrix(NA, nrow = 2, ncol = 3)
colnames(result_table) <- c("Not Stopped", "Bribe requested", "Stopped/given warning")
rownames(result_table) <- c("Upper class", "Lower class")
result_table <- format(residuals, digits = 5)
result_table

#d
#For data with positive standardized residuals, which indicates that the observed frequency is higher than the predetermined frequency, positive residual standard deviations indicate that police are more likely to obtain tickets from these drivers. For data with negative standardized residuals, this means that the observed frequency is lower than the predetermined frequency, in which case a negative residual means that the police are less likely to obtain tickets from these drivers. Based on the above data, the social class of drivers may have an impact on bribery behavior between police and drivers. Police are more likely to demand bribes from drivers of higher social class.

   
#Question2
#a
#H0：The reservation policy has no effect on the number of new or repaired drinking water facilities in the villages
#H1:The reservation policy has effect on the number of new or repaired drinking water facilities in the villages

#b
#run the bivariate regression
data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
X <- data$reserved  
Y <- data$water
model <- lm(Y~X)
summary(model)
