#problem 1
#given N(31.5, 0.8)
pnorm(1,lower.tail = TRUE) - pnorm(-1, lower.tail = TRUE)

#problem 2
pnorm(33.9, mean = 31.5, sd = 0.8, lower.tail = TRUE) - pnorm(29.1, mean = 31.5, sd = 0.8, lower.tail = TRUE)

#problem 3, part A
data <- c(99, 123, 75, 138, 105, 65, 116)
sample_mean <- mean(data)
print(sample_mean)

#part B
sample_var <- var(data)
sample_sd <- sd(data)
print(paste("This is sample variance:", sample_var))
print(paste("This is sample SD:", sample_sd))

#part C
degrees_of_freedom <- length(data) - 1
bound <- qt(0.025, degrees_of_freedom, lower.tail=TRUE) * sample_sd / sqrt(degrees_of_freedom)
abs_bound <- abs(bound)
print(paste("The 95% interval is: (",(sample_mean - abs_bound), "; ", (sample_mean + abs_bound), ")"))

#part D
null_value <- 90
confidence_level <- 0.9
result <- t.test(data, mu = null_value, alternative = "two.sided",conf.level = confidence_level)
print(result)
#Since p-value > alpha (0.23>0.1), we do not have sufficient evidence
#to reject the H0.

#problem 4
#Given: n=81, sample mean = 57.8 feet, s = 6.02 feet
#Competitors claim 60 feet

#Part A: 90% and 95% C-Intervals for Mu
degrees_of_freedom <- 80
sample_mean <- 57.8
sample_sd <- 6.02
bound <- qt(0.05, degrees_of_freedom, lower.tail=TRUE) * sample_sd / sqrt(degrees_of_freedom)
abs_bound <- abs(bound)
print(paste("The 90% interval is: (",(sample_mean - abs_bound), "; ", (sample_mean + abs_bound), ")"))
bound <- qt(0.025, degrees_of_freedom, lower.tail=TRUE) * sample_sd / sqrt(degrees_of_freedom)
abs_bound <- abs(bound)
print(paste("The 95% interval is: (",(sample_mean - abs_bound), "; ", (sample_mean + abs_bound), ")"))

#Part B
#H0: Mu = 60; Ha: Mu < 60
#Since n>30, by CLT we can use Normal distribution
n <- 81
sample_mean <- 57.8
sample_sd <- 6.02
null_value <- 60
confidence_level <- 0.95
z_score <- (sample_mean - null_value) / (sample_sd / sqrt(n))
p_value <- pnorm(z_score, lower.tail = TRUE)
cat("Z-score:", z_score, "\n")
cat("p-value:", p_value, "\n")

