# Load required libraries
library(psych)
library(tableone)

#Using mtcars

# Convert to binary variables for further analysis
mtcars$cyl_bin <- ifelse(mtcars$cyl > 6, 1, 0)
mtcars$gear_bin <- ifelse(mtcars$gear > 4, 1, 0)
mtcars$carb_bin <- ifelse(mtcars$carb > 2, 1, 0)
mtcars$mpg_bin <- ifelse(mtcars$mpg > median(mtcars$mpg), 1, 0)
mtcars$hp_bin <- ifelse(mtcars$hp > median(mtcars$hp), 1, 0)

# Select the binary variables
binary_vars <- c("am", "cyl_bin", "gear_bin", "carb_bin", "mpg_bin", "hp_bin")

# Calculate tetrachoric correlation matrix
tetra_corr <- tetrachoric(mtcars[binary_vars])
tetra_corr$rho[lower.tri(correlation_matrix)] <- NA

# Display the correlation matrix
print(tetra_corr$rho)

# Function to convert correlation to p-values
cor_to_p <- function(cor, n, method = "pearson") {
  # Statistic
  if (method == "kendall") {
    insight::format_alert(
      "p-value estimation for Kendall's correlation is not perfectly correct.",
      "Help us to improve it."
    )
    statistic <- (3 * cor * sqrt(n * (n - 1))) / sqrt(2 * (2 * n + 5))
  } else {
    statistic <- cor * sqrt((n - 2) / (1 - cor^2))
  }
  
  # p-value
  if (method == "kendall") {
    p <- 2 * stats::pnorm(-abs(statistic))
  } else {
    p <- 2 * stats::pt(-abs(statistic), df = n - 2)
  }
  
  list(p = p, statistic = statistic)
}

# Calculate p-values for the correlation matrix
corp <- cor_to_p(tetra_corr$rho, n, method = "pearson")
p_logical<-as.matrix(corp$p<0.05)

# Display the correlation p-value matrix
print(corp$p)

# Create a color palette from red to blue
require(corrplot)
color_palette <- colorRampPalette(c("#A54A4A", "#556B9F"))(100)

# Plot correlation matrix with p-values and customized color

corrplot(tetra_corr$rho,is.corr=FALSE,type = "lower",col = color_palette,
         tl.col = "black",tl.cex=.6,cl.cex = 0.5,cl.align.text = "l",
         cl.offset = 0.1,p.mat = p_logical,pch = "*",pch.col="yellow",
         sig.level = 0.05,pch.cex = 1.5,title = "Tetrachoric Correlation")


