# Statistical Analysis and Visualization Script
# This script performs statistical analysis on a generated dataset with three groups
# using Kruskal-Wallis test and conducts post hoc tests (Dunn's test) for multiple comparisons.
# Additionally, it creates a boxplot visualization of the dataset.

# Install and load required packages with error handling
if (!requireNamespace("dunn.test", quietly = TRUE)) {
  install.packages("dunn.test")
}
library(dunn.test)

# Set seed for reproducibility
set.seed(123)

# Generate a dataset with three groups
group1 <- rnorm(30, mean = 5, sd = 2)
group2 <- rnorm(30, mean = 8, sd = 2)
group3 <- rnorm(30, mean = 6, sd = 2)

# Combine the groups into a single dataset
yourdataset <- data.frame(
  Value = c(group1, group2, group3),
  group = rep(c("Group1", "Group2", "Group3"), each = 30)
)

# Print a summary of the generated dataset
summary(yourdataset)

# Perform Kruskal-Wallis test
kruskal_result <- kruskal.test(Value ~ group, data = yourdataset)
# Conduct Kruskal-Wallis test to assess differences in 'Value' across 'group' levels in 'yourdataset'

# Display Kruskal-Wallis test results
print(kruskal_result)
# Print Kruskal-Wallis test results, including test statistic, degrees of freedom, and p-value

# Check for significance; if Kruskal-Wallis is significant, conduct post hoc tests (Dunn's test)
if (kruskal_result$p.value < 0.05) {
  dunn_result <- dunn.test(yourdataset$Value, yourdataset$group, method = "bonferroni")
  # Perform Dunn's test with Bonferroni correction for multiple comparisons
  # Assess differences in 'Value' among 'group' levels when Kruskal-Wallis is significant
  
  # Display post hoc test results
  print(dunn_result)
}

# Extract and organize results with letters
dunn_groups <- data.frame(
  Comparison = dunn_result$comparisons,
  P.adjusted = dunn_result$P.adjusted
)

# Create 'dunn_groups' data frame with comparison groups and adjusted p-values from Dunn's test

# Filter rows where P.adjusted is < 0.05
significant_comparisons <- dunn_groups[dunn_groups$P.adjusted < 0.05, c("Comparison", "P.adjusted")]
# Select rows where adjusted p-value is < 0.05, creating 'significant_comparisons' data frame

# Display resulting data frame
print(significant_comparisons)
# Print data frame with significant group comparisons and their adjusted p-values

# Additional script for pairwise Wilcoxon test
# Check if the Kruskal-Wallis test is significant
if (kruskal_result$p.value < 0.05) {
  # Perform Dunn's test with Bonferroni correction for multiple comparisons
  dunn_result <- dunn.test(yourdataset$Value, yourdataset$group, method = "bonferroni")
  
  # Display post hoc test results
  print(dunn_result)
} else {
  # If the Kruskal-Wallis test is not significant, print a message or take other actions
  print("No significant differences detected in the Kruskal-Wallis test.")
}

# Boxplot visualization
par(mar = c(5, 5, 4, 2) + 0.1, cex.lab = 1.8)  # Adjust the values as needed

# Set y-axis label size
ylim <- c(0, max_observed + 0.25 * max_observed)

# Generate colors based on the number of unique groups
colors <- rainbow(length(unique(yourdataset$group)))

# Create the boxplot
myplot <- boxplot(yourdataset$Value ~ yourdataset$group, 
                  ylab = "Value", boxwex = 0.5, xaxt = "n", yaxt = "n",
                  col = colors, ylim = ylim)    

# Set axis labels and appearance
axis(1, at = 1:length(unique(yourdataset$group)), labels = unique(yourdataset$group), 
     cex.axis = 1.5, font.axis = 2, tick = FALSE, cex = 0.3)

axis(2, cex.axis = 1.2, font.axis = 2, ylab = "")  # Set y-axis label to an empty string



### End of script
