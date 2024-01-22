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
