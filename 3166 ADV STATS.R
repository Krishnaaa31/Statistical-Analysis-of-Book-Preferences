setwd("C:\\Users\\Admin\\Downloads")
df = read.csv("stats.csv",header = TRUE, stringsAsFactors = FALSE)
df
colnames(df)
new_names <- c("Timestamp","Emailaddress","Gender","Preference","Frequency","Convenience","Portability","Spending","Quantity","Title Influence","Purchase Factors","Environment Impact","Experience Diff","E-books' Adv","Print's Adv","Access Impact","Satisfaction","Recommendation Likelihood")  
colnames(df) <- new_names
df
head(df)
colnames(df)
df
table_gender_preference <- table(df$Gender, df$Preference)


#Null Hypothesis: The proportion of males preferring e-books is the same as the proportion of females preferring e-books.
#Alternative Hypothesis: The proportion of males preferring e-books is different from the proportion of females preferring e-books.
chi_square_result <- chisq.test(table_gender_preference)
print(chi_square_result)
if (chi_square_result$p.value < 0.05) {
  print("Reject the null hypothesis: There is a significant difference in book preferences between males and females.")
} else {
  print("Fail to reject the null hypothesis: There is no significant difference in book preferences between males and females.")
}

# Assuming 'df' is your data frame
# Choose two levels for comparison (e.g., 'E-books' and 'Print books')
subset_df <- df[df$Preference %in% c('E-books', 'Print books'), ]
# Perform t-test
#Null Hypothesis (H0): There is no significant difference in satisfaction between users who prefer E-books and those who prefer Print books.
#Alternative Hypothesis (H1): There is a significant difference in satisfaction between users who prefer E-books and those who prefer Print books.
#H0: μ1 = μ2 (The population means of satisfaction for E-books and Print books users are equal)
#H1: μ1 ≠ μ2 (The population means of satisfaction for E-books and Print books users are not equal)
t_test_result <- t.test(Satisfaction ~ Preference, data = subset_df)
print(t_test_result)
if (t_test_result$p.value < 0.05) {
  print("Reject the null hypothesis: There is a significant difference in satisfaction between users who prefer e-books and those who prefer print books.")
} else {
  print("Fail to reject the null hypothesis: There is no significant difference in satisfaction between users who prefer e-books and those who prefer print books.")
}


# Assuming 'df' is your dataframe
# Select only the necessary columns for the analysis
subset_df <- df[, c("Preference", "Spending")]

# Perform Kruskal-Wallis test
kruskal_result <- kruskal.test(Spending ~ Preference, data = subset_df)

# Print the result
print(kruskal_result)
if (kruskal_result$p.value < 0.05) {
  print("Reject the null hypothesis: There is a significant difference in spending among different Preferences.")
} else {
  print("Fail to reject the null hypothesis: There is no significant difference in spending among different Preferences.")
}

