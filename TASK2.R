#install packages
install.packages(c("car","caret","corrplot", "tidyverse", "readxl", "randomForest","datarium","qqplotr"))
#Load libraries
lapply(c("car","caret","corrplot","dplyr","tidyverse", "readxl", "randomForest","datarium","qqplotr","gridExtra","e1071","mgcv"), library, character.only = TRUE)
#Import Excel
df <- read_excel("/Users/bharath/Desktop/R_Studio/Task 2 - Statistical Analysis/concrete+compressive+strength/concrete_compressive_strength.xlsx")
#Overview of Data
str(df)
#First 10 Rows
head(df,10)
# Remove anything after "("
names(df) <- gsub("\\s*\\(.*\\)", "", names(df)) 
#Replace Blank spaces with an underscore
names(df) <- gsub(" ", "_", names(df))
#Add a new column cement_water_ratio
df$Cement_Water_Ratio <- df$Cement / df$Water
#Display New Column Names.
names(df)
#Check for null values
null_counts <- sapply(df, function(x) sum(is.na(x)))
#Print the Count of Null values
print(null_counts)
#Check for Duplicates
duplicate_count <- sum(duplicated(df))
#Print Number of Duplicates
cat("Number of duplicate rows in the dataset:", duplicate_count, "\n")
#Remove Duplicates
df <- unique(df)
# Number of Unique values in each Column
unique_count <- sapply(df, function(x) length(unique(x)))
# Print the number of unique values for each column
print(unique_count)
#Descriptive Stats of the Dataset
summary(df)
# Convert "Concrete_Category" to 1 for "Coarse" and 0 for "Fine"
df$Concrete_Category <- ifelse(df$Concrete_Category == "Coarse", 1, 0)
#Numerical variables are stored in num_var
num_var <- c("Cement", "Blast_Furnace_Slag", "Fly_Ash", "Water", "Superplasticizer", "Coarse_Aggregate", "Fine_Aggregate", "Age","Cement_Water_Ratio")

# Load Gridextra and create histograms,scatter-plots and box-plots for numerical variables.
plot_sp <- lapply(num_var, function(var) {
  ggplot(df, aes(x = .data[[var]], y = .data[["Concrete_compressive_strength"]])) +
    geom_point(alpha = 0.5,color='blue') +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    theme_minimal() +
    labs(title = paste(var, "vs Strength"), x = var, y = "Strength")
})
grid.arrange(grobs = plot_sp, ncol = 2)
#Plotting Histograms to check for distribution.
plot_hist <- lapply(num_var, function(var) {
  ggplot(df, aes(x = .data[[var]])) +
    geom_histogram(bins = 30, fill = "grey", color = "black") +
    theme_minimal() +
    labs(title = paste("Histogram of", var), x = var, y = "Frequency")
})
grid.arrange(grobs = plot_hist, ncol = 2)

#Concrete Category split by fly_ash
ggplot(df, aes(x = Concrete_Category, y = Concrete_compressive_strength, fill = Contains_Fly_Ash)) +
  stat_summary(geom = "bar", fun = "mean", position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = "mean_se", position = position_dodge(0.9), width = 0.25) +
  labs(title = "Concrete Strength by Category and Fly Ash",
       x = "Category", y = "Avg. Compressive Strength") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")

#Plot Boxplot to check for outliers
plot_box <- lapply(num_var, function(var) {
  ggplot(df, aes(y = .data[[var]])) +
    geom_boxplot(fill = "lightblue", color = "black") +
    theme_minimal() +
    labs(title = paste("Boxplot of", var), y = var, x = "") +
    coord_flip()  # This flips the boxplot to horizontal orientation
})
grid.arrange(grobs = plot_box, ncol = 2)
#To check if our numeric columns are normally distributed
shapiro_results <- lapply(df[num_var], shapiro.test)
shapiro_results
#We found data isn't normally distributed, now we see specific skewness values in each column.
for (col in num_var) {
  cat("Skewness of", col, ":", skewness(df[[col]], na.rm = TRUE), "\n")
}
# Assuming df is your data frame and the columns to transform are specified
log_transform <- c("Cement", "Blast_Furnace_Slag",  "Age","Cement_Water_Ratio")
sqrt_transform<-c("Fly_Ash", "Superplasticizer", "Concrete_compressive_strength")
# Apply log transformation with a constant added to avoid log(0)
df[log_transform] <- lapply(df[log_transform], function(x) log(x + 1))
df[sqrt_transform] <- lapply(df[sqrt_transform], function(x) sqrt(x))
# Loop to check skewness for each column in vars
for (col in num_var) {
  cat("Skewness of", col, ":", skewness(df[[col]], na.rm = TRUE), "\n")
}
#Plotting density plot to know the distribution after transformation
par(mfrow=c(3,3))  # Set up a 3x3 plot grid
for(var in num_var) {
  plot(density(df[[var]]), main=var, xlab="")
}
#Reset the plotting dimensions
par(mfrow=c(1,1))
#Encoding to include it in correlation Plot
df$Contains_Fly_Ash <- as.numeric(df$Contains_Fly_Ash)
# Compute the correlation matrix using spearman method as data isn't normally distributed.
correlation_matrix <- cor(df,method="spearman")
corrplot(correlation_matrix,method = "number",number.cex = 0.7,tl.cex = 0.6, addCoef.col = "black",diag = TRUE)
#Using random Forest Model for feature selection
set.seed(123)
rf_model <- randomForest(Concrete_compressive_strength ~ ., data = df, importance = TRUE)
# Get and plot variable importance
importance <- importance(rf_model)
varImpPlot(rf_model, main = "Variable Importance for Concrete Strength")
# Print variable importance scores
print(importance[order(-importance[,"%IncMSE"]),])
#Multiple linear regression model as the baseline model
mlr<-lm(Concrete_compressive_strength~Cement_Water_Ratio+Age+Blast_Furnace_Slag+Superplasticizer, data = df)
summary(mlr)
#To display columns based on feature selection
data.frame(colnames(df))
#Now let's plot to see the relationship
pairs(df[,c(11,8,2,12,5)], lower.panel = NULL, pch = 19,cex = 0.2)
plot(mlr,1)
plot(mlr,2)
#First Non-Parametric regression: GAM model
gam_model <- gam(Concrete_compressive_strength ~ Age + Cement_Water_Ratio + s(Blast_Furnace_Slag) + s(Superplasticizer), data = df)
gam_predicted_values <- predict(gam_model, newdata = df)
gam_residuals <- df$Concrete_compressive_strength - gam_predicted_values
# Create a residuals plot for Homoscedasticity of residuals
plot(gam_predicted_values, gam_residuals, xlab = "Fitted Values",ylab = "Residuals",main = "Residuals Plot for GAM Model")
abline(h = 0, col = "red")
# Summary of GAM model
summary(gam_model)
#GAM Metrics
gam_model_metrics <- postResample(pred = gam_predicted_values, obs = df$Concrete_compressive_strength)
print(gam_model_metrics)
#Prediction Vs Actual Plot
plot_data <- data.frame(
  Actual = df$Concrete_compressive_strength,
  Predicted = gam_predicted_values
)
ggplot(plot_data, aes(x = df$Concrete_compressive_strength, y = gam_predicted_values)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter points
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual Vs Predicted Plot: GAM Model", x = "Actual Concrete Compressive Strength",y = "Predicted Concrete Compressive Strength") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# Second Non-Parametric Model: Random Forest
set.seed(123)  # For reproducibility
rf_model <- randomForest(Concrete_compressive_strength ~ Cement_Water_Ratio+Age+Blast_Furnace_Slag+Superplasticizer, data = df,ntree=100)
rf_predicted_values <- predict(rf_model, newdata = df)
rf_residuals<-df$Concrete_compressive_strength - rf_predicted_values
# Create a residuals plot for Homoscedasticity of residuals
plot(rf_predicted_values, rf_residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals Plot for RF Model")
abline(h = 0, col = "red")
#Prediction Vs Actual Plot
#Prediction Vs Actual Plot
plot_data2 <- data.frame(
  Actual = df$Concrete_compressive_strength,
  Predicted = rf_predicted_values
)
ggplot(plot_data2, aes(x = df$Concrete_compressive_strength, y = rf_predicted_values)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter points
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual Vs Predicted Plot: Random Forest", x = "Actual Concrete Compressive Strength",y = "Predicted Concrete Compressive Strength") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#Analyzing Metric Score
rf_model_metrics <- postResample(pred = rf_predicted_values, obs = df$Concrete_compressive_strength)
print(rf_model_metrics)
#Perform First Hypothesis test using Wilcoxon rank-sum test.
mann_whitney_result <- wilcox.test(Concrete_compressive_strength ~ Contains_Fly_Ash,data = df, exact = FALSE)
# Print the results
print(mann_whitney_result)
# Perform Second hypothesis test using Kruskal-Wallis
kruskal_test_result <- kruskal.test(Concrete_compressive_strength ~ Concrete_Category, data = df)
# Print the results
print(kruskal_test_result)
# Perform third Hypothesis test using Kendall correlation test
cor_result <- cor.test(df$Fine_Aggregate, df$Concrete_compressive_strength, method = "kendall")
# Print the results
print(cor_result)
# Create Categories for Superplasticizer and Fly_Ash based on bimodal distribution.
df <- df %>%
  mutate(Category = case_when(
    Superplasticizer == 0 & Fly_Ash == 0 ~ "Zero",
    TRUE ~ "Not Zero"
  ))
# Print the distribution of categories.
print(table(df$Category))
#Perform Wilcoxon Rank-Sum Test
wilcoxon_test_result <- wilcox.test(Concrete_compressive_strength ~ Category, data = df)
#Print the results of the Wilcoxon Rank-Sum Test
print(wilcoxon_test_result)
