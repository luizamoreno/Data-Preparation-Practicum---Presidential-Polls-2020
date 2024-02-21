
# Data Cleaning and Analysis: 2020 Presidential Polls
# Practicum Assignment: Luiza Moreno Ribeiro


# Load necessary libraries
library(readxl)
library(dplyr)

# Set the path to Excel file: Presidential Polls 2020
excel_file_path <- "/Users/luizamoreno/Desktop/presidential_polls_2020.xlsx"

dataframe1 <- read_excel("/Users/luizamoreno/Desktop/presidential_polls_2020.xlsx"
, sheet = 1)

dataframe2 <- read_excel("/Users/luizamoreno/Desktop/presidential_polls_2020.xlsx", sheet = 2)

# Print the structure of the dataframes
str(dataframe1)
str(dataframe2)

#There are several issues with the dataset. First, we must convert the "start date" to a numeric date format.
dataframe1 <- dataframe1 %>%
  mutate(startdate = as.Date(as.numeric(startdate), origin = "1899-12-30"))

# Convert 'weight' to numeric
dataframe1$weight <- as.numeric(dataframe1$weight)

# Convert 'startdate' to Date format
dataframe2$startdate <- as.Date(dataframe2$startdate)

# Convert 'weight' to numeric
dataframe2$weight <- as.numeric(dataframe2$weight)

str(dataframe1) 
str(dataframe2)
colnames(dataframe1)
colnames(dataframe2)


# Combine the two dataframes into one 
merged_dataframe <- merge(dataframe1, dataframe2, by = "state", all = TRUE)


# Check the structure and column names of the merged dataframe
str(merged_dataframe)
colnames(merged_dataframe)
summary(merged_dataframe)
colnames(merged_dataframe)

# Visualize the distribution of percentage votes
ggplot(merged_dataframe, aes(x = pct.x)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Percentage Votes (Candidate_name.x)",
       x = "Percentage Votes", y = "Frequency")
print(ggplot)
#This histogram provides insight into the distribution of percentage votes for the candidate X.
#The x-axis represents the percentage votes, while the y-axis shows the frequency. 
#Understanding the distribution is crucial for assessing the variability of voter preferences.
# It helps identify patterns, outliers, or skewed distributions, providing valuable information for further analysis


# Visualize the distribution and identify outliers using a boxplot for samples size.
ggplot(merged_dataframe, aes(y = samplesize.x)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot of Samplesize.x",
       x = "Variable", y = "Samplesize.x")
print(ggplot)

# This boxplot provides a visual representation of the distribution of samplesize.x.
# It helps in identifying potential outliers that may deviate significantly from the majority of the data.
# The boxplot shows that there are several outliers in the dataset.
# Adressing and analyzing outliers is crucial when cleaning the data 


#Looking at the outliers from sample size 
outliers <- boxplot(merged_dataframe$samplesize.x, plot = FALSE)$out
outlier_data <- merged_dataframe[merged_dataframe$samplesize.x %in% outliers, ]
summary(outlier_data)

# Remove rows with missing values
merged_dataframe_cleaned <- na.omit(merged_dataframe)

# Calculate the IQR for the 'samplesize.x' column
qnt <- quantile(merged_dataframe$samplesize.x, c(0.25, 0.75), na.rm = TRUE)
iqr <- qnt[2] - qnt[1]

# Define the lower and upper bounds for outliers
lower_bound <- qnt[1] - 1.5 * iqr
upper_bound <- qnt[2] + 1.5 * iqr

# Identify outliers
outliers <- which(merged_dataframe$samplesize.x < lower_bound | merged_dataframe$samplesize.x > upper_bound, arr.ind = TRUE)

# Print summary statistics of the outliers
summary(merged_dataframe[outliers, ])

# Remove outliers from the dataframe
merged_dataframe <- merged_dataframe[-outliers, ]

# Plot a histogram of 'samplesize.x' after removing outliers
ggplot(merged_dataframe, aes(x = samplesize.x)) +
  geom_histogram(binwidth = 500, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Sample Sizes (Excluding Outliers)",
       x = "Sample Size", y = "Frequency")

# Now we can see much more clearly the distribution of frequency vs sample size. 
# In this case, we used the Interquartile Range (IQR) method to identify and remove outliers.
# The IQR method helps maintain a clean dataset by removing extreme values that might distort the analysis
# Removing outliers ensures that the distribution of sample sizes is more representative.



# Now we can impute missing values using the mean in order to not compromise the dataset by just removing data
# Imputing missing values
merged_dataframe$samplesize.x[is.na(merged_dataframe$samplesize.x)] <- mean(merged_dataframe$samplesize.x, na.rm = TRUE)

# Visualize the distribution of the imputed 'samplesize.x'
ggplot(merged_dataframe, aes(x = samplesize.x)) +
  geom_histogram(binwidth = 500, fill = "purple", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Imputed Sample Sizes",
       x = "Sample Size", y = "Frequency")
#Now we have a more accurate histogram of the distribution of sample sizes vs frequency

# Box plot for samples ize in dataframe2
ggplot(dataframe2, aes(y = samplesize.y)) +
  geom_boxplot() +
  labs(title = "Box Plot of Sample Size in dataframe2")
print(ggplot)

# Description:
# This box plot visually represents the distribution of sample sizes in the second dataframe (dataframe2). 
# Sample sizes is crucial in statistical analysis, as it gives insights into the variability and spread 

# Check for spelling and formatting issues in column names
colnames(merged_dataframe) <- make.names(colnames(merged_dataframe))

# Check for negative values in numerical columns
negative_values <- sapply(merged_dataframe, function(x) any(x < 0, na.rm = TRUE))
colnames(merged_dataframe) <- make.names(colnames(merged_dataframe))





