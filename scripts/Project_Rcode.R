#---------------------------------Loading the libraries---------------------------------#

#Library: Manipulation
library(readr)
library(dplyr)
library(lubridate)

#Library: Viz
library(ggplot2)
library(gridExtra)

#Library: other
library(beepr)

#---------------------------------Loading the datasets---------------------------------#
# Set the directory path (change this path to the folder of 10 csv files)
directory <- "Dataset"

# Get a list of all CSV file names in the directory
file_list <- list.files(path = directory, pattern = "*.csv", full.names = TRUE)

# Read all CSV files into a list of data frames
dataset_list <- lapply(file_list, function(file) {
  df <- read.csv(file)
  colnames(df) <- tolower(colnames(df))  # Normalize column names to lowercase
  return(df)
})

# Find all unique column names across the datasets
all_columns <- Reduce(union, lapply(dataset_list, colnames))

# Standardize datasets to have the same columns
standardized_datasets <- lapply(dataset_list, function(df) {
  df[, setdiff(all_columns, colnames(df))] <- NA  # Add missing columns as NA
  df[all_columns]  # Reorder columns to match the full set
})

# Combine the standardized datasets
combined_dataset <- bind_rows(standardized_datasets)
beep()

#---------------------------------Data Summary---------------------------------#
#Dataset Shape
dim(combined_dataset)

#Data Types
str(combined_dataset)
#glimpse(combined_data)

#Dataset column names
column_names <- colnames(combined_dataset)

#Initial 5Num summary
summary(combined_dataset)

#---------------------------------Extracting Date from REP_DATE ---------------------------------#

#combined_dataset$rep_date <- parse_date_time(combined_dataset$rep_date, orders = c("ymd HMS", "ymd"))
#beep()
#---------------------------------Cleaning: Column-wise---------------------------------#

#Checking for missing values 
missing_values <- colSums(is.na(combined_dataset))

# Combine the missing values and summary statistics into a data frame for easier view.
missing_summary_col <- data.frame(Variable = names(missing_values), Missing_Values = missing_values)
print(missing_summary_col)

# many columns with missing values, check rows before dropping columns

#---------------------------------Cleaning: Row-wise---------------------------------#

# Check rows with a significant number of missing values
missing_counts <- rowSums(is.na(combined_dataset))

# Create a summary of rows with missing values
missing_summary_row <- data.frame(Row_Index = 1:nrow(combined_dataset), Missing_Count = missing_counts)

# Filter for rows with more than 8 missing values
significant_missing_row <- missing_summary_row[missing_summary_row$Missing_Count > 8, ]

# Display the summary of rows with significant missing values
print(head(significant_missing_row, 10))

# Remove rows with more than 8 missing values
cleaned_row_df <- combined_dataset[rowSums(is.na(combined_dataset)) <= 8, ]

# Check the dimensions of the cleaned dataset
cleaned_row_dim <- dim(cleaned_row_df)

# Display the dimensions of the cleaned dataset
print(cleaned_row_dim)
print(dim(combined_dataset)) #compare to original: 9,608,947 to 7318270      

#---------------------------------Cleaning: Dropping columns ---------------------------------#
#dropping columns with greater than 20-30% of missing values

columns_to_drop <- c('uid','age','estarea','polyid','cfactor','cfl','tfc0','sfl','ecozone','sfc0','cbh')

cleaned_df <- cleaned_row_df[ , !names(cleaned_row_df) %in% columns_to_drop]

missing_values_summary_after_removal <- colSums(is.na(cleaned_df))

print(missing_values_summary_after_removal)

print(dim(cleaned_row_df))
print(dim(cleaned_df)) #From 39 columns to 28 columns

#---------------------------------Cleaning: Initial Data Type Checking---------------------------------#
# Check the structure of the cleaned dataset to evaluate data types
cleaned_structure <- str(cleaned_df)

#---------------------------------Cleaning: Column: elev ---------------------------------#
# Check the summary of ELEV col
elev_distribution <- summary(cleaned_df$elev)
print(elev_distribution)

# Create a box plot
boxplot(cleaned_df$elev,
        main = "Boxplot: Elevation Distribution",
        ylab = "Elevation",
        horizontal = TRUE,
        sub = 'Before cleaning')
#Interpretation: Too many outliers - Impute by median (Removing will distort overall distribution)
#Action: COL: ELEV - impute missing rows by median

# Calculate the median of the ELEV column
median_elev <- median(cleaned_df$elev, na.rm = TRUE)

# Fill missing values in the ELEV column with the median
cleaned_df$elev[is.na(cleaned_df$elev)] <- median_elev

# Check the summary of the ELEV column after filling missing values
filled_elev_summary <- summary(cleaned_df$elev)

# Display the summary of the ELEV column after filling missing values
print(filled_elev_summary)

# Check if missing values in ELEV are now properly filled
missing_elev_count <- sum(is.na(cleaned_df$elev)) # Correct way to count missing values in a single column
print(missing_elev_count)  # Should be 0

#---------------------------------Cleaning: Check how to impute cols: bfc, cfb, greenup ---------------------------------#
#visualize outliers for bfc
summary(cleaned_df$bfc)
boxplot(cleaned_df$bfc,
        main = "Boxplot: bfc",
        ylab = "bfc",
        horizontal = TRUE)
#Action: impute col: bfc by median

#visualize outliers for cfb
summary(cleaned_df$cfb)
boxplot(cleaned_df$cfb,
        main = "Boxplot: cfb",
        ylab = "cfb",
        horizontal = TRUE)
#Action: impute col: cfb by median

#visualize outliers for greenup
summary(cleaned_df$greenup)
boxplot(cleaned_df$greenup,
        main = "Boxplot: greenup",
        ylab = "greenup",
        horizontal = TRUE)
#Action: impute col: greenup by median

#---------------------------------Cleaning: Impute cols: bfc, cfb, greenup ---------------------------------#
# Calculate median for bfc, cfb, and greenup columns
median_bfc <- median(cleaned_df$bfc, na.rm = TRUE)
median_cfb <- median(cleaned_df$cfb, na.rm = TRUE)
median_greenup <- median(cleaned_df$greenup, na.rm = TRUE)

# Impute missing values in bfc, cfb, and greenup columns with their respective medians
cleaned_df$bfc[is.na(cleaned_df$bfc)] <- median_bfc
cleaned_df$cfb[is.na(cleaned_df$cfb)] <- median_cfb
cleaned_df$greenup[is.na(cleaned_df$greenup)] <- median_greenup

# Check the summary of missing values after imputing PCP, PCURING, CFACTOR, and GREENUP
missing_values_summary_after_final_imputation <- colSums(is.na(cleaned_df))


# Display the summary of missing values after final imputation
print(missing_values_summary_after_final_imputation)

#---------------------------------Cleaning: Categorical Variables ---------------------------------#
# Check unique values in categorical columns
categorical_columns <- c('source', 'sensor', 'satellite', 'agency', 'fuel')
unique_values <- lapply(cleaned_df[categorical_columns], unique)

# Display the unique values for each categorical column
print(unique_values)

# Findings: agency has "-", fuel has "-99999" and "", satellite has "" 

#---------------------------------Cleaning: Columns: satellite, agency, fuel ---------------------------------#
# Replace weird entries and blanks in the AGENCY and FUEL columns
cleaned_df$agency[cleaned_df$agency == "-"] <- NA
cleaned_df$fuel[cleaned_df$fuel == "-99999"] <- NA
cleaned_df$fuel[cleaned_df$fuel == ""] <- NA
cleaned_df$satellite[cleaned_df$satellite == ""] <- NA

# Check the unique values again after cleaning
unique_values_cleaned <- lapply(cleaned_df[categorical_columns], unique)

# Display the unique values for each categorical column after cleaning
print(unique_values_cleaned)


#---------------------------------Cleaning: Impute satellite, agency, fuel with "unknown" to preserve data ---------------------------------#
cleaned_df[categorical_columns] <- lapply(cleaned_df[categorical_columns], function(column) {
  ifelse(is.na(column), "unknown", column)
})

# duplicates <- cleaned_df[duplicated(cleaned_df), ]
# head(duplicates)

#export as new csv
write.csv(combined_dataset, "cleaned_df.csv", row.names = FALSE)

# Last check of the summary of missing values
missing_values_clean <- colSums(is.na(cleaned_df))

# Display the summary of missing values 
print(missing_values_clean)

#-------Pat Notes---------#
#can't figure out how to transform rep_date to date without losing data (i'm getting NAs)
#adjustments can still be done on the ff:
# rows cleaning with > 8 missing values
# column cleaning with 20 - 30 % missing values
# hard to remove duplicates -- too long process
# export as new csv file for EDA 



# Create a bar graph using ggplot2
fuel_summary <- cleaned_df %>%
  count(fuel) %>%
  arrange(desc(n))
ggplot(fuel_summary, aes(x = fuel, y = count)) +
  geom_bar(stat = "identity",fill = "steelblue") +
  labs(title = "Fuel Type Distribution", x = "Fuel Type", y = "Count") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Example: Filter the dataset 
cleaned_df_filtered <- cleaned_df %>%
  filter(fuel %in% c("C1","C3","C4","D1","o1","M1","M2","M4","S1", "S2"))  # Keep only these fuel types

# Create the boxplot for the filtered data
ggplot(cleaned_df_filtered, aes(x = fuel, y = hfi, fill = fuel)) +
  geom_boxplot() +
  labs(title = "Boxplot of HFI by Fuel Type",
       x = "Fuel Type",
       y = "HFI") +
  theme_minimal() +
  theme(legend.position = "none")
