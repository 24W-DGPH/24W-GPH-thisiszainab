# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  # Core data science packages (includes dplyr)
  writexl,    # For writing Excel files
  here        # For managing file paths
)

# Load dataset (Ensure "my_data.csv" is in the correct folder)
data <- read_csv(here::here("my data.csv"))

# Calculate and print the average age for reference
average_age <- data %>% summarise(Average_age = mean(Age, na.rm = TRUE))
print(average_age)

# Add Age_Group column
data <- data %>%
  mutate(Age_Group = case_when(
    Age < 20 ~ "Under 20",
    Age >= 20 & Age < 30 ~ "20-29",
    Age >= 30 & Age < 40 ~ "30-39",
    Age >= 40 & Age < 50 ~ "40-49",
    TRUE ~ "50 and above"
  ))

# Rename columns for easier reference (Use backticks for column names with spaces or numbers)
data <- data %>% 
  rename(
    Gender = `2. Gender`,
    Relationship_Status = `3. Relationship Status`,
    Occupation_Status = `4. Occupation Status`,
    Organization_Affiliation = `5. What type of organizations are you affiliated with?`,
    Uses_Social_Media = `6. Do you use social media?`,
    Social_Media_Platforms = `7. What social media platforms do you commonly use?`,
    Social_Media_Hours = `8. What is the average time you spend on social media every day?`,
    Social_Media_Distraction = `9. How often do you find yourself using Social media without a specific purpose?`,
    Social_Media_Busy_Distraction = `10. How often do you get distracted by Social media when you are busy doing something?`,
    Social_Media_Restlessness = `11. Do you feel restless if you haven't used Social media in a while?`,
    Easily_Distracted = `12. On a scale of 1 to 5, how easily distracted are you?`,
    Bothered_By_Worries = `13. On a scale of 1 to 5, how much are you bothered by worries?`,
    Difficulty_Concentrating = `14. Do you find it difficult to concentrate on things?`,
    Social_Comparison_Frequency = `15. On a scale of 1-5, how often do you compare yourself to other successful people through the use of social media?`,
    Feelings_About_Comparison = `16. Following the previous question, how do you feel about these comparisons, generally speaking?`,
    Validation_Seeking = `17. How often do you look to seek validation from features of social media?`,
    Depression_Frequency = `18. How often do you feel depressed or down?`,
    Interest_Fluctuation = `19. On a scale of 1 to 5, how frequently does your interest in daily activities fluctuate?`,
    Sleep_Issues = `20. On a scale of 1 to 5, how often do you face issues regarding sleep?`
  )

# Filter relevant columns and rows
# ✅ Fix: Add `Age_Group` to the selected columns
filtered_data <- data %>%
  select(Age, Age_Group, Gender, Occupation_Status, Sleep_Issues, 
         Depression_Frequency, Social_Media_Hours) %>%
  filter(!is.na(Age) & !is.na(Age_Group) & !is.na(Gender) & !is.na(Occupation_Status) & 
           !is.na(Sleep_Issues) & !is.na(Depression_Frequency) & !is.na(Social_Media_Hours))


# View the filtered data
head(filtered_data)
summary(filtered_data)

# Save the filtered data to a CSV file
write_csv(filtered_data, here::here("filtered_data.csv"))

# Save the filtered data to an Excel file
write_xlsx(filtered_data, here::here("filtered_data.xlsx"))

