# Load required libraries
library(ggplot2)
library(plotly)
library(dplyr)
library(ggthemes)

# Load the filtered dataset
data <- read.csv("filtered_data.csv")

# ✅ 1. Scatter Plot: Social Media Usage vs Age (Color = Sleep Issues, Size = Depression)
scatter_plot <- ggplot(data, aes(x = Age, y = Social_Media_Hours, 
                                 color = Sleep_Issues, size = Depression_Frequency)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
  labs(title = "Social Media Usage vs. Age",
       subtitle = "Color: Sleep Issues | Size: Depression Frequency",
       x = "Age", y = "Social Media Hours") +
  theme_minimal()

ggplotly(scatter_plot)  # Make interactive

# ✅ 2. Bar Chart: Social Media Usage for Students vs. Professionals
occupation_plot <- ggplot(data, aes(x = Occupation_Status, y = Social_Media_Hours, fill = Occupation_Status)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Avg. Social Media Usage by Occupation",
       x = "Occupation Status", y = "Avg. Social Media Hours") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

ggplotly(occupation_plot)

# ✅ 3. Violin Plot: Gender-wise Social Media Usage
violin_plot <- ggplot(data, aes(x = Gender, y = Social_Media_Hours, fill = Gender)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  labs(title = "Social Media Usage Distribution by Gender",
       x = "Gender", y = "Social Media Hours") +
  theme_minimal() +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink"))

ggplotly(violin_plot)

# ✅ 4. Boxplot: Depression Frequency across Occupation Status
boxplot <- ggplot(data, aes(x = Occupation_Status, y = Depression_Frequency, fill = Occupation_Status)) +
  geom_boxplot() +
  labs(title = "Depression Frequency by Occupation",
       x = "Occupation Status", y = "Depression Frequency") +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2")

ggplotly(boxplot)

# ✅ 5. Pie Chart: Gender Distribution
gender_data <- data %>%
  count(Gender) %>%
  mutate(percentage = (n / sum(n)) * 100)

pie_chart <- ggplot(gender_data, aes(x = "", y = percentage, fill = Gender)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  labs(title = "Gender Distribution in Dataset") +
  theme_void() +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink"))

ggplotly(pie_chart)
