# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  shiny, ggplot2, plotly, dplyr, shinythemes, gganimate, gifski, writexl, here, tidyverse
)

# Load and process dataset
data <- read_csv(here::here("my data.csv"))

# Calculate and print the average age for reference
print(data %>% summarise(Average_age = mean(Age, na.rm = TRUE)))

# Add Age_Group column
data <- data %>%
  mutate(Age_Group = case_when(
    Age < 20 ~ "Under 20",
    Age >= 20 & Age < 30 ~ "20-29",
    Age >= 30 & Age < 40 ~ "30-39",
    Age >= 40 & Age < 50 ~ "40-49",
    TRUE ~ "50 and above"
  ))

# Rename columns for easier reference
data <- data %>% rename(
  Gender = `2. Gender`,
  Relationship_Status = `3. Relationship Status`,
  Occupation_Status = `4. Occupation Status`,
  Social_Media_Hours = `8. What is the average time you spend on social media every day?`,
  Sleep_Issues = `20. On a scale of 1 to 5, how often do you face issues regarding sleep?`,
  Depression_Frequency = `18. How often do you feel depressed or down?`
)

# Filter relevant columns
filtered_data <- data %>%
  select(Age, Age_Group, Gender, Occupation_Status, Sleep_Issues, Depression_Frequency, Social_Media_Hours) %>%
  filter(complete.cases(.))

# Save filtered data
write_csv(filtered_data, here::here("filtered_data.csv"))
write_xlsx(filtered_data, here::here("filtered_data.xlsx"))

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Social Media & Mental Health Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("ageRange", "Select Age Range:", 
                  min = ifelse(is.na(min(filtered_data$Age, na.rm = TRUE)), 0, min(filtered_data$Age, na.rm = TRUE)),
                  max = ifelse(is.na(max(filtered_data$Age, na.rm = TRUE)), 100, max(filtered_data$Age, na.rm = TRUE)),
                  value = c(
                    ifelse(is.na(min(filtered_data$Age, na.rm = TRUE)), 0, min(filtered_data$Age, na.rm = TRUE)), 
                    ifelse(is.na(max(filtered_data$Age, na.rm = TRUE)), 100, max(filtered_data$Age, na.rm = TRUE))
                  ),
                  step = 1),
      
      radioButtons("genderFilter", "Select Gender:", 
                   choices = c("All", "Male", "Female"), 
                   selected = "All"),
      
      selectInput("occupationFilter", "Select Occupation:",
                  choices = c("All", unique(filtered_data$Occupation_Status)),
                  selected = "All")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Scatter Plot", plotlyOutput("scatterPlot")),
        tabPanel("Boxplot", plotlyOutput("boxPlot")),
        tabPanel("Animated Scatter", imageOutput("animatedScatter"))
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Reactive filtered data
  filtered_data_reactive <- reactive({
    df <- filtered_data %>% filter(Age >= input$ageRange[1] & Age <= input$ageRange[2])
    
    if (input$genderFilter != "All") {
      df <- df %>% filter(Gender == input$genderFilter)
    }
    
    if (input$occupationFilter != "All") {
      df <- df %>% filter(Occupation_Status == input$occupationFilter)
    }
    
    return(df)
  })
  
  # Scatter Plot: Age vs. Social Media Usage
  output$scatterPlot <- renderPlotly({
    p <- ggplot(filtered_data_reactive(), aes(x = Age, y = Social_Media_Hours, 
                                              color = Sleep_Issues, size = Depression_Frequency)) +
      geom_point(alpha = 0.7) +
      scale_color_gradient(low = "blue", high = "red") +
      geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
      labs(title = "Age vs. Social Media Usage",
           subtitle = "Color: Sleep Issues | Size: Depression Frequency",
           x = "Age", y = "Social Media Hours") +
      theme_minimal()
    
    ggplotly(p) %>% animation_opts(1000, easing = "elastic")
  })
  
  # Boxplot: Depression Frequency across Occupation Status
  output$boxPlot <- renderPlotly({
    p <- ggplot(filtered_data_reactive(), aes(x = Occupation_Status, y = Depression_Frequency, fill = Occupation_Status)) +
      geom_boxplot() +
      labs(title = "Depression Frequency by Occupation",
           x = "Occupation Status", y = "Depression Frequency") +
      theme_minimal() +
      scale_fill_brewer(palette = "Dark2")
    
    ggplotly(p)
  })
  
  # Animated Scatter Plot: Age vs. Social Media Usage
  output$animatedScatter <- renderImage({
    tempFile <- tempfile(fileext = ".gif")
    
    anim_plot <- ggplot(filtered_data_reactive(), aes(x = Age, y = Social_Media_Hours, color = Gender)) +
      geom_point(alpha = 0.7, size = 3) +
      labs(title = "Age vs. Social Media Usage (Animated)", x = "Age", y = "Social Media Hours") +
      theme_minimal() +
      transition_states(Age, transition_length = 2, state_length = 1) +
      ease_aes("cubic-in-out")
    
    anim_save(tempFile, animate(anim_plot, renderer = gifski_renderer(), height = 400, width = 600))
    
    list(src = tempFile, contentType = "image/gif")
  }, deleteFile = TRUE)
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
