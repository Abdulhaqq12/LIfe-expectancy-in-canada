library(tidyverse)
library(tidyr)
library(ggplot2)
library(tibble)


# Import the CSV file as tibble
LE<- read_csv("C:/Users/user/Downloads/life expectancy complete.csv") %>%
  as_tibble()
LE

#First objective 
#fIlter for only canada only 
Canada_LE <- LE %>%
  filter(Province == "Canada") %>% 
  select(Year, Gender, `Life Excpectancy`) %>% 
  #reshaping the data 
  pivot_wider(names_from = Gender, values_from = `Life Excpectancy`) %>%
  #Renaming the column 
  rename(YEAR = Year, MALE = Male, FEMALE = Female) 

# View the  table
print(Canada_LE)
# Reshape the Canada_LE data for visualization
Canada_LE_long <- Canada_LE %>%
  pivot_longer(cols = c(MALE, FEMALE), names_to = "Gender", values_to = "Life_Expectancy")

# Create the line graph
ggplot(Canada_LE_long, aes(x = YEAR, y = Life_Expectancy, color = Gender, group = Gender)) +
  geom_line(linewidth = 1) +  
  geom_point(size = 3) +  
  labs(
    title = "Life Expectancy in Canada Over the Years",
    x = "Year",
    y = "Life Expectancy",
    color = "Gender"  
  ) +
  scale_color_manual(values = c("blue", "red")) +  # Customize colors for Male and Female
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






#Second objective  

# Filter for the year 2015 and 2017, excluding "Canada"
filtered_data <- LE %>%
  filter(Year == "2015 to 2017" & Province != "Canada")

# Reshape the data to  get "Male" and "Female" as separate columns
life_expectancy_table <- filtered_data %>%
  select(Province, Gender, `Life Excpectancy`) %>%
  pivot_wider(names_from = Gender, values_from = `Life Excpectancy`)

# Display the resulting table
print("Life Expectancy Table by Province and Gender (2015 to 2017):")
print(life_expectancy_table)

#Plotting the graph 
ggplot(filtered_data, aes(x = Province, y = `Life Excpectancy`, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +  
  coord_flip() +  # Flip the chart to make it horizontal
  geom_text(aes(label = round(`Life Excpectancy`, 1)),
            position = position_dodge(width = 0.9),  
            hjust = 1.5,
            color = "black") +
  theme_minimal() +
  labs(
    title = "Life Expectancy for Each Province (2015 to 2017)",
    x = "Life Expectancy", 
    y = "Province", 
    fill = "Gender"
  ) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1), 
        panel.spacing = unit(1, "lines"))  






















