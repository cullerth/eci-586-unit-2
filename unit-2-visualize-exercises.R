# INTRO ################

#' For our Unit 2 exercises, we will be working with 
#' the data_to_explore data frame we created in Unit 1
#' consisting of our merged gradebook, log data, and survey data.
#' 
#' In the space below, load the necessary package(s) to
#' import, wrangle and explore the data-to-explore.csv file 
#' located in the data folder.
#' 
#' Import to your R environment and save your data 
#' to a new object named data_to_viz.  

library(tidyverse)
library(dplyr)

data_to_viz <- read_csv("data/data-to-explore.csv")


# VARIATION ####################

## Bar Charts ########

### Exercise 1 #################

#' In the space below, create a basic visualization
#' that examines a categorical variable of interest. 
#' 
#' Add an appropriate title to your chart.
#' 
#' Add a caption that poses a question educators may have 
#' about this data that your visualization could help answer.
#' 
#' The first exercise has been done for you. 

ggplot(data_to_viz) +
  geom_bar(aes(x = subject)) +
  labs(title = "Number of Student Enrollments per Subject",
       caption = "Which online courses have had the largest enrollment?")

ggplot(data_to_viz) +
  geom_bar(aes(x = gender)) +
  labs(title = "Gender representation",
       caption = "What is the proportion of female to male students in the dataset?")


## Histograms & Similar Geoms #####

### Exercise 2 ####################

#' In the space below, create a basic visualization
#' that examines a continuous variable of interest.
#' 
#' This could be a histogram, frequency polygon, 
#' density curve or even a dot plot.   
#' 
#' Add an appropriate title to your chart.
#' 
#' Add a caption that poses a question educators may have 
#' about this data that your visualization could help answer.

ggplot(data = data_to_viz) +
  geom_histogram(mapping = aes(x = proportion_earned), bins = 10) + 
  labs(title = "Final Grades",
       caption = "What does the distribution of studen'ts final grades look like?")


# COVARIATION ####################

## Boxplots & Similar Geoms #####

#### Exercise 3 ##################

#' In the space below, create a basic visualization
#' that examines the relationship between a categorical 
#' variable and a continuous variable.
#' 
#' This could be a boxplot or violin plot.   
#' 
#' Add an appropriate title to your chart.
#' 
#' Add a caption that poses a question educators may have 
#' about this data that your visualization could help answer.

ggplot(data = data_to_viz) +
  geom_boxplot(mapping = aes(x = subject, y = proportion_earned)) + 
  labs(title = "Final Grades by Subject",
       caption = "How do students' final grades compare across subjects?")


## Counts #####

### Exercise 4 ##################

#' In the space below, create a basic visualization
#' that examines the relationship between two categorical variables.
#' 
#' This could be a count plot or heatmap.   
#' 
#' Add an appropriate title to your chart.
#' 
#' Add a caption that poses a question educators may have 
#' about this data that your visualization could help answer.

ggplot(data = data_to_viz) +
  geom_count(mapping = aes(x = subject, y = enrollment_reason)) + 
  labs(title = "Enrollent Reason by Subject",
       caption = "What are some of the more common reaons students enrolled in particular courses?")

## Scatterplots & Line Plots #####

### Exercise 5 ##################

#' In the space below, create a basic visualization
#' that examines the relationship between two continuous variables.
#' 
#' This could be a scatterplot with layers, 
#' a log-log or line plot, or one using coord functions.   
#' 
#' Add an appropriate title to your chart.
#' 
#' Add a caption that poses a question educators may have 
#' about this data that your visualization could help answer.

data_to_viz %>%
  group_by(subject) %>%
  summarise(mean_grade = mean(proportion_earned, na.rm = TRUE), mean_time_hrs = mean(time_spent/60, na.rm = TRUE)) %>%
  ggplot() +
    geom_point(mapping = aes(x = mean_grade, y = mean_time_hrs, color = subject)) +
    geom_smooth(mapping = aes(x = mean_grade, y = mean_time_hrs), method = lm) +
    labs(title = "Average Final Grades by Average Time Spent",
         caption = "Is there a relationship between the average final grades earned in a course and the average time spent on that course?")

## Big Data & Customizing Plots ####

### Exercise 6 ##################

#' In the space below, select one of your graphs
#' from above or create an entirely new viz. 
#' 
#' Add an appropriate title to your chart.
#' 
#' Add or adjust any aesthetics to improve the 
#' readability of visual appeal of your viz. 
#' 
#' Zoom or clip your x and/or y axis if needed to
#' better see key data points. 
#' 
#' Add or adjust any labels such as graph titles, 
#' subtitles, captions, axis titles, or annotations
#' that you think will help others interpret your chart. 
#' 
#' Add a theme you find attractive and adjust if needed. 
#' 
#' Use a color scale if appropriate to modify the 
#' default colors used by ggplot. 
#' 
#' Adjust or remove your legend as appropriate. 

sum(is.na(data_to_viz$gender))

female <- data_to_viz %>%
  group_by(subject) %>%
  filter(gender == "F") %>%
  summarise(final_grade = mean(proportion_earned * 100, na.rm = TRUE))

male <- data_to_viz %>%
  group_by(subject) %>%
  filter(gender == "M") %>%
  summarise(final_grade = mean(proportion_earned * 100, na.rm = TRUE)) 

combined_final_grades <- merge(female, male, by = 'subject') %>%
  rename(female_final_grades = 'final_grade.x', male_final_grades = 'final_grade.y')

ggplot(combined_final_grades, aes(x = subject)) +
  geom_point(aes(y = female_final_grades, color = 'Female')) +
  geom_point(aes(y = male_final_grades, color = 'Male')) +
  # geom_smooth(mapping = aes(x = mean_grade, y = mean_time_hrs), method = lm) +
  labs(title = "Average Final Grades by Gender & Subject",
       x = 'Final Grades',
       y = "Subject",
       color = "Gender",
       caption = "Is there a gender difference in final grades across subjects?")


#' Once you are satisfied with your data visualization, 
#' create a new R Markdown file that you will use for 
#' your first "data product." 
#' 
#' Use the data-product-example.Rmd located in the 
#' files pane as a template for how your final 
#' markdown file should look. 
#' 
#' The first code chunk named "setup" runs the code
#' necessary for knitting your markdown file to an 
#' HTML webpage. Note that is includes the argument
#' include=FALSE which tells R not to include any code
#' or output in your final document. 
#' 
#' Repurpose one of the other code chunks to recreate 
#' the data visualization you made above. 
#' 
#' Note that you will need to add all the code necessary 
#' to make your visualization including loading required 
#' packages and importing your data. 
#' 
#' Also note that I included some additional arguments
#' to my code chunk to prevent messages add warnings 
#' from being displayed in my knitted document. 
#' 
#' In the YAML section at the top, be sure to change
#' the title and include your name as author. 
#' Also include the "cold_folding: hide" option 
#' as illustrated in the data-product-example.Rmd file. 
#' This will allow others to view your code if desired
#' to see you you created your data viz. 

