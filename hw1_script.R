library(readxl)
library(dplyr)
library(ggplot2)


### CHAPTER 2 EXERCISES

## QUESTION 3a

infant_birth_weight = read_excel('/Users/maddiethall/R Repos/anthro690/Infant Birth Weight (1).xlsx')

infant_birth_weight = infant_birth_weight %>%
  rename(infant_weight = `Child's Birth Weight (kg)`,
         maternal_weight = `Maternal Weight Gain (kg)`,
         systolic_bp = `Maternal Systolic Blood Pressure`,
         diastolic_bp = `Maternal Diastolic Blood Pressure`,
         smoking = `Maternal Smoking`)

desc_stats = infant_birth_weight %>%
  summarise(
    across(
      c(infant_weight, maternal_weight, systolic_bp, diastolic_bp),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        median = ~median(.x, na.rm = TRUE),
        mode = ~{
          ux = unique(.x)
          ux[which.max(tabulate(match(.x, ux)))]
        },
        range = ~diff(range(.x, na.rm = TRUE)),
        variance = ~var(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE)
      )))
View(desc_stats)   


## QUESTION 3b

by_smoking = infant_birth_weight %>%
  group_by(smoking) %>%
  summarise(
    across(
      c(infant_weight, maternal_weight, systolic_bp, diastolic_bp),
                   list(
                     mean = ~mean(.x, na.rm = TRUE),
                     median = ~median(.x, na.rm = TRUE),
                     mode = ~{
                       ux = unique(.x)
                       ux[which.max(tabulate(match(.x, ux)))]
                     },
                     range = ~diff(range(.x, na.rm = TRUE)),
                     variance = ~var(.x, na.rm = TRUE),
                     sd = ~sd(.x, na.rm = TRUE)
                   )))
View(by_smoking) 


## QUESTION 3c

barplot(table(infant_birth_weight$smoking),
        main = "Frequency of Maternal Smoking",
        xlab = "Smoking Status",
        ylab = "Number of Mothers",
        col = c("lightblue", "salmon"))

## QUESTION 3c

par(mfrow=c(1,2))
hist(infant_birth_weight$systolic_bp,
     main = "Histogram of Systolic BP",
     xlab = "Systolic Blood Pressure")
hist(infant_birth_weight$diastolic_bp,
     main = "Histogram of Diastolic BP",
     xlab = "Diastolic Blood Pressure")

ggplot(infant_birth_weight, aes(x = systolic_bp)) +
  geom_histogram(bins = 8, fill = "steelblue", color = "white") +
  facet_wrap(~ smoking) +
  labs(
    title = "Systolic Blood Pressure by Smoking Status",
    x = "Systolic Blood Pressure",
    y = "Frequency"
  ) +
  theme_minimal()

ggplot(infant_birth_weight, aes(x = diastolic_bp)) +
  geom_histogram(bins = 8, fill = "steelblue", color = "white") +
  facet_wrap(~ smoking) +
  labs(
    title = "Diastolic Blood Pressure by Smoking Status",
    x = "Diastolic Blood Pressure",
    y = "Frequency"
  ) +
  theme_minimal()
