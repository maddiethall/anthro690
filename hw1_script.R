library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

### CHAPTER 2 EXERCISES

## Question 2

ethnic_data = c(30, 22, 11)
ethnic_data = data.frame(ethnic_data)
ethnic_data = ethnic_data %>%
  mutate(names = c("Thai", "Chinese", "Laotian"))
mean(ethnic_data$ethnic_data)
sd(ethnic_data$ethnic_data)
var(ethnic_data$ethnic_data)
barplot(ethnic_data$ethnic_data,
        main = "Ethnic Group Frequencies",
        xlab = "Thai            Chinese          Laotian",
        ylab = "Frequency")

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

desc_stats_df <- infant_birth_weight %>%
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
      )
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("variable", "stat"),
    names_pattern = "^(.*)_(mean|median|mode|range|variance|sd)$",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  )

desc_stats_df

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

by_smoking_df <- infant_birth_weight %>%
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
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  # Convert from wide to long format for clean table
  pivot_longer(
    -smoking,
    names_to = c("variable", "stat"),
    names_pattern = "^(.*)_(mean|median|mode|range|variance|sd)$",
    values_to = "value"
  ) %>%
  # Spread stats into columns
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>%
  arrange(smoking, variable)


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


### QUESTION 4

## QUESTION 4a

school_snacks = read_excel('/Users/maddiethall/R Repos/anthro690/School Snacks (1).xls')

school_snacks = school_snacks %>%
  rename(height = `Height (cm)`,
         weight = `Weight (kg)`,
         activity = `Activity level (high=1/low=0)`,
         gender = `Gender`,
         snack_machine = `Snack Machine Present (yes=1/no=0)`)

desc_stats_snacks = school_snacks %>%
  summarise(
    across(
      c(height, weight),
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
      )
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("variable", "stat"),
    names_pattern = "^(.*)_(mean|median|mode|range|variance|sd)$",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  )

## QUESTION 4b

by_gender = school_snacks %>%
  group_by(gender) %>%
  summarise(
    across(
      c(height, weight),
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
View(by_gender) 

by_gender_df <- school_snacks %>%
  group_by(gender) %>%
  summarise(
    across(
      c(height, weight),
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
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  # Pivot longer so variables are rows
  pivot_longer(
    -gender,
    names_to = c("variable", "stat"),
    names_pattern = "^(.*)_(mean|median|mode|range|variance|sd)$",
    values_to = "value"
  ) %>%
  # Pivot wider so stats are columns
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>%
  arrange(gender, variable)

## QUESTION 4c

by_activity = school_snacks %>%
  group_by(activity) %>%
  summarise(
    across(
      c(height, weight),
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
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  # Pivot longer so variables are rows
  pivot_longer(
    -activity,
    names_to = c("variable", "stat"),
    names_pattern = "^(.*)_(mean|median|mode|range|variance|sd)$",
    values_to = "value"
  ) %>%
  # Pivot wider so stats are columns
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>%
  arrange(activity, variable)

## QUESTION 4d

by_snacks = school_snacks %>%
  group_by(snack_machine) %>%
  summarise(
    across(
      c(height, weight),
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
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  # Pivot longer so variables are rows
  pivot_longer(
    -snack_machine,
    names_to = c("variable", "stat"),
    names_pattern = "^(.*)_(mean|median|mode|range|variance|sd)$",
    values_to = "value"
  ) %>%
  # Pivot wider so stats are columns
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>%
  arrange(snack_machine, variable)

## QUESTION 4e

barplot(table(school_snacks$activity),
        main = "Frequency of Activity Level",
        xlab = "Activity Level",
        ylab = "Count",
        col = c("lightblue", "salmon"))

## QUESTION 4f

school_snacks$activity = factor(school_snacks$activity, labels = c("Low", "High"))


ggplot(school_snacks, aes(x = weight)) +
  geom_freqpoly(bins = 30, linewidth = 1) +
  labs(
    title = "Frequency Polygon of Weight",
    x = "Weight (kg)",
    y = "Frequency"
  ) +
  theme_minimal()

ggplot(school_snacks, aes(x = height)) +
  geom_freqpoly(bins = 30, linewidth = 1) +
  labs(
    title = "Frequency Polygon of Weight",
    x = "Height (cm)",
    y = "Frequency"
  ) +
  theme_minimal()



ggplot(school_snacks, aes(x = weight, group = activity, color = activity)) +
  geom_freqpoly(bins = 30, linewidth = 1) +
  labs(
    title = "Frequency Polygon of Weight by Activity Level",
    x = "Weight (kg)",
    y = "Frequency"
  ) +
  theme_minimal()

ggplot(school_snacks, aes(x = height, group = activity, color = activity)) +
  geom_freqpoly(bins = 30, linewidth = 1) +
  labs(
    title = "Frequency Polygon of Weight by Activity Level",
    x = "Height (cm)",
    y = "Frequency"
  ) +
  theme_minimal()


