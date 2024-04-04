
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r}
library(tidyverse)
library(skimr)
library(stringr)
library(dplyr)
library(ggplot2)

```

## 1. Import your data into R.

```{r}
## 1. Importing data into R

data <- read_csv("https://raw.githubusercontent.com/dilernia/STA418-518/main/Data/census_data_2008-2021.csv") 
head(data)
```

## 2. Explore and display high-level characteristics of your data set, e.g., important variables and their types, different levels for factor variables, any patterns of missing values.

```{r}
## To explore high-level characteristics of the data using the glimpse() function.
glimpse(data)
```

```{r}
## To explore any patterns of missing values

skim(data)
```

```{r}
## To get the column names of the dataset

colnames(data)
```

## 1. At least 5 distinct types of ggplot visualizations together visualizing at least 2 quantitative and 2 categorical variables. For example, side-by-side box plots, two separate scatter plots for different variables, and a bar chart would count as three. You are allowed and encouraged to create visualizations we did not explicitly cover in class.

```{r}
# Load necessary libraries
library(ggplot2)
library(dplyr)


# 1. Side-by-side box plots for median income by state
ggplot(data, aes(x = county_state, y = median_income, fill = county_state)) +
  geom_boxplot() +
  theme(legend.position = "none")

# 2. Scatter plot for median income vs. median monthly rent cost
ggplot(data, aes(x = median_income, y = median_monthly_rent_cost)) +
  geom_point()

# 3. Stacked bar chart for proportion of males and females by state
data %>%
  group_by(county_state) %>%
  summarize(prop_male = mean(prop_male), prop_female = mean(prop_female)) %>%
  pivot_longer(cols = c(prop_male, prop_female), names_to = "gender", values_to = "proportion") %>%
  ggplot(aes(x = county_state, y = proportion, fill = gender)) +
  geom_bar(stat = "identity", position = "stack")

# 4. Heatmap for median income by state and year
ggplot(data, aes(x = county_state, y = year, fill = median_income)) +
  geom_tile()

# 5. Line plot for poverty rate over time
ggplot(data, aes(x = year, y = prop_poverty, group = 1)) +
  geom_line() +
  geom_point()

```

## Generating 5 distinct types of ggplot visualizations for the top 20 county_states.

```{r}
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Filter data to top 20 county states by population
top_county_states <- data %>%
  group_by(county_state) %>%
  summarize(total_population = sum(population)) %>%
  arrange(desc(total_population)) %>%
  slice_head(n = 20) %>%
  pull(county_state)

data_top20 <- data %>%
  filter(county_state %in% top_county_states)

# 1. Side-by-side box plots for median income by state
ggplot(data_top20, aes(x = county_state, y = median_income, fill = county_state)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Scatter plot for median income vs. median monthly rent cost
ggplot(data_top20, aes(x = median_income, y = median_monthly_rent_cost)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Stacked bar chart for proportion of males and females by state
data_top20 %>%
  group_by(county_state) %>%
  summarize(across(c(prop_male, prop_female), mean)) %>%
  pivot_longer(cols = c(prop_male, prop_female), names_to = "gender", values_to = "proportion") %>%
  ggplot(aes(x = county_state, y = proportion, fill = gender)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4. Heatmap for median income by state and year
ggplot(data_top20, aes(x = county_state, y = year, fill = median_income)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5. Line plot for poverty rate over time
ggplot(data_top20, aes(x = year, y = prop_poverty)) +
  geom_line() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

```



## 2. At least two tables of summary statistics obtained using group-wise operations containing at least three statistics for each group, e.g., the sample size, sample mean, and sample standard deviation.

```{r}
## To obtain summary statistics for each county/state

county_state_stats <- data %>%
  group_by(county_state) %>%
  summarize(
    sample_size = n(),
    mean_pop = mean(population),
    sd_pop = sd(population),
    mean_income = mean(median_income),
    sd_income = sd(median_income),
    mean_rent = mean(median_monthly_rent_cost),
    sd_rent = sd(median_monthly_rent_cost)
  )

```


```{r}
## To obtain summary statistics for each year

year_stats <- data %>%
  group_by(year) %>%
  summarize(
    sample_size = n(),
    mean_pop = mean(population),
    sd_pop = sd(population),
    mean_income = mean(median_income),
    sd_income = sd(median_income),
    mean_home_cost = mean(median_monthly_home_cost),
    sd_home_cost = sd(median_monthly_home_cost)
  )

```

## Creating a stacked bar chart that displays the population by year and gender

```{r}
# Load necessary libraries
library(ggplot2)
library(dplyr)


# Aggregate population by year and gender
pop_data <- data %>%
  group_by(year) %>%
  summarize(prop_male = mean(prop_male * population), 
            prop_female = mean(prop_female * population)) %>%
  pivot_longer(cols = c(prop_male, prop_female), 
               names_to = "gender", 
               values_to = "population")

# Create stacked bar chart
ggplot(pop_data, aes(x = year, y = population, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Population by Year and Gender", 
       x = "Year", 
       y = "Population") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12)) 

```

## 3. Merging at least two tables of data together to produce the results tables or visualizations


```{r}
names(pop_data)
```


```{r}
names(year_stats)
```

```{r}

# Merging year_stats and pop_data by year
merged_data <- merge(year_stats, pop_data, by = "year")

# Creating a scatter plot of population by mean_income, colored by gender
# This visualization allows us to see whether there are any patterns or trends in the data, such as differences in the relationship between population and income by gender.

ggplot(data = merged_data, aes(x = mean_income, y = population, color = gender)) +
  geom_point() +
  ggtitle("Population and Mean Income by Gender") +
  xlab("Mean Income") +
  ylab("Population")

```


## 4. At least one data set should be pivoted between long and wide format

```{r}
# Here each observation is a unique combination of county_state and variable, and the value column contains the corresponding value for that combination.

# Pivoting county_state_stats from wide to long format

county_state_stats_long <- county_state_stats %>%
  pivot_longer(cols = c(sample_size, mean_pop, sd_pop, mean_income, sd_income, mean_rent, sd_rent),
               names_to = "variable",
               values_to = "value")

# Printing the first few rows of the long-format data

head(county_state_stats_long)

```


## 5. At least one string variable that is manipulated using stringr functions

** Here we have used stringr functions such as str_sub to extract sub string of a string. Here we are extracting state name from county_state **

```{r}
library(stringr)

# creating a new column with state abbreviation
data <- data %>%
  mutate(state = str_sub(county_state, -2)) 

# removing the underscores from the county_state column
data <- data %>%
  mutate(county_state = str_replace(county_state, "_", " "))

# extracting only the state name from county_state column
data <- data %>%
  mutate(state_name = str_extract(county_state, "([A-Za-z]+)\\s*$")) 


```


## 6. At least one Monte Carlo simulation for conducting statistical inference of your data


```{r}
# Defining population and median income parameters
pop_mean <- 10000
pop_sd <- 5000
income_mean <- 50000
income_sd <- 10000

# Generating 1000 random samples for population and median income using Monte Carlo simulation
n_samples <- 1000
pop_samples <- rnorm(n_samples, mean = pop_mean, sd = pop_sd)
income_samples <- rnorm(n_samples, mean = income_mean, sd = income_sd)

# Calculating 95% confidence intervals for population and median_income
pop_ci <- quantile(pop_samples, c(0.025, 0.975))
income_ci <- quantile(income_samples, c(0.025, 0.975))

# Printing confidence intervals
cat("95% Confidence Interval for Population: [", round(pop_ci[1],2), ",", round(pop_ci[2],2), "]\n")
cat("95% Confidence Interval for Median Income: [", round(income_ci[1],2), ",", round(income_ci[2],2), "]\n")

# Ploting histograms of the simulated population and median_income data
hist(pop_samples, breaks = 20, main = "Simulated Population Data", xlab = "Population")
hist(income_samples, breaks = 20, main = "Simulated Median Income Data", xlab = "Median Income")

```


## 7. At least one bootstrap approach for conducting meaningful statistical inference of your data

```{r}
# Extracting the population column from the data
population_data <- data$population

# Define the bootstrap function
bootstrap_mean <- function(data, nboot, alpha) {
  # Calculate the observed mean of the data
  obs_mean <- mean(data)
  
  # Initialize an empty vector to store the bootstrap means
  boot_means <- rep(0, nboot)
  
  # Generate nboot bootstrap samples and calculate the mean of each sample
  for (i in 1:nboot) {
    boot_sample <- sample(data, replace = TRUE)
    boot_means[i] <- mean(boot_sample)
  }
  
  # Calculate the lower and upper confidence bounds
  lb <- quantile(boot_means, prob = (1 - alpha) / 2)
  ub <- quantile(boot_means, prob = 1 - (1 - alpha) / 2)
  
  # Return a list containing the observed mean, bootstrap means, and confidence bounds
  return(list(obs_mean = obs_mean, boot_means = boot_means, lb = lb, ub = ub))
}

# Set the number of bootstrap samples and the confidence level
nboot <- 1000
alpha <- 0.05

# Call the bootstrap function on the population data
boot_results <- bootstrap_mean(population_data, nboot, alpha)

# Print the observed mean and confidence bounds
cat("Observed mean:", boot_results$obs_mean, "\n")
cat("95% Confidence Interval: (", boot_results$lb, ",", boot_results$ub, ")\n")

```

## Creating a histogram with the bootstrap means as the x-axis and the frequency of occurrence as the y-axis. 

** The observed mean will be marked with a red dashed line, and the confidence interval will be marked with blue dashed lines.**

** Here we have used the geom_vline() function to draw vertical lines on the plot.**



```{r}
library(ggplot2)

# Create a data frame of the bootstrap means
boot_means_df <- data.frame(boot_means = boot_results$boot_means)

# Create a histogram of the bootstrap means with confidence interval lines
ggplot(boot_means_df, aes(x = boot_means)) +
  geom_histogram(binwidth = 10000, color = "black", fill = "skyblue", alpha = 0.5) +
  geom_vline(xintercept = boot_results$obs_mean, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = boot_results$lb, color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = boot_results$ub, color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Bootstrap Means of Population Data",
       x = "Bootstrap Mean",
       y = "Frequency") +
  theme_bw()

```

** The histogram plot explains the visual representation of the bootstrap means and confidence bounds for the population data set, allowing for easy interpretation and communication of the results. **
