
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```



# Data-Set Selected:

## Gun Violence Data

### Description
Information on instances of gun violence documented in the United States from January 2013 through March 2018. Note that gun violence incidents in 2013 are not all included, with only 279 incidents from 2013 in this data set, notably missing the Las Vegas Mass Shooting. The data is contained in gunViolenceFull.csv.

### Source:
Data was obtained from the GitHub repo https://github.com/jamesqo/gun-violence-data, which originally obtained the data from the Gun Violence Archive’s website. From the organization’s description:
Gun Violence Archive (GVA) is a not for profit corporation formed in 2013 to provide free online public access to accurate information about gun-related violence in the United States. GVA will collect and check for accuracy, comprehensive information about gun-related violence in the U.S. and then post and disseminate it online.


# Project Requirements

### Loading Required Libraries
```{r}

# Some of these libraries might not be installed in your system, please install them using console first before proceeding with this step.
library(tidyverse)
library(stringr)
library(skimr)
library(knitr)
library(ggthemes)
library(VIM)
library(naniar)
library(lubridate)
library(flextable)
library(gt)
library(gtExtras)
library(kableExtra)
library(viridis)
library(ggmap)
```
### Loading the Given Data
```{r}
Gun_Data <- read.csv('gunviolenceFull.csv')
head(Gun_Data)
```


## Data Dictionary and Exploratory Data Analysis


### Data Dictionary and missingness patterns
```{r}

# Define variable names, descriptions, units, and types
var_names <- c("incident_id", "date", "state", "city_or_county", "address", "n_killed", "n_injured", "incident_url", "source_url", "incident_url_fields_missing", "congressional_district", "gun_stolen", "gun_type", "incident_characteristics", "lat", "location_description", "long", "n_guns_involved", "notes", "participant_age", "participant_age_group", "participant_gender", "participant_name", "participant_relationship", "participant_status", "participant_type", "sources", "state_house_district", "state_senate_district", "address_full")
var_desc <- c("Unique identifier for each incident", "Date of the incident", "State where the incident occurred", "City or county where the incident occurred", "Address of the incident", "Number of people killed in the incident", "Number of people injured in the incident", "URL for the incident details page on the Gun Violence Archive website", "URL for the source article about the incident", "Indicator of missing fields in the incident URL", "Congressional district where the incident occurred", "Indicator of whether the gun was stolen", "Type of gun used in the incident", "Description of the incident characteristics", "Latitude of the incident location", "Description of the incident location", "Longitude of the incident location", "Number of guns involved in the incident", "Additional notes about the incident", "Age of each participant in the incident", "Age group of each participant in the incident", "Gender of each participant in the incident", "Name of each participant in the incident", "Relationship of each participant to the incident", "Status of each participant in the incident", "Type of each participant in the incident", "Sources of information about the incident", "State House district where the incident occurred", "State Senate district where the incident occurred", "Full address of the incident location")
var_units <- c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "degrees", "", "degrees", "", "", "", "", "", "", "", "", "", "", "", "", "")
var_type <- c("integer", "date", "character", "character", "character", "integer", "integer", "character", "character", "logical", "integer", "character", "character", "character", "numeric", "character", "numeric", "integer", "character", "character", "character", "character", "character", "character", "character", "character","character", "integer", "integer", "character")


# Create data frame for Data Dictionary
data_dict <- data.frame(var_names, var_desc, var_units, var_type)

# Print Data Dictionary
knitr::kable(data_dict, col.names = c("Variable Name", "Description", "Units", "Type"), caption = "Data Dictionary for Sample Data")

# Using the glimpse to get the structure of data.
Gun_Data %>% glimpse()

# Using Skim to get the high-level characteristics of the dataset.
Gun_Data %>% skimr::skim()

# Generate the missingness pattern plot
gg_miss_var(Gun_Data)

```

### Merging Datasets
```{r}
# First, we'll create a dataset state_counts that counts the number of incidents in each state
state_counts <- aggregate(incident_id ~ state, data = Gun_Data, FUN = length)

# Next, we'll create a dataset state_killed that sums the number of people killed in each state
state_killed <- aggregate(n_killed ~ state, data = Gun_Data, FUN = sum)

#Finally, we can merge the two datasets based on the state variable
state_merged <- merge(state_counts, state_killed, by = "state")


ggplot(state_merged, aes(x = state)) +
  geom_bar(aes(y = incident_id), stat = "identity", fill = "blue", alpha = 0.7) +
  geom_bar(aes(y = n_killed), stat = "identity", fill = "red", alpha = 0.7) +
  labs(x = "State", y = "Count", title = "Gun Violence Incidents and Deaths by State")
```

### Date / Time Manipulation
```{r}
# extract the month from the date variable
Gun_Data$month <- month(ymd(Gun_Data$date))

# view the new variable
head(Gun_Data$month,100)
```

### String Manipulattion
```{r}
# extract only the street name from the address variable
Gun_Data$street <- str_extract(Gun_Data$address, "^[^,]*")

# output the modified dataframe
head(Gun_Data$street)
```

### Tables of Summary Statistics
```{r}
# Table 1

incident_summary <- Gun_Data %>%
  group_by(state) %>%
  summarise(n_incidents = n()) %>%
  arrange(desc(n_incidents))

flextable(incident_summary) %>%
  set_caption("Table 1: Number of gun violence incidents by state") %>%
  theme_vanilla()
```

```{r}
# Table 2

death_injury_summary <- Gun_Data %>%
  separate(date, into = c("month", "day", "year"), sep = "/") %>%
  group_by(year) %>%
  summarise(n_killed = sum(n_killed), n_injured = sum(n_injured))

kable(death_injury_summary, format = "markdown", align = "c") %>%
  kable_styling() %>%
  add_header_above(c(" " = 1, "Total Number of Deaths and Injuries by Year" = 2)) %>%
  row_spec(0, bold = TRUE)
```

## Data Visualizations


Plot 1: Scatter plot of latitude and longitude of incidents

        The plot shows the latitude and longitude of each gun violence incident.
        The x-axis represents the longitude and the y-axis represents the latitude.
        The title of the plot is "Geographic Distribution of Gun Violence Incidents" and the caption includes the source of the data.
        The points are colored in blue for emphasis.
        The plot shows that gun violence incidents are distributed across the United States, with some clustering in urban areas.
```{r}
# Create a scatter plot of the latitude and longitude of incidents
ggplot(Gun_Data, aes(x = long, y = lat, color = state)) +
  geom_point(alpha = 0.6) +
  labs(title = "Latitude and Longitude of Incidents",
       x = "Longitude",
       y = "Latitude",
       color = "State") +
  theme(plot.title = element_text(hjust = 0.5))
```
Plot2:

Bar plot that shows the number of gun-related deaths per state. The data is stored in a data frame called Gun_Data. The x-axis represents the states and the y-axis represents the number of gun-related deaths.

```{r}
ggplot(data = Gun_Data, aes(x = state, y = n_killed)) +
  geom_bar(stat = "identity", fill = "black", color = "darkred") + 
  labs(title = "Gun-related Deaths per State") + 
  theme(axis.text.y = element_text(hjust = 1, size = 8)) + 
  coord_flip()
```

Plot 3: 

```{r}
# Create a summary table of number of incidents per state
state_summary <- Gun_Data %>% 
  group_by(state) %>% 
  summarize(num_incidents = n())

# Create a total count of incidents
total_incidents <- sum(state_summary$num_incidents)

# Add a column for percentage of incidents
state_summary$percent_incidents <- state_summary$num_incidents / total_incidents * 100

# Sort by percent_incidents descending
state_summary <- state_summary[order(-state_summary$percent_incidents),]

# Create donut chart
ggplot(state_summary, aes(x = "", y = percent_incidents, fill = state)) + 
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y", start = 0, direction = -1) +
  scale_fill_hue(name = "State") +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "Gun Violence Incidents by State",
       subtitle = paste("Total Incidents:", total_incidents),
       fill = "State", 
       caption = "Data source: Gunviolence.csv")
```


Plot 4:

```{r}
age_data <- Gun_Data %>%
  filter(!is.na(participant_age_group)) %>%
  mutate(participant_age_group = factor(participant_age_group, levels = c("Child 0-11", "Teen 12-17", "Adult 18+")),
         date = as.Date(date)) %>%
  group_by(participant_age_group, year = format(date, "%Y")) %>%
  summarise(count = n()) %>%
  arrange(year, participant_age_group)

ggplot(age_data, aes(x = year, y = count, fill = participant_age_group)) +
  geom_bar(stat = "identity") +
  labs(title = "Participant Age Groups in Gun Violence Incidents", x = "Year", y = "Number of Participants", fill = "Age Group") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
  theme_bw()
```

Plot 5:

```{r}
# create a ridgeline chart with the number of shooting victims on the x-axis and the state on the y-axis.

library(ggridges)

# Create a subset of the data with only the variables we need
shootings_sub <- Gun_Data %>% 
  select(state, n_killed, n_injured)

# Create a ridgeline chart
ggplot(shootings_sub, aes(x = n_killed + n_injured, y = state, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_gradient(low = "gray90", high = "red") +
  labs(title = "Number of Shooting Victims by State", 
       x = "Number of Victims", y = "State") +
  theme_minimal() + 
   xlim(0, 3)
```

### Monte Carlo Methods of Inference

One non-trivial question we can explore using Monte Carlo simulation is whether there is a significant difference in the average number of people killed in mass shootings in the top 10 states with the most incidents compared to the bottom 10 states. We can formulate the null and alternative hypotheses as follows:

Null hypothesis: There is no significant difference in the average number of people killed in mass shootings between the top 10 states and the bottom 10 states.
Alternative hypothesis: The top 10 states have a significantly higher average number of people killed in mass shootings than the bottom 10 states.

To test this hypothesis, we can use a permutation test. Specifically, we can randomly permute the state labels and calculate the difference in means between the top 10 and bottom 10 states. We can repeat this process many times to obtain the null distribution of the test statistic. We can then compare the observed difference in means to the null distribution and calculate the p-value as the proportion of simulated differences that are greater than or equal to the observed difference.

```{r}
# Subset the data to include only mass shootings
mass_shootings <- Gun_Data %>%
  filter(n_killed >= 4)

# Create a vector indicating whether each state is in the top 10 or bottom 10
top_bottom_vec <- Gun_Data %>%
  group_by(state) %>%
  summarize(total_incidents = n()) %>%
  arrange(desc(total_incidents)) %>%
  mutate(top_bottom = ifelse(rank(total_incidents) <= 10, "Top 10", ifelse(rank(total_incidents) >= 43, "Bottom 10", "Other"))) %>%
  filter(top_bottom %in% c("Top 10", "Bottom 10")) %>%
  pull(top_bottom)

# Define the test statistic function
diff_means <- function(x, y) {
  mean(mass_shootings$n_killed[x]) - mean(mass_shootings$n_killed[y])
}

# Compute the observed difference in means
obs_diff <- diff_means(top_bottom_vec == "Top 10", top_bottom_vec == "Bottom 10")

# Permutation test
set.seed(123)
n_permutations <- 10000
permuted_diffs <- replicate(n_permutations, {
  permuted_vec <- sample(top_bottom_vec)
  diff_means(permuted_vec == "Top 10", permuted_vec == "Bottom 10")
})

# Compute p-value
p_value <- mean(abs(permuted_diffs) >= abs(obs_diff))

# Display null distribution
null_dist <- tibble(diff = permuted_diffs) %>%
  ggplot(aes(x = diff)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "gray") +
  geom_vline(xintercept = obs_diff, color = "red", size = 1.5) +
  labs(title = "Null Distribution of Difference in Means",
       x = "Difference in Means",
       y = "Frequency") +
  theme_classic()

# Display results
cat("Observed difference in means: ", obs_diff, "\n")
cat("P-value: ", p_value, "\n")
null_dist
```



### Bootstrap Methods

```{r}
# Subset data to only include incidents in California
ca_data <- Gun_Data %>% filter(state == "California")

# Define function to calculate median number of people injured in bootstrap samples
med_boot <- function(data, i) {
  med <- median(sample(data$n_injured, replace = TRUE))
  return(med)
}

# Generate bootstrap distribution
set.seed(123) # for reproducibility
boot_dist <- replicate(1000, med_boot(ca_data, i = 1:nrow(ca_data)))

# Calculate 95% confidence interval
ci_lower <- quantile(boot_dist, 0.025)
ci_upper <- quantile(boot_dist, 0.975)

# Plot bootstrap distribution
ggplot(data.frame(median = boot_dist), aes(x = median)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "lightblue") +
  geom_vline(xintercept = median(ca_data$n_injured), color = "red", linetype = "dashed", size = 1.2) +
  geom_vline(xintercept = ci_lower, color = "green", linetype = "dashed", size = 1.2) +
  geom_vline(xintercept = ci_upper, color = "green", linetype = "dashed", size = 1.2) +
  labs(title = "Bootstrap Distribution of Median Number of People Injured in Gun Violence Incidents in California",
       x = "Median Number of People Injured",
       y = "Frequency") +
  theme_minimal()
```
