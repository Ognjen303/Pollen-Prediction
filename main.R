# main.R
# Bayesian Data Analysis for fitting
# Weekly pollen concentrations over multiple years

if (!require(tidybayes)) {
  install.packages("tidybayes")
  library(tidybayes)
}
if (!require(metadat)) {
  install.packages("metadat")
  library(metadat)
}
if(!require(cmdstanr)){
  install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
  library(cmdstanr)
}
cmdstan_installed <- function(){
  res <- try(out <- cmdstanr::cmdstan_path(), silent = TRUE)
  !inherits(res, "try-error")
}
if(!cmdstan_installed()){
  install_cmdstan()
}
if(!require(bayesplot)){
  install.packages("bayesplot")
  library(bayesplot)
}
if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}
if(!require(tidyr)){
  install.packages("tidyr")
  library(tidyr)
}
if(!require(ggdist)){
  install.packages("ggdist")
  library(ggdist)
}
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require(brms)) {
  install.packages("brms")
  library(brms)
}
if(!require(astsa)){
  install.packages("astsa")  # if you haven't already
  library(astsa)
}
if(!require(forecast)){
  install.packages("forecast")  # if you haven't already
  library(forecast)
}
if(!require(tidyverse)){
  install.packages("tidyverse")  # if you haven't already
  library(tidyverse)
}
if(!require(fpp2)){
  install.packages("fpp2")  # if you haven't already
  library(fpp2)
}
if(!require(scales)){
  install.packages("scales")  # if you haven't already
  library(scales)
}


# Read the csv file
becej_weekly_data <- read.csv("data/final_data/becej_weekly.csv", encoding = "UTF-8")

data <- becej_weekly_data %>% mutate(across(c('y'), round))

# Define the model formula
formula <- bf(y ~ 1 + (1 | year),
              family = "poisson")

(priors <- c(
  prior( # population-level intercept
    normal(0, 10), 
    class = "Intercept"
  ),
  prior( # group-level intercept
    normal(0, 10),
    class = "sd",
    group = "year",
    coef = "Intercept"
  )#,
  #prior( # group-level intercept
  #  normal(0, 10),
  #  class = "sd",
  #  group = "year",
  #  coef = "week"
  #)
))


# Fit the model
fit <- brm(formula,
          data = data,
          prior = priors,
          seed = 4911,
          iter = 4000,
          control = list(adapt_delta = 0.92)
)


# Print the summary of the fit
summary(fit)


# Generate posterior predictive samples
posterior_preds <- posterior_predict(fit)


# Summarize the posterior predictions
data_with_preds <- data %>%
    mutate(
        pred_median = round(apply(posterior_preds, 2, median)),
        pred_lower = apply(posterior_preds, 2, quantile, 0.025),
        pred_upper = apply(posterior_preds, 2, quantile, 0.975)
    )



# -------------------------Plotting------------------------------


# The time range being plot
years <- 2016:2024
weeks <- 1:53


# Create a grid of all weeks and year combinations

all_week_years <- expand.grid(year = years, week = weeks) %>%
  mutate(week_year = paste(week, year, sep = "_")) %>%
  arrange(year, week) %>%  # Order by year and week
  pull(week_year)


# Merge posterior predictions sequence with the complete data frame
data_with_preds_full <- all_week_years %>% 
  as_tibble() %>% 
  rename(week_year = value) %>% 
  left_join(data_with_preds, by = "week_year")


data_with_preds_full$week_year <- factor(data_with_preds_full$week_year, levels = unique(data_with_preds_full$week_year))

# Extract week_year values
selected_labels <- all_week_years[grep("^(1_|18_|36_)", all_week_years)]



# Custom function to create multi-line x-axis labels
multi_line_labels <- function(week_year) {
  

  # Create an empty output array
  # years <- numeric(length(week_year))
  years <- rep("", length(week_year))
  
  print(years)
  
  # Loop through the input array and check the first three characters
  # Place a year label only below '18'
  week18_indices <- substr(week_year, 1, 3) == "18_"
  years[week18_indices] <- substr(week_year[week18_indices], 4, 7)
  
  # Extract the week
  week_part <- as.numeric(sapply(strsplit(week_year, "_"), `[`, 1))
  
  print(week18_indices)
  print(years)
  
  paste(week_part, years, sep = "\n")
}





# Create the plot with adjusted aesthetics and legend
ggplot(data_with_preds_full, aes(x = week_year, y = y, group = year)) +
  geom_point(na.rm = TRUE) +  # Original data points
  geom_line(aes(y = pred_median), color = "red", size = 1, na.rm = TRUE) +  # Posterior mean
  geom_ribbon(data = na.omit(data_with_preds_full),  # Temporarily remove rows with missing values
              aes(ymin = pred_lower, ymax = pred_upper),
              fill = "red", alpha = 0.2) +
  scale_x_discrete(
    breaks = selected_labels,
    labels = multi_line_labels
  ) +  # Place the new labels on the x-axis using the selected week years
  labs(
    title = "Weekly Measurement from 2016 to 2024", # Posterior Predictive Distribution and 
    x = "Week_Year",
    y = "Pollen Concentration (grains/m^3)",
  ) +
  theme(axis.text=element_text(size=13),
           axis.title=element_text(size=15,face="bold"),
        plot.title = element_text(size = 16, face = "bold")
  )
