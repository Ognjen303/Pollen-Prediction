# BECEJ WEEKLY ANALYSIS (ALL YEARS)

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



# Read the csv file
becej_weekly_data <- read.csv("data/final_data/becej_weekly.csv", encoding = "UTF-8")

data <- becej_weekly_data %>% mutate(across(c('y'), round))

# Define the model formula
formula <- bf(y ~ 1 + (1 + week| year),
              family = "poisson")


# If we chose normal(0, 1) the model would diverge


(priors <- c(
  prior( # population-level intercept
    normal(0, 1000), 
    class = "Intercept"
  ),
  prior( # group-level intercept
    normal(0, 1000),
    class = "sd",
    group = "year",
    coef = "Intercept"
  ),
  prior( # group-level intercept
    normal(0, 1000),
    class = "sd",
    group = "year",
    coef = "week"
  )
))


# Fit the model
fit_norm10 <- brm(formula,
           data = data,
           prior = priors,
           seed = 4911,
           iter = 4000,
           control = list(adapt_delta = 0.92)
           )

summary(fit_norm10)


# Generate posterior predictive samples
posterior_preds <- posterior_predict(fit_norm10)


# Summarize the posterior predictions
data_with_preds <- data %>%
    mutate(
        pred_mean = round(apply(posterior_preds, 2, mean)),
        pred_lower = apply(posterior_preds, 2, quantile, 0.025),
        pred_upper = apply(posterior_preds, 2, quantile, 0.975)
    )


# Posterior predictive plotting

years <- 2016:2024
weeks <- 1:53

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






# Extract labels for February each year for the x-axis
selected_labels <- all_week_years[substr(all_week_years, 1, 1) == "8"]

# Define the new labels
new_labels <- c('Feb 2016', 'Feb 2017', 'Feb 2018', 'Feb 2019', 'Feb 2020', 'Feb 2021', 'Feb 2022', 'Feb 2023', 'Feb 2024')



# Define the legend labels
legend_labels <- c("Original Data", "Posterior Predictive")

# Create the plot with adjusted aesthetics and legend
ggplot(data_with_preds_full, aes(x = week_year, y = y, group=year)) +
  geom_point(na.rm = TRUE) +  # Original data points
  geom_line(aes(y = pred_mean), color = "red", size = 1, na.rm = TRUE) +  # Posterior mean
  geom_ribbon(data = na.omit(data_with_preds_full),  # Temporarily remove rows with missing values
              aes(ymin = pred_lower, ymax = pred_upper),
              fill = "red", alpha = 0.2) +
  scale_x_discrete(breaks = selected_labels, labels = new_labels) +  # Place the new labels on the x-axis using the selected week years
  labs(
    title = "Posterior Predictive Distribution and Original Data",
    x = "Date",
    y = "Pollen Concentration (grains/m^3)"
  ) +
  theme_minimal() +  # Minimal theme
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Increase x-axis tick font size and angle
    axis.text.y = element_text(size = 12),  # Increase y-axis tick font size
    axis.title.x = element_text(size = 14),  # Increase x-axis label font size
    axis.title.y = element_text(size = 14),  # Increase y-axis label font size
    plot.title = element_text(size = 16, face = "bold"),  # Increase title font size and weight
    legend.position = "top",  # Set legend position to top
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 12),  # Increase legend font size
    legend.background = element_rect(fill = "white", color = "black"),  # Style legend background
    legend.box.margin = margin(5, 5, 5, 5)  # Add margin around the legend
  ) +
  guides(color = guide_legend(title = NULL, override.aes = list(shape = c(16, NA), linetype = c(0, 1)), 
                              labels = legend_labels))  # Add and style legend
