# main.R
# Fitting a hierarchical model for
# Weekly pollen concentrations spanning multiple years

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
# Library for data manipulation (e.g. mutate, across, left_join)
if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

# For model fitting and posterior predictive sampling
if (!require(brms)) {
  install.packages("brms")
  library(brms)
}


# Read in data
weekly_pollen_measurements <- read.csv("data/final_data/becej_weekly.csv", encoding = "UTF-8")

# R-like assert statement
stopifnot(c("week", "year", "week_year", "y") %in% colnames(weekly_pollen_measurements))

# Round measurements to int.
# Because we fit a Poisson model, which works with ints.
data <- weekly_pollen_measurements %>% mutate(across(c('y'), round))


# Define the model formula in brms
formula <- bf(y ~ 1 + (1 + week | year),
              family = "poisson")

# Set priors
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
  ),
  prior( # group-level slope
    normal(0, 10),
    class = "sd",
    group = "year",
    coef = "week"
  )
))


# Fit the model using Hamiltonian MC in brms
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
pp_samples <- posterior_predict(fit)

# Augment data by adding columns pp_median, pp_lower and pp_upper
data_pp <- data %>%
  mutate(
    pp_median = round(apply(pp_samples, 2, median)),
    pp_lower = apply(pp_samples, 2, quantile, 0.025),
    pp_upper = apply(pp_samples, 2, quantile, 0.975)
  )

required_columns <- c("week", "year", "week_year", "y", "pp_median", "pp_lower", "pp_upper")

stopifnot(required_columns %in% colnames(data_pp))

# -------------------------Plotting------------------------------


# Time range to plot
years <- 2016:2024
weeks <- 1:53


# Create a grid of all weeks and year combinations
grid_week_years <- expand.grid(year = years, week = weeks) %>%
  mutate(week_year = paste(week, year, sep = "_")) %>%
  arrange(year, week) %>%  # Order by year and week
  pull(week_year)


# Merge data_pp with the grid.
grid_data_pp <- grid_week_years %>%
  as_tibble() %>%                          # convert into a tibble (a tidy version of data.frame)
  rename(week_year = value) %>%            # rename column value to week_year
  left_join(data_pp, by = "week_year")     # Join two datasets by column week_year.
# Retain all rows from the left table (grid_week_years)
# and adding matching rows to the right table (data_pp).
# For unmatched rows, the columns from the right table will have NA values.


stopifnot(required_columns %in% colnames(grid_data_pp))

# Preserve week_year order as a factor
grid_data_pp$week_year <- factor(grid_data_pp$week_year, levels = unique(grid_data_pp$week_year))


# Extract week_year values to be plotted as x-axis labels
selected_labels <- grid_week_years[grep("^(1_|18_|36_)", grid_week_years)]


# Creates multi-line x-axis labels
multi_line_labels <- function(week_year) {


  # Create an empty output array
  years <- rep("", length(week_year))

  # Place a year label only below '18'
  week18_indices <- substr(week_year, 1, 3) == "18_"

  # Extract YYYY from the strings
  years[week18_indices] <- substr(week_year[week18_indices], 4, 7)

  # Extract the week
  week_part <- as.numeric(sapply(strsplit(week_year, "_"), `[`, 1))

  paste(week_part, years, sep = "\n")
}


# Create the plot with adjusted aesthetics and legend
ggplot(grid_data_pp, aes(x = week_year, y = y, group = year)) +
  geom_point(na.rm = TRUE) +  # Original data points
  geom_line(aes(y = pp_median), color = "red", size = 1, na.rm = TRUE) +  # Posterior mean
  geom_ribbon(data = na.omit(grid_data_pp),  # Temporarily remove rows with missing values
              aes(ymin = pp_lower, ymax = pp_upper),
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
