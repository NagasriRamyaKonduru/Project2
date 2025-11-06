# exploration.R
# Static data analysis for Mobile Device Usage Project

library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(corrplot)

# ---- Load Data ----
data <- read_csv("data/user_behavior_dataset.csv", show_col_types = FALSE)

# Clean column names
data <- data %>%
  rename_with(~ gsub(" ", "_", .x)) %>%
  rename(
    App_Usage_Time = `App_Usage_Time_(min/day)`,
    Screen_On_Time = `Screen_On_Time_(hours/day)`,
    Battery_Drain = `Battery_Drain_(mAh/day)`,
    Data_Usage = `Data_Usage_(MB/day)`
  )

# ---- Summaries ----
cat("\n===== Dataset Summary =====\n")
print(summary(data))

# One-way table
cat("\n===== One-way Table: Gender =====\n")
if ("Gender" %in% names(data)) print(table(data$Gender))

# Two-way table
cat("\n===== Two-way Table: Gender x Operating_System =====\n")
if (all(c("Gender", "Operating_System") %in% names(data))) print(table(data$Gender, data$Operating_System))

# Numeric summaries
cat("\n===== Mean Screen On Time by Gender =====\n")
if (all(c("Gender", "Screen_On_Time") %in% names(data))) {
  print(
    data %>%
      group_by(Gender) %>%
      summarise(
        Mean_Screen = mean(Screen_On_Time, na.rm = TRUE),
        SD_Screen = sd(Screen_On_Time, na.rm = TRUE),
        .groups = "drop"
      )
  )
}

# ---- Plots ----
cat("\n===== Generating Plots =====\n")

# 1. Histogram – Screen On Time
p1 <- ggplot(data, aes(x = Screen_On_Time)) +
  geom_histogram(fill = "skyblue", bins = 20, color = "white") +
  labs(title = "Distribution of Screen On Time (hours/day)", x = "Screen On Time (hours/day)", y = "Count")
print(p1)

# 2. Boxplot – Screen Time by Gender
if ("Gender" %in% names(data)) {
  p2 <- ggplot(data, aes(x = Gender, y = Screen_On_Time, fill = Gender)) +
    geom_boxplot() +
    labs(title = "Screen On Time by Gender", x = "Gender", y = "Screen On Time (hours/day)")
  print(p2)
}

# 3. Scatterplot – App Usage vs Screen Time
if (all(c("App_Usage_Time", "Screen_On_Time") %in% names(data))) {
  p3 <- ggplot(data, aes(x = App_Usage_Time, y = Screen_On_Time, color = Gender)) +
    geom_point(alpha = 0.7) +
    labs(title = "App Usage (min/day) vs Screen On Time (hours/day)",
         x = "App Usage (min/day)", y = "Screen On Time (hours/day)")
  print(p3)
}

# 4. Bar chart – Average Battery Drain by OS
if (all(c("Operating_System", "Battery_Drain") %in% names(data))) {
  p4 <- ggplot(data, aes(x = Operating_System, y = Battery_Drain, fill = Operating_System)) +
    stat_summary(fun = mean, geom = "bar") +
    labs(title = "Average Battery Drain by Operating System", x = "Operating System", y = "Battery Drain (mAh/day)")
  print(p4)
}

# 5. Faceted plot – Data Usage vs Screen On Time by OS
if (all(c("Data_Usage", "Screen_On_Time", "Operating_System") %in% names(data))) {
  p5 <- ggplot(data, aes(x = Data_Usage, y = Screen_On_Time)) +
    geom_point(color = "steelblue") +
    facet_wrap(~Operating_System) +
    labs(title = "Data Usage vs Screen On Time by Operating System",
         x = "Data Usage (MB/day)", y = "Screen On Time (hours/day)")
  print(p5)
}

# 6. Heatmap – Correlation among numeric variables
numeric_data <- data %>% select(where(is.numeric))
if (ncol(numeric_data) > 1) {
  corr <- cor(numeric_data, use = "pairwise.complete.obs")
  corrplot::corrplot(corr, method = "color", tl.cex = 0.8)
}

cat("\n===== End of Exploration =====\n")

