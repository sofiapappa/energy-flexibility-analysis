library(dplyr)
library(lubridate)
library(tidyr)
library(corrplot)

df <- read.csv("data/energy_flexibility_data.csv", stringsAsFactors = FALSE)

time_col <- "timestamp"
df$timestamp <- ymd_hms(df$timestamp, tz="UTC")
df <- arrange(df, timestamp)

# Basic data-frame checks
str(df)
summary(df)

appliances <- setdiff(names(df), "timestamp")

par(mfrow = c(3, 2))  # 3 rows, 2 columns

for (col in appliances) {
  plot(df$timestamp, df[[col]],
       type = "l",
       xlab = "Time",
       ylab = "kW",
       main = col)
}

par(mfrow = c(1, 1))  # reset layout

df <- df |> mutate(date = as.Date(timestamp),
                   hour = hour(timestamp),
                   wday = wday(timestamp, label = TRUE))
head(df) 

#check for missing colums:
colSums(is.na(df))

#daily energy per appliance:
dt_h <- 0.25
daily_energy <- df |> group_by(date) |> 
  summarise(across(all_of(appliances),
                   function(x) sum(x * dt_h, na.rm = TRUE),
                   .names = "{sub('_kW','',.col)}_daily_kWh"))
daily_energy

X <- daily_energy |> select(-date)

cor_mat <- cor(X, use="pairwise.complete.obs", method = "pearson")
heatmap(cor_mat,
        symm = TRUE,
        margins = c(6, 6))


corrplot(cor_mat, method = "circle", type = "upper")

# load factor summary
load_stats_table <- df |>
  select(all_of(appliances)) |> 
  pivot_longer(
    cols = everything(), 
    names_to = "Appliance", 
    values_to = "kW"
  ) |>
  group_by(Appliance) |>
  summarise(
    mean = mean(kW, na.rm = TRUE),
    max  = max(kW, na.rm = TRUE),
    p95  = quantile(kW, 0.95, na.rm = TRUE)
  )
load_stats_table

#“pattern finding” step (CRISP-DM)
pca <- prcomp(X, center = TRUE, scale. = TRUE)
summary(pca)
pca$rotation[, 1:3]   # loadings: which appliances drive PC1/PC2/PC3

