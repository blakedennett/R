
# Way to use libraries/packages in R
library(rlang)
library(readr)
library(tidyverse)
library(dplyr)
library(lubridate)



# read csv file and put it into a variable
data1 <- read_csv('C:/Users/Blake Dennett/Downloads/Spring2023/appliedProgramming/Data/stock_market_data/sp500/csv/j-p/nflx.csv')

# turn data into a dataframe
df <- data.frame(data1)

head(df, show_col_types = False)

# number of columns in df
print(ncol(df))

# number of rows in df
print(nrow(df))

# isolates a specific column
df$High

# get mean of the column
print(mean(df$High))

# first item specifies rows, second specifies column(s)
print(df[1:5, "High"])

# gets the volume and open of the first 5 columns
print(df[1:5, c("Volume", "Open", "Close")])

# gets the close if greater than 600
print(df[df$Close > 670, 1:ncol(df)])

# tidyvers version of previous line
print(
    df %>%
        select(Date, High, Low, Open, Close) %>%
        filter(Close > 680) %>%
        arrange(desc(Close))
)


# adding parts of the date to the df
df <- df %>%
    separate(Date, into=c('day', 'month', 'yr'), sep='-')


# make each part of the data numeric
df$month <- as.numeric(df$month)
df$day <- as.numeric(df$day)
df$yr <- as.numeric(df$yr)

# make a column that subtracts the high from the low
df$difference <- df$High - df$Low

# make a column that holds the difference in a percentage
df$percent_difference <- 1 - df$Low / df$High

# make a column with avg open by month per year
yrs <- min(df$yr):max(df$yr)   # makes a list from the first to last year
months <- min(df$month):max(df$month)
df$avg_open_month_per_yr <- 0

for (year in yrs) {
    for (m in months) {
        if (year != 2022 && m > 4) {
            avg_df <- df[df$yr == year & df$month == m, ]
            avg <- mean(avg_df$Open)
            # ifelse is the same as np.where in python
            if (any(df$yr == year & df$month == m)) {
                df$avg_open_month_per_yr <- ifelse(df$yr == year & df$month == m, avg, df$avg_open_month_per_yr)
            }
        }
    }
}


# make a column of yr open avg
df$yr_avg_open <- 0

for (year in yrs) {
    avg_df <- df[df$yr == year,]
    avg <- mean(avg_df$Open)
    df$yr_avg_open <- ifelse(df$yr == year, avg, df$yr_avg_open)
}


# plot avg open per year
plot_yr <- function() {
    print(ggplot(df, aes(x=yr, y=yr_avg_open)) + 
    geom_bar(stat = "identity"))
}
plot_yr()

# make avg month column
df$avg_open_per_month <- 0

for (m in months) {
    avg_df <- df[df$month == m, ]
    avg <- mean(avg_df$Open)
    df$avg_open_per_month <- ifelse(df$month == m, avg, df$avg_open_per_month)
}


# plot the avg price per month
plot_month <- function() {
    print(ggplot(df, aes(x=month, y=avg_open_per_month)) + 
    geom_bar(stat = "identity"))
}

# plot_month()



# find biggest jump from day one of month to last day of month
months <- list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12) 
df$month_difference <- 0

handle_months <- function(months) {
    for (m in months) {
        if ((year > 2002) || (year == 2002 && m > 4)) {
            df$month_difference <- ifelse(df$month == m & df$day == max(df[df$yr == year & df$month == m, "day"]) & df$yr == year, 
            df$Open - df[df$month == m & df$yr == year & df$day == min(df[df$yr == year & df$month == m, "day"]), "Open"], 
            df$month_difference)
        }
    }
    return(df)
}

for (year in yrs) {
    df <- handle_months(months)
}

df[is.na(df)] <- 0

# the biggest jump up was $85.84 and down was $-203.64 
max <- max(df$month_difference)
min <- min(df$month_difference)
max_date <- df[df$month_difference == max & df$day == max(df[ , "day"]), c("month", "yr")]
min_date <- df[df$month_difference == min & df$day == max(df[ , "day"]), c("month", "yr")]
print("===========================================================================================")
print("Biggest changes from day one of month to last day of month")
print(sprintf("The max was: %.2f", max))
print(sprintf("The month and year were %d/%d", as.integer(max_date[1]), as.integer(max_date[2])))
print(sprintf("The min was: %.2f", min))
print(sprintf("The month and year were %d/%d", as.integer(min_date[1]), as.integer(min_date[2])))
print("===========================================================================================")



# find biggest jump from day one of yr to last day of yr
df$yr_difference <- 0

for (year in yrs) {
    if (year != 2002) {
        df$yr_difference <- ifelse(df$yr == year & df$month == 1 & df$day == min(df[df$yr == year & df$month == 1, "day"]), 
        df[df$yr == year & df$month == 12 & df$day == max(df[ , "day"]), "Open"] - df$Open,
        df$yr_difference)
    }
}

df[is.na(df)] <- 0

max <- max(df$yr_difference)
min <- min(df$yr_difference)
max_yr <- df[df$yr_difference == max, "yr"]
min_yr <- df[df$yr_difference == min, "yr"]
print("Biggest changes from day one of year to last day of year")
print(sprintf("The max was: %.2f", max))
print(sprintf("The year was %.0f", as.integer(max_yr)))
print(sprintf("The min was: %.2f", min))
print(sprintf("The year was %.0f", as.integer(min_yr)))