
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
frst_yr <- min(df$yr)
lst_yr <- max(df$yr)
yrs <- frst_yr:lst_yr   # makes a list from the first to last
months <- min(df$month):max(df$month)
df$avg_open_month <- 0

for (year in yrs) {
    for (m in months) {
        if (year != 2022 && m > 4) {
            avg_df <- df[df$yr == year & df$month == m, ]
            avg <- mean(avg_df$Open)
            # ifelse is the same as np.where in python
            if (any(df$yr == year & df$month == m)) {
                df$avg_open_month <- ifelse(df$yr == year & df$month == m, avg, df$avg_open_month)
            }
        }
    }
}


# make a column of yr open avg



print(head(df[df$avg_open_month > 0, ]))



# ================================================= plot the avg price per month



# ================================================= find biggest jump from day one of month to last day of month