
# Way to use libraries/packages in R
library(rlang)
library(readr)



# read csv file and put it into a variable
data1 <- read_csv('C:/Users/Blake Dennett/Downloads/Spring2023/appliedProgramming/Data/stock_market_data/sp500/csv/j-p/nflx.csv')

# turn data into a dataframe
nflx_data <- data.frame(data1)

head(nflx_data, show_col_types = False)

# isolates a specific column
nflx_data$High

# first item specifies rows, second specifies column(s)
print(nflx_data[1:5, "High"])

