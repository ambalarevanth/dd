# Setup the dimension tables
state_table <- data.frame(key=c("CA", "NY", "WA", "ON", "QU"),
                          name=c("California", "New York", "Washington", "Ontario", "Quebec"),
                          country=c("USA", "USA", "USA", "Canada", "Canada"))

month_table <- data.frame(key=1:12,
                          desc=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                          quarter=c("Q1","Q1","Q1","Q2","Q2","Q2","Q3","Q3","Q3","Q4","Q4","Q4"))

prod_table <- data.frame(key=c("Printer", "Tablet", "Laptop"), price=c(225, 570, 1120))

# Function to generate the Sales table
gen_sales <- function(no_of_recs) {
  # Generate transaction data randomly
  loc <- sample(state_table$key, no_of_recs, replace=T, prob=c(2,2,1,1,1))
  time_month <- sample(month_table$key, no_of_recs, replace=T)
  time_year <- sample(c(2012, 2013), no_of_recs, replace=T)
  prod <- sample(prod_table$key, no_of_recs, replace=T, prob=c(1, 3, 2))
  unit <- sample(c(1,2), no_of_recs, replace=T, prob=c(10, 3))
  amount <- unit * prod_table[prod,]$price
  sales <- data.frame(month=time_month, year=time_year, loc=loc, prod=prod, unit=unit, amount=amount)
  # Sort the records by time order
  sales <- sales[order(sales$year, sales$month),]
  row.names(sales) <- NULL
  return(sales)
}

# Now create the sales fact table
sales_fact <- gen_sales(500)

# Look at a few records
head(sales_fact)

# Build up a cube
revenue_cube <- tapply(sales_fact$amount, sales_fact[,c("prod", "month", "year", "loc")],
                       FUN=function(x){return(sum(x))})

# Showing the cells of the cube
print("Revenue Cube:")
print(revenue_cube)

# Displaying dimension names of the cube
print("Dimension Names:")
print(dimnames(revenue_cube))

# Slice: cube data in Jan, 2012
print("Cube data in Jan, 2012:")
print(revenue_cube[, "1", "2012",])

# Slice: cube data for Tablet in Jan, 2012
print("Cube data for Tablet in Jan, 2012:")
print(revenue_cube["Tablet", "1", "2012",])







# Simple Calculator in R

# Function to perform basic calculations
calculate <- function(a, operator, b) {
  switch(operator,
         "+", a + b,
         "-", a - b,
         "*", a * b,
         "/", ifelse(b != 0, a / b, "Error: Division by zero"),
         "Invalid operator")
}

# Read input from the user
num1 <- as.numeric(readline("Enter the first number: "))
operator <- readline("Enter the operator (+, -, *, /): ")
num2 <- as.numeric(readline("Enter the second number: "))

# Perform calculation and display result
result <- calculate(num1, operator, num2)
cat("Result:", result, "\n")

