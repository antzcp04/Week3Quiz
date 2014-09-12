# 1. Write a function that takes a numeric vector and calculates the mean of the observations in the vector.

v <- sample(1:20, 10, replace=T)
getMean <- function(v) {
   vResult <- mean(v)
   print(vResult)  
}
  

# 2. Modify your function in the previous question so that it can handle a numeric vector with missing values.
v <- c(1,2,3,4,5,6,7,8,9,10,NA)
getMean <- function(v) {

  vResult <- mean(v, na.rm=TRUE)
  print(vResult)  

}

3. Write a function that takes two numeric input values and calculates the greatest common divisor of the two 
numbers.


gcd <- function(a,b)  {
  
  if ( b==0  )  {
    
    a
  } else {
    gcd(b, a %% b)
  }
  
}

#4. Write a function that implements Euclid's algorithm (you may need to do a bit of research to find this 
                                                        #algorithm) for finding the greatest common divisor of two numeric inputs.

gcd <- function(a,b)  {
   
  while(a %% b = 0) {
    
     q <-  a %/% b
     r <-  a %% b
    
    a <-  q     
    b <-  r
  }
  print(a) 
}


#5. Write a function that takes two numeric inputs x and y and calculates x^2y + 2xy - xy^2

getCalculate <- function(x,y) {
   
  vResult <- (x^2*y) + (2*x*y) - (x*y^2)
  print(vResult)
  
}


#6. Read in the week-3-price-data.csv and week-3-make-model-data.csv files as data frames and then merge 
#them by the ModelNumber key. Leave the "all" parameters as their defaults. How many observations end 
#up in the result? Is this what you would have expected?

week3PriceData <- read.csv(file="C:/Anthony/School_CUNY/IS 607/Week3/week-3-price-data.csv", header=TRUE, sep=",")
week3ModelData <- read.csv(file="C:/Anthony/School_CUNY/IS 607/Week3/week-3-make-model-data.csv", header=TRUE, sep=",")

week3All <- merge(week3PriceData,week3ModelData ,by.week3PriceData="ModelNUmber",by.week3ModelData="ModelNUmber")

There are 27 rows of records at the end of the result. I would have expected 28 Records since the Price Data has 28 records 
and assuming that ModelNumber Key are valid keys.

#7. Use the data sets from the previous question, but this time merge them so that the rows from the price-data 
#table all appear, even if there is no match in the make-model table.

week3All <- merge(week3PriceData,week3ModelData ,by.week3PriceData="ModelNUmber",by.week3ModelData="ModelNUmber",
                  all = TRUE ,  all.week3PriceData = all, all.week3ModelData = all)


#8. Take your result from question 7 and subset it so that only the 2010 vehicles are included.

subset(week3All, Year == 2010)

#9. Take your result from question 7 and subset it so that only the red cars that cost more than $10,000 are 
#included.

subset(week3All, Color == "Red" & Price > 10000)

#10. Take your result from question 9 and subset it so that the ModelNumber and Color columns are removed.

aNewFrame$ModelNumber <- NULL
aNewFrame$Color <- NULL


#11. Write a function that takes as input a character vector and returns a numeric vector with the numbers of 
#characters in each of the elements in the original vector.

getNumVector <- function(v) {
  
  print(v)
  print(nchar(v) ) 
  
}


#12. Write a function that takes two character vectors of equal length and concatenates them element by 
#element with a space as the separator. Have the function die gracefully if the vectors are not the same length.

get2Vector <- function(v1, v2) {
  
  vCount1 <- length(v1)
  vCount2 <- length(v2)
  errMsg <- "Unmatched Vectors"
  newVector <- ""
  if (  vCount1 == vCount2  )  {
    for (i in 1:vCount1 ) {
      newVector[i]  <- paste( v1[i] , " " , v1[i] ) 
    }
    print(newVector)
  } else {
    print(errMsg)
  }
  
}

#13. Write a function that takes a character vector and returns the substring of three characters that begins with 
#the first vowel in the string. Have the function handle gracefully substrings where this isn't possible.

get2Vector <- function(v) {
  
  vCount1 <- length(v1)
  vCount2 <- length(v2)
  errMsg <- "Unmatched Vectors"
  newVector <- ""
  if (  vCount1 == vCount2  )  {
    for (i in 1:vCount1 ) {
      newVector[i]  <- paste( v1[i] , " " , v1[i] ) 
    }
    print(newVector)
  } else {
    print(errMsg)
  }
  
}

#14. Suppose you have a data frame where one column gives the month (in numeric format), the next gives the 
#day, and the third column gives the year. Use R to create such a data frame (by hand is fine) and then add a 
#fourth column with the date in date format.

m = c(1, 2, 3) 
d = c(11, 12, 13) 
y = c(2010, 2011, 2012) 

stringDate <- paste(m[1],"-",d[1],"-",y[1] )
stringDate <- paste(m[2],"-",d[2],"-",y[2] )
stringDate <- paste(m[3],"-",d[3],"-",y[3] )

df = data.frame(m[1], d[1], y[1] , as.Date(stringDate, "%m - %d - %Y"))   
df = data.frame(m[2], d[2], y[2] , as.Date(stringDate, "%m - %d - %Y"))   
df = data.frame(m[3], d[3], y[3] , as.Date(stringDate, "%m - %d - %Y"))   


#15. Illustrate the code necessary to take a string of MM-DD-YYYY format and convert it to a date.

stringDate <- c("01-01-1980", "12-31-2014")
dates <- as.Date(stringDate, "%m-%d-%Y")

#16. Illustrate the code necessary to take a date and extract the month of the date.
dates <- as.Date(stringDate, "%m-%d-%Y")
format(dates, "%b")


#17. Create a sequence of all of the dates from January 1, 2005, to December 31, 2014.
getAllDates <- function() {
  startDateStr <- c("01-01-2005")
  endDateStr <- c("12-31-2014")
  startDate <- as.Date(startDateStr, "%m-%d-%Y")
  endDate <- as.Date(endDateStr, "%m-%d-%Y")
  maxCtr <- difftime(endDate, startDate , units = "days")
  
  for (i in 1:maxCtr ) {
    stringDate <- as.Date(stringDate, "%m-%d-%Y") + 1
    print(stringDate ) 
    
  }
  
}
