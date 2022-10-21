library(pdftools) 
library(xlsx)
library(openxlsx)
library(dbplyr)
library(reshape2)
library(data.table)
library(tidyverse)
library(zoo)

files <- list.files("C:\\Users\\computer\\Desktop\\Ester\\0\\Programming\\Advisory to Shipping\\2021-2017", pattern = "pdf$") 

#Function 
subsetting_function_b <- function(x){
  
  tx <- pdf_text(x)
  tx2 <- unlist(str_split(tx, "[\\r\\n]+"))
  tx3 <- str_split_fixed(str_trim(tx2), "\\s{2,}", 5)
  
  
  #ARRIVALS
  #subset file by Arrivals and convert to dataframe from matrix, array
  subsetfile_b <- as.data.frame(tx3[c(26:30),c(1:4)])
  
  #set column names (x1, Daily Average, High, Low)
  subsetfile_b_wColumns <- setNames(subsetfile_b, c("x1", "Available", "Used", "Percentage"))
  
  #unpivot table on x1 column
  subsetfile_b_unpivot <- melt(subsetfile_b_wColumns, id = c("x1")) 
  
  #append category column (Arrivals)
  subsetfile_b_appendcat <- cbind(subsetfile_b_unpivot, "Booking Slots")
  
  
  #DATE (YEARMON)
  #find date within file and split from other text at delimiter
  datesplit <- tx3[c(8),c(2)] %>% strsplit(split = " - ")
  
  #convert list to data frame and select month and year. format to yyyymm 
  dateconvert <- data.frame(datesplit)[2,]
  dateformat <- as.Date(as.yearmon(dateconvert))
  
  #append date column
  subsetfile_b_wDate <- cbind(subsetfile_b_appendcat, dateformat)
  
  
}


#LOOP
#run loop (apply function) through the file list against the function
list_b <- lapply(files, subsetting_function_b)

#manipulate ouput as a single dataframe
combinedDf_b <- rbindlist(list_b)

#set new column names
final_result_b <- setNames(combinedDf_b, c("Measure", "Attribute", "Value", "Category", "Date"))

#remove special characters from the values columns 
final_result_b$Value <- gsub("[[:punct:]]","",final_result_b$Value)


#OUTPUT
#output table to csv
write.csv(final_result_b, file="C:\\Users\\computer\\Desktop\\Ester\\0\\Programming\\Advisory to Shipping\\2021-2017\\bookings_combined.csv")

