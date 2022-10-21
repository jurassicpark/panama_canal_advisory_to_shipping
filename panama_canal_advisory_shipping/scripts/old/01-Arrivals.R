library(pdftools) 
library(reshape2)
library(data.table)
library(utils)
library(zoo)
library(stats)
library(stringr)

#check which packages are actually needed 
#package NCmisc function list.functions.in.file(filename, alphabetic = TRUE)

#Advisory to Shipping pdfs dated later than 2016 incorporate Neopanamax vessels and have a slightly different files than previous dated files.
#these scripts (01-Arrivals, 02-Transits, and 03-Bookings) are for parsing these pdfs

#DIRECTORY
#set working directory to path folder if necessary (setwd)
files <- list.files("C:\\Users\\computer\\Desktop\\Ester\\0\\Programming\\Advisory to Shipping\\2021-2017", pattern = "pdf$") 


#Function 
subsetting_function_a <- function(x){
  
  tx <- pdf_text(x)
  tx2 <- unlist(str_split(tx, "[\\r\\n]+"))
  tx3 <- str_split_fixed(str_trim(tx2), "\\s{2,}", 5)
  
  
  #ARRIVALS
  #subset file by Arrivals and convert to dataframe from matrix, array
  subsetfile_a <- as.data.frame(tx3[c(16:19),c(1:4)])
  
  #set column names (x1, Daily Average, High, Low)
  subsetfile_a_wColumns <- setNames(subsetfile_a, c("x1", "Daily Average", "High", "Low"))
  
  #unpivot table and set new column names
  subsetfile_a_unpivot <- melt(subsetfile_a_wColumns, id = c("x1")) 
  
  #append category column (Arrivals)
  subsetfile_a_appendcat <- cbind(subsetfile_a_unpivot, "Arrivals")
  
  
  #DATE (YEARMON)
  #find date within file and split from other text at delimiter
  datesplit <- tx3[c(8),c(2)] %>% strsplit(split = " - ")
  
  #convert list to data frame and select month and year. format to yyyymm 
  dateconvert <- data.frame(datesplit)[2,]
  dateformat <- as.Date(as.yearmon(dateconvert))
  
  #append date column
  subsetfile_a_wDate <- cbind(subsetfile_a_appendcat, dateformat)
  
}


#LOOP
#run loop (apply function) through the file list against the function
list_a <- lapply(files, subsetting_function_a)

#manipulate the output as a single dataframe
combinedDf_a <- rbindlist(list_a)
#set new column names
final_result_a <- setNames(combinedDf_a, c("Measure", "Attribute", "Value", "Category", "Date"))

#OUTPUT
#output table to csv
write.csv(final_result_a, file="C:\\Users\\computer\\Desktop\\Ester\\0\\Programming\\Advisory to Shipping\\2021-2017\\arrivals_combined.csv")








