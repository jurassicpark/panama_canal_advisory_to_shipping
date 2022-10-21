library(pdftools) 
library(reshape2)
library(data.table)
library(utils)
library(zoo)
library(stats)
library(stringr)

#Advisory to Shipping pdfs dated later than 2016 incorporate Neopanamax vessels and have a slightly different files than previous dated files.
#these scripts (01-Arrivals, 02-Transits, and 03-Bookings) are for parsing these pdfs

#DIRECTORY
#set working directory to path folder if necessary (setwd)
files <- list.files("C:\\Users\\computer\\Desktop\\Ester\\0\\Programming\\Advisory to Shipping\\2021-2017", pattern = "pdf$") 

#Function 
subsetting_function_t <- function(x){
  
  tx <- pdf_text(x)
  tx2 <- unlist(str_split(tx, "[\\r\\n]+"))
  tx3 <- str_split_fixed(str_trim(tx2), "\\s{2,}", 5)
  
  
  #ARRIVALS
  #subset file by Arrivals and convert to dataframe from matrix, array
  subsetfile_t <- as.data.frame(tx3[c(21:24),c(1:4)])
  
  #set column names (x1, Daily Average, High, Low)
  subsetfile_t_wColumns <- setNames(subsetfile_t, c("x1", "Total", "Daily Average", "Percentage"))
  
  #unpivot table and set new column names
  subsetfile_t_unpivot <- melt(subsetfile_t_wColumns, id = c("x1")) 
  
  #append category column (Arrivals)
  subsetfile_t_appendcat <- cbind(subsetfile_t_unpivot, "Oceangoing Transits")
  
  
  #DATE (YEARMON)
  #find date within file and split from other text at delimiter
  datesplit <- tx3[c(8),c(2)] %>% strsplit(split = " - ")
  
  #convert list to data frame and select month and year. format to yyyymm 
  dateconvert <- data.frame(datesplit)[2,]
  dateformat <- as.Date(as.yearmon(dateconvert))
  
  #append date column
  subsetfile_t_wDate <- cbind(subsetfile_t_appendcat, dateformat)
  
}


#LOOP
#run loop (apply function) through the file list against the function
list_t <- lapply(files, subsetting_function_t)

#manipulate output as a single dataframe
combinedDf_t <- rbindlist(list_t)

#set new column names
final_result_t <- setNames(combinedDf_t, c("Measure", "Attribute", "Value", "Category", "Date"))

#OUTPUT
#output table to csv
write.csv(final_result_t, file="C:\\Users\\computer\\Desktop\\Ester\\0\\Programming\\Advisory to Shipping\\2021-2017\\transits_combined.csv")
