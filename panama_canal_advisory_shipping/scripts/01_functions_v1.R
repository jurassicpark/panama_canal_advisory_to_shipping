#########################################################
########## Panama Canal - Advisory to Shipping ##########
#########################################################
##### The main goal is to extract tables (Arrivals, Transits, Bookings)
##### from Advisories to Shipping pdfs. These files are provided on the
##### Panama Canal website at https://www.pancanal.com/common/maritime/advisories/

#########################################################
##### Script is to create functions that iteratively extracts tables from pdfs

##### Advisory to Shipping pdfs dated later than 2016 incorporate Neopanamax vessels
##### and have a slightly different file structure than previously dated files. 
##### These scripts are for pdfs date 2017 and later.

##### DIRECTORY #####
## set working directory to path folder if necessary (setwd)
files <- list.files("path_name", pattern = "pdf$") 

##### FUNCTIONS #####

#### ARRIVALS ####

subsetting_function_a <- function(x){
  
  tx <- pdf_text(x)
  tx2 <- unlist(str_split(tx, "[\\r\\n]+"))
  tx3 <- str_split_fixed(str_trim(tx2), "\\s{2,}", 5)
  
  #subset file by Arrivals and convert to dataframe from matrix, array
  subsetfile_a <- as.data.frame(tx3[c(16:19),c(1:4)])
  
  #set column names (x1, Daily Average, High, Low)
  subsetfile_a_wColumns <- setNames(subsetfile_a, c("x1", "Daily Average", "High", "Low"))
  
  #unpivot table and set new column names
  subsetfile_a_unpivot <- melt(subsetfile_a_wColumns, id = c("x1")) 
  
  #append category column (Arrivals)
  subsetfile_a_appendcat <- cbind(subsetfile_a_unpivot, "Arrivals")
  
  ### DATE (YEARMON)
  #find date within file and split from other text at delimiter
  datesplit <- tx3[c(8),c(2)] %>% strsplit(split = " - ")
  
  #convert list to data frame and select month and year. format to yyyymm 
  dateconvert <- data.frame(datesplit)[2,]
  dateformat <- as.Date(as.yearmon(dateconvert))
  
  #append date column
  subsetfile_a_wDate <- cbind(subsetfile_a_appendcat, dateformat)
  
}

### LOOP
#run loop (apply function) through the file list against the function
list_a <- lapply(files, subsetting_function_a)

### RESULT
#manipulate the output as a single dataframe
combinedDf_a <- rbindlist(list_a)
#set new column names
final_result_a <- setNames(combinedDf_a, c("Measure", "Attribute", "Value", "Category", "Date"))

#########################################################

#### TRANSITS ####

subsetting_function_t <- function(x){
  
  tx <- pdf_text(x)
  tx2 <- unlist(str_split(tx, "[\\r\\n]+"))
  tx3 <- str_split_fixed(str_trim(tx2), "\\s{2,}", 5)

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

### LOOP
#run loop (apply function) through the file list against the function
list_t <- lapply(files, subsetting_function_t)

### Result
#manipulate output as a single dataframe
combinedDf_t <- rbindlist(list_t)
#set new column names
final_result_t <- setNames(combinedDf_t, c("Measure", "Attribute", "Value", "Category", "Date"))

#########################################################

#### BOOKINGS ####

subsetting_function_b <- function(x){
  
  tx <- pdf_text(x)
  tx2 <- unlist(str_split(tx, "[\\r\\n]+"))
  tx3 <- str_split_fixed(str_trim(tx2), "\\s{2,}", 5)
  
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

### LOOP
#run loop (apply function) through the file list against the function
list_b <- lapply(files, subsetting_function_b)

### RESULT
#manipulate ouput as a single dataframe
combinedDf_b <- rbindlist(list_b)
#set new column names
final_result_b <- setNames(combinedDf_b, c("Measure", "Attribute", "Value", "Category", "Date"))
#remove special characters from the values columns 
final_result_b$Value <- gsub("[[:punct:]]","",final_result_b$Value)

#########################################################

