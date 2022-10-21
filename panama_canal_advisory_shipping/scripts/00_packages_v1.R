#########################################################
########## Panama Canal - Advisory to Shipping ##########
#########################################################
##### The main goal is to extract tables (Arrivals, Transits, Bookings)
##### from Advisories to Shipping pdfs. These files are provided on the
##### Panama Canal website at https://www.pancanal.com/common/maritime/advisories/

#########################################################
##### Script is to load packages

library(pdftools) 
library(reshape2)
library(data.table)
library(utils)
library(zoo)
library(stats)
library(stringr)
