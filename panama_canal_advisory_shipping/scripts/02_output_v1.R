#########################################################
########## Panama Canal - Advisory to Shipping ##########
#########################################################
##### The main goal is to extract tables (Arrivals, Transits, Bookings)
##### from Advisories to Shipping pdfs. These files are provided on the
##### Panama Canal website at https://www.pancanal.com/common/maritime/advisories/

#########################################################
##### Script is to output extracted tables into csv files

##### OUTPUT #####
# output tables to csv
write.csv(final_result_a, file = "path_name\\arrivals_combined.csv")
write.csv(final_result_t, file = "path_name\transits_combined.csv")
write.csv(final_result_b, file = "path_name\\bookings_combined.csv")

