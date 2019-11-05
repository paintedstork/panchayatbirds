#README: How to extract locality information from eBird Sampling Data

#1/ Download Sampling event data from https://ebird.org/data/download . Name of the file will be like this ebd_sampling_relSep-2019.tar. This is an archive of multiple files and one of the file is the eBird sampling data for the entire world. 
#Note: Download will take a while as the file size for September 2019 is 3.6GB; and it will grow in the future. 

#2/ Untar this archive file using any standard utility into a folder named ebd_sampling_relSep-2019

#3/ In this folder, you will find ebd_sampling_relSep-2019.txt.gz which is another compressed file. Unzip this file to obtain ebd_sampling_relSep-2019.txt. This file will be more than 12GB in size.

#4/ For the next step, you need awk [so, install awk from internet]. On the command line or terminal, change the directory to ebd_sampling_relSep-2019. Run the below awk command. 

#awk 'BEGIN {FS="\t";}{print $13"\t"$15"\t"$16;}' ebd_sampling_relSep-2019.txt > ebd.txt

#What does this script do? We want only three columns from the sampling data - LOCALITY ID, LATITUDE and LONGITUDE. They are the 13th, 15th and 16th fields in this file. Awk will parse this tab (/t) separated file, line by line, pick only these columns and output to ebd.txt.  
#Note: Make appropriate change to the name of the file ebd_sampling_relSep-2019
#Note: ebd.txt (an uncompressed file) itself will be nearly 2GB in size. 
#Note: You can use perl or python to do the same job.

#5/ For the next step, you need to install R and libraries like data.table. Execute the following steps.

#setwd(“ebd_sampling_relSep-2019"")  					# This sets the current working directory. You may need to provide full path to reach this directory. 
getwd()   											            # Confirm whether you reached this working directory.

library(data.table)
library(tidyverse)

ebd <- fread ("ebd.txt", sep='\t', data.table=TRUE)		# Read the file into a data table
names(ebd) 										                        # Verify whether it has the three columns we wanted
nrow(ebd) 										                        # Verify it has lots of rows. 50478976 is the value for Sep 2019

# Next three steps are optimization. We do not want to store the data as characters or floating point numbers. The most optimal storage is as an integer
ebd$'LOCALITY ID' <- sub('.' , "" , ebd$'LOCALITY ID')   	# Remove the first character of the locality string. Which is “L”. We only need the number
ebd$LONGITUDE	<- ebd$LONGITUDE * 100000      		          # Multiply latitude with 100000. No data is lost.
ebd$LATITUDE	<- ebd$LATITUDE * 100000              	    # Multiply longitude with 100000. No data is lost. 

# Convert all three columns into integers
ebd$'LOCALITY ID' <- as.integer(as.character(ebd$'LOCALITY ID'))
ebd$LATITUDE 	    <- as.integer(as.character(ebd$LATITUDE))
ebd$LONGITUDE 	  <- as.integer(as.character(ebd$LONGITUDE))

ebd <- ebd [order(LATITUDE,LONGITUDE)]				        # Sort the data table with first latitude and then longitude. Compression works better in sorted files.

samples <- as.data.frame(table (ebd$'LOCALITY ID'))		# Table of sampling counts per location
colnames(samples) <- c("LOCALITY ID", "SAMPLES")		
samples$'LOCALITY ID' <- as.integer (as.character(samples$'LOCALITY ID'))
ebd <- unique(ebd, by="LOCALITY ID")					        # Remove duplicates
ebd <- inner_join(ebd, samples, by = "LOCALITY ID")		# Join # of samples with main file

saveRDS(ebd, "locality.RDS")							            # Save the datatable as an RDS. RDS file is automatically compressed. readRDS will load this file back. 
#### Size of this RDS file for September 2019 was 63.6MB	

