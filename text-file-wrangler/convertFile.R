source('metadata.R')

## author: nelson.okello
## created: 28-Oct-2014
##
## Reads the file from the specified location
## Record by record
convertFile <- function(batchFileName, headerFileName, subscribersMappingFileName, dest) {
        #print(paste('convertFile.[Batch File Name]', batchFileName, sep = ': '))
        #print(paste('convertFile.[Header File Name]', headerFileName, sep = ': '))
        #print(paste('convertFile.[Subscriber Mapping File Name]', subscribersMappingFileName, sep = ': '))
        
        f_header <- getHeader(headerFileName)
        rec_num <- 1
        global_rec_num <- 1
        header_written <- FALSE
        
        # Create destination directory
        dest_dir_name <- sub(substr(basename(batchFileName), 1, 6), 
                             substr(basename(batchFileName), 1, 4), basename(batchFileName))
        dest_dir <- paste(dest, dest_dir_name, sep = '/')
        dir.create(dest_dir, showWarnings = F)
        dest <- dest_dir
        
        
        # print(paste('File Name', basename(batchFileName), sep = ': ')
        out_filename <- getFileName(basename(batchFileName), subscribersMappingFileName)
        out_file_path <- paste(dest, out_filename, sep = '/')
        error_file_path <- paste(out_file_path, 'error', sep = '.')
        
        c_in <- file(batchFileName, 'r')
        c_out <- file(paste(out_file_path, 'rds', sep = '.'), 'w')
        c_error <- file(paste(error_file_path, 'rds', sep = '.'), 'w')
        
        while(TRUE) {
                r1 <- readLines(c_in, n = 1)
                if(length(r1) == 0) {
                        print("reached the end")
                        break
                } else {
                        ## Extract record values 
                        f_values <- unlist(extract.recordValues(r1))
                        
                        ## Mark for processing only if the number of columns match what is expected
                        ## Number of Header (Attribute) Names MUST be EQUAL to 
                        ## the number of values extracted
                        if(length(f_header) == length(f_values)) {
                                max.len <- max(length(f_header), length(f_values))
                                f_values <- c(f_values, rep('', max.len - length(f_values)))
                                print(f_values)
                        
                        
                                f_record <- data.frame(f_header, f_values)
                                colnames(f_record) <- c('attribute', 'value')
          
                                ## Check if the header is already written. If not, write it
                                ## then mark it as such
                                ## If header is already written, write out the record line
                                ## along with the record number, then increment 
                                ## both record number (VALID records only) and global record number
                                ## (TOTAL number of lines processed so far)
                                if(header_written == FALSE) {
                                        writeLines(formatOutput(rec_num, f_record$attribute, TRUE), c_out)
                                        writeLines(formatOutput(rec_num, f_record$value), c_out)
                                        
                                        header_written <- TRUE
                                        rec_num <- rec_num + 1        
                                        global_rec_num <- global_rec_num + 1
                                        
                                } else {
                                        writeLines(formatOutput(rec_num, f_record$value), c_out)
                                        rec_num <- rec_num + 1        
                                        global_rec_num <- global_rec_num + 1
                                }  
                                
                        } else {
                                max.len <- max(length(f_header), length(f_values))
                                f_values <- c(f_values, rep('', max.len - length(f_values)))
                                print(f_values)
                                
                                f_record <- data.frame(f_header, f_values)
                                colnames(f_record) <- c('attribute', 'value')
                                
                                writeLines(formatOutput(global_rec_num, f_record$value), c_error)
                                global_rec_num <- global_rec_num + 1
                        }
                        
                }
        }
        
        close(c_error)
        close(c_out)
        close(c_in)
        
}

## Retrieves header from the specified file
getHeader <- function(headerFileName) {
        f_header <- scan(headerFileName, what = '')
}

formatOutput <- function(rec_num, record, header = FALSE) {
        out_record <- paste(record, collapse = "|")
        if(header == TRUE) {
                num_out_record <- paste('rec_num', out_record, sep = '|')
        } else {
                num_out_record <- paste(rec_num, out_record, sep = '|')        
        }
        
        num_out_record
}

## Trims the specified string
## removing both leading and trailing white spaces
trim.all <- function(x) {
        gsub("^\\s+|\\s+$", "", x)
}

## Splits the specified string 
## to return individual field values as a vector
extract.recordValues <- function(line) {
        fields <- strsplit(line, '\\|', perl = TRUE)
        trimmed <- lapply(fields, trim.all)
}