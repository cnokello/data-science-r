## Reads the file from the specified location
## Record by record
read.file <- function(filename, headerFileName, outFileName) {
        f_header <- getHeader(headerFileName)
        rec_num <- 1
        header_written <- FALSE
        
        c_in <- file(filename, 'r')
        c_out <- file(paste(filename, 'rds', sep = '.'), 'w')
        
        while(TRUE) {
                r1 <- readLines(c_in, n = 1)
                if(length(r1) == 0) {
                        print("reached the end")
                        break
                } else {
                        ## Create the record data frame
                        f_record <- data.frame(f_header, extract.general(r1))
                        colnames(f_record) <- c('attribute', 'value')
          
                        print(formatOutput(rec_num, f_record$value))
                        if(header_written == FALSE) {
                                writeLines(formatOutput(rec_num, f_record$attribute, TRUE), c_out)
                                header_written <- TRUE
                        } else {
                                writeLines(formatOutput(rec_num, f_record$value), c_out)
                                #print(as.matrix(f_record$value, nrow = 1))
                                rec_num <- rec_num + 1        
                        }  
                        
                }
        }
        
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
extract.general <- function(line) {
        fields <- strsplit(line, '\\|')
        trimmed <- lapply(fields, trim.all)
}