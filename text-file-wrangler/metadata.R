require(reshape2)
## author: nelson.okello
## created: 28-Oct-2014
##
## Constructs the file name.
## batchFileName = Name of the batch to be converted
## subsFileName = Name of the file that contains Bank Code to Subscriber ID mapping
## Returns the file name to be used after conversion
getFileName <- function(batchFileName, subsFileName) {
        print(paste('getFileName.[batchFileName]', batchFileName, sep = '; '))
        print(paste('getFileName.[subsFileName]', subsFileName, sep = '; '))
        
        ## Get metadata
        file_metadata <- getMetadata(batchFileName, subsFileName)
        out_fileName <- paste(file_metadata, collapse = '')
        print(out_fileName)
}

## For the specified Batch File Name and Subscriber Mapping File
## Returns a map of the file metadata
getMetadata <- function(fileName, subsFileName) { 
        print(paste('getMetadata.[fileName]', fileName, sep = '; '))
        print(paste('getMetadata.[subsFileName]', subsFileName, sep = '; '))
        
        fields <- colsplit(string = fileName, pattern = '\\.', names = c("date", "bankCode"))
        bank_code <- fields$bankCode
        
        ## Extract file name only
        filename_c <- strsplit(fileName,'\\/')
        filename_only <- sapply(filename_c, '[', c(2))
        # print(paste('File Name Only', filename_only, sep = ': '))
        
        ## Extract file type
        file_type <- substr(fileName, 5, 6)
        if(file_type == 'CE') {
                file_type <- 'IC'
        }
        # else file_type <- tmp_file_type

        # print(paste('File Type', file_type, sep = ': '))
        
        ## Extract file submission date
        submission_date <- substr(fileName, 7, 12)
        submission_date <- paste(submission_date, '01', sep = '')
        print(paste('Submission Date', submission_date, sep = ': '))
        d <- seq(as.Date(submission_date, '%Y%m%d'), length = 1, by = 'months')
        submission_date <- as.character(d - 1, format = '%Y%m%d')
        # print(paste('Submission Date', submission_date, sep = ': '))
        
        ## Extract file version number
        file_version <- substr(filename_only, 15, 17)
        # print(paste('Version', file_version, sep = ': '))
        
        ## Extract institution type
        institution_type <- substr(fileName, 4, 4)
        if(institution_type == 'B') institution_type <- 'BNK'
        if(institution_type == 'M') institution_type <- 'MFB'
        if(institution_type == 'D') institution_type <- 'DPF'
        # print(paste('Institution Type', institution_type, sep = ': '))
        
        ## Extract country code
        country_code <- 'KE'
        # print(paste('Country Code', country_code, sep = ': '))
        
        ## Extract bureau code
        bureau_code <- 'CRB'
        
        ## Extract subscriber id
        subscriber_info <- read.csv(subsFileName)
        part_subscriber_id <- subscriber_info[subscriber_info$bank_code == bank_code, 'subscriber_id']
        subscriber_id <- formatC(part_subscriber_id, width = 4, format = "d", flag = "0")
        
        file_metadata <- c('countryCode' = country_code, 'institutionType' = institution_type,
                           'bureauCode' = bureau_code, 'fileType' = file_type, 'subscriberId' = subscriber_id, 
                           'submissionDate' = submission_date,  'bankCode' = bank_code)
        
        return(file_metadata)
}