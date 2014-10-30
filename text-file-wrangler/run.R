source('convertFile.R')
source('db.R')

## author: nelson.okello
## created: 28-Oct-2014
## 
## Initiates the converter processing
## Requires the source directory (contains the files to be converted)
## and the destination directory (where convereted files are to be pushed)
run <- function(dataSourceDir, dataDestDir, subscribersMappingFileName) {
        files <- dir(dataSourceDir, recursive = T)
        
        f_regex <- c('CRBBBC+', 'CRBBCA+', 'CRBBCR+', 'CRBBFA+', 'CRBBSI+', 'CRBBGI+', 'CRBBCI+', 'CRBBCE+')
        
        
        for(file in files) {
                file_path <- file.path(dataSourceDir, file)
                print(dirname(file_path))
                if(!file.info(file_path)$isdir) {
                        if(nchar(basename(file)) == 22) {
                                batchFileName <- paste(dataSourceDir, file, sep = '/')
                                headersFileName <- NULL
                                for(c_regex in f_regex) {
                                        if(length(grep(c_regex, basename(file), perl = T, value = T)) > 0) {
                                                headersFileName <- tolower(substr(basename(file), 5, 6))
                                                if(headersFileName == 'ce') headersFileName <- 'ic'
                                                headersFileName <- paste(headersFileName, 'headers', sep = '.')
                                                headersFileName <- paste(dataSourceDir, headersFileName, sep = '/')
                                                print(paste(batchFileName, headersFileName, sep = ': '))
                                                
                                                ## Run conversion
                                                convertFile(batchFileName, headersFileName, subscribersMappingFileName, dataDestDir)
                                        } 
                                }
                        } else {
                                msg <- 'Invalid file name'
                                msg_type <- 'FILE'
                                createConverterLog(file, msg, msg_type)       
                        } 
                }
        }
}