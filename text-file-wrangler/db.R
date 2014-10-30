require(RJDBC)

## author: nelson.okello
## 29-Oct-2014
## 
## Inserts a converter log to a DB table
createConverterLog <- function(filename, msg, msg_type) {
        
        ## Load config properties
        cfg <- getConfigs()
        
        
        ## Create DB Connection
        db_driver <- JDBC(cfg['db.driver.class'], cfg['db.driver.path'], NA)
        db_conn <- dbConnect(db_driver, cfg['db.url'], cfg['db.user'], cfg['db.password'])
        time_stamp <- as.numeric(Sys.time())
        
        ## Run the insert query
        dbSendUpdate(db_conn, 'insert into converter_log (
                     filename, message, message_type, log_time) 
                     values(?, ?, ?, ?)', filename, msg, msg_type, time_stamp)
        
        ## Retrieve the latest logs
        getConverterLogs(db_conn)
}

## Retrieve the latest converter logs
getConverterLogs <- function(con, limit = 10) {
        dbGetQuery(con, 'select * from converter_log order by log_time desc limit ?', limit)
}

## Load config properties
getConfigs <- function() {
        props <- read.csv('code/cfg.properties', header = F, sep = '=', 
                          strip.white = T, na.strings = "NA", stringsAsFactors = FALSE)
        names(props) <- c('prop', 'val')
        prop_vec <- setNames(props$val, props$prop)
        # print(prop_vec['db.host'])
}

time.sample <- function() {
        system.time({
                n <- 1000
                r <- numeric(n)
                for(i in 1:n) {
                        x <- rnorm(n)
                        r[i] <- mean(x)
                }
        })
}
