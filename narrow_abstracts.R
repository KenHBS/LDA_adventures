library(RMySQL)
###############################################################################
## 1) Read in the data for abstracts
## 2) Narrow down the abstracts in the database to observations with:
#### a) Data for abstract
#### b) Data for jel labels
#### c) Valid JEL codes
#### d) JEL codes from the AEA's description of JEL codes
## 3) Remove explicit JEL labels from abstracts
## 4) Remove empty space after labels
## 5) Create new database table and save results
###############################################################################

## 1) Read in the broad abstract set:
   con  <- dbConnect(MySQL(), host = "localhost", port = 3306, user = "root",
                     password = "qwertz", dbname = "basecamp")
   ## If this throws an error: start the SQL server using:
    ## system('mysql.server start')
   quer <- dbSendQuery(con, "SELECT * FROM abstract_set")
   df   <- fetch(quer, n = -1)
   dbClearResult(quer) 

   
## 2) Narrow down abstracts in the database:
  # a) Empty abstracts
  df <- df[df$abstract != "", ]
  
  # b) Scraped labels, that aren't JEL codes, but 3 digits (e.g 635 026)
  digit_inds    <- grepl("[0-9]{3}", df$labels, perl = TRUE)
  df            <- df[!digit_inds, ]
  
  # c) No labels (labels may be recovered from full PDF, 603 articles)
  df      <- df[which(df$labels != ""), ]
  
  # d) Only keep the docs with JEL-codes described online:
  load("/Users/Ken/Desktop/Basecamp/jel_offline.Rdata")
  
  splitted_labels <- str_split(df$labels, " ")
  invalid_jels    <- lapply(splitted_labels, function(x) !x %in% jel_labels)
  invalid_jels    <- which(sapply(invalid_jels, function(x) sum(x) > 0))
  
  df              <- df[-invalid_jels, ]
 
 
## 3) Remove the JEL codes from the abstracts, avoid perfect prediction:
  df$abstract   <- gsub("(\\W[A-Z][0-9]{1,2}\\W)", "", df$abstract, perl = T)
  df$abstract   <- gsub("JEL", "", df$abstract)
  df$abstract   <- gsub("[^a-zA-Z0-9_\\s]", "", df$abstract, perl = T)
    
  
## 4) Remove empty space after labels:
  df$labels <- gsub("\\s(?<=\\Z)", "", df$labels, perl = T)
  
## 5) Create new database table and save results
  dbSendQuery(con, "CREATE TABLE narrow_abstracts (
                      art_id VARCHAR(100),
                      abstract VARCHAR(3000),
                      labels VARCHAR(50));")
  dbSendQuery(con, "ALTER TABLE narrow_abstracts
                      ADD PRIMARY KEY (art_id);")
  types      <- list(art_id   = "VARCHAR(100)",
                     abstract = "VARCHAR(3000)",
                     labels   = "VARCHAR(50)")
  dbWriteTable(con, name = "narrow_abstracts", val = df, append = TRUE, 
               field.types = types, row.names = FALSE) 
  dbDisconnect(con)
