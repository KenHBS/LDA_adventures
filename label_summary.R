library(RMySQL)
library(stringr)

## Get the data in R:
  con          <- dbConnect(MySQL(), host = "localhost", port = 3306, user = "root",
                            password = "qwertz", dbname = "basecamp")
  quer <- dbSendQuery(con, "SELECT * FROM abstract_set")
  df   <- fetch(quer, n = -1)
  dbClearResult(quer)

### Three sources of wrongness:
  # 1) Empty abstracts
  valid_df <- df[df$abstract != "", ]
  
  # 2) Scraped labels, that aren't JEL codes, but 3 digits (e.g 635 026)
  digit_inds    <- grepl("[0-9]{3}", valid_df$labels, perl = TRUE)
  valid_df      <- valid_df[!digit_inds, ]
  
  # 3) No labels (labels may be recovered from full PDF, 603 articles)
  valid_df      <- valid_df[which(valid_df$labels != ""), ]

  
  
### SUMMARIZE THE JEL CODES IN THE 5434 USEFUL ABSTRACTS:
  label_summary <- table(valid_df$labels)
  
  length(label_summary)   # 3997  unique label combinations
  max(label_summary)      # 31    most common label combination occurs 31 times
  
  split_labels <- str_split(valid_df$labels, " ")
  mean(sapply(split_labels, length)) # 3.68 labels per document on average
  
  bag_of_labels <- unlist(split_labels)
  length(unique(bag_of_labels))  # 661 unique labels
 
  sum_per_label <- table(bag_of_labels)
  mean(sum_per_label)     # Every label occurs on average 25.91 times in corpus     
  sum_per_label[(sum_per_label == max(sum_per_label))]   # E32 occurs 340 times 
  # in the corpus: Business Fluctuations and Cycles
  
  # Most common General Category:
  general_cats <- table(gsub("[^A-Z]", "", bag_of_labels))
     # Y:	Miscellaneous Categories  least: 3 times
     # D: Microeconomics    most: 2950 times 
  sum(grepl("D", valid_df$labels))  # D present in 1896 documents
  

### 
  