#### SCRAPE FROM AEA AGAIN:
library(rvest)
library(RMySQL)

## 0) Functions for this script
## 1) Identify all the journals in the association
## 2) Start the loop with:
##    ##    For every journal:
      ##    ##  Get all issues & For every issue:
      ##    ##    ##  Get links & DOI
      ##    ##    ##  Open database connection and save them there.


## 0) Functions for this script:
  get_issue_links <- function(basepage){
    read_html(basepage) %>%
        html_nodes("article") %>%
        html_nodes("a") %>%
        html_attr("href")
  }
  
  get_art_id <- function(issue_url){
    art_id <- read_html(issue_url) %>%
                  html_nodes("article") %>%
                  html_attr("id")
    art_id <- art_id[!(is.na(art_id))]
    # Get rid of the Forewords, Front Matters and Editor's Introduction:
    # (this works, because they have "i" at the end)
    word_length <- nchar(art_id)
    keep_inds   <- substr(art_id, word_length, word_length) != "i"
    art_id      <- art_id[keep_inds]
    
    art_links   <- paste0("https://www.aeaweb.org/articles?id=", art_id) 
    return(data.frame(oid = art_id, art_links = art_links))
  }
  


## 1) Identify all the journal in the association:
   abbrev_jrnl <- c("aer", "app", "pol", "mac", "mic", "jel", "jep")
   basepages    <-  paste0("https://www.aeaweb.org/journals/", 
                           abbrev_jrnl, "/issues")
   
## 2) Start the loop:
   journal_nr <- 1
   for(page in basepages){
     # Get the links of all issues:
     issue_links <- get_issue_links(page)
     issue_links <- paste0("https://www.aeaweb.org", issue_links)
     
     print(paste0("Started working on ", page))
     issue_nr <- 1
     # Get the article IDs & links
     for(issue in issue_links){
       art_url_id <- get_art_id(issue)
       closeAllConnections()
       
       con        <- dbConnect(MySQL(), host = "localhost", port = 3306, 
                               user = "root",
                               password = "qwertz", dbname = "basecamp")
       types      <- list(oid = "VARCHAR(50)", 
                          art_links = "VARCHAR(100)")
       dbWriteTable(con, name = "aea_links_ext", val = art_url_id, append = TRUE, 
                    field.types = types, row.names = FALSE) 
       dbDisconnect(con)
       print(paste0("Just did issue ", issue_nr, " from journal # ", journal_nr))
       issue_nr <- issue_nr + 1
       
       Sys.sleep(runif(n=1, 1, 2))
     }
     Sys.sleep(runif(n=1, 1, 2))
     journal_nr <- journal_nr + 1
   }
   