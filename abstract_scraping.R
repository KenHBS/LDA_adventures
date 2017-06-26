setwd("/Users/Ken/Desktop/Basecamp/txtdata/")
library(rvest)
library(RMySQL)

###############################################################################

## 0) Define functions for the script
## 1) Identify ungrabbed links from database
## 2) Scrape the links for JEL codes and abstracts
## 3) Recover some missing JEL codes from abstracts (regex)

###############################################################################


###############################################################################
## 0) Define functions for the script:
###############################################################################

  grab_url <- function(somelink){
    webpage <- try(read_html(somelink))
    if("try-error" %in% class(webpage)){
      # If some error occurs: 1) check internet connectivity:
      inet_check <- utils::nsl("www.google.com")
      i <- 1
      while(is.null(inet_check)){
        inet_check <- utils::nsl("www.google.com")
        i <- i+1
        paste0("Internet seems to be gone, wait 10 minutes. Trial # ", i)
        Sys.sleep(600)
      } ## Only get out of this loop when internet connection returns
      webpage <- try(read_html(somelink))
      if("try-error" %in% class(webpage)){
        return("Invalid URL")
      }
    }
    return(webpage)
  }
  
  get_abstract <- function(webpage){
    # From get the abstract:
    abstract   <- html_nodes(webpage, xpath = abs_xpath) %>%
      html_text()
    if(length(abstract) > 0){
      return(gsub("\n|Abstract|\t", "", abstract))
    } else{
      return("")
    }
  }
  
  get_labels <- function(webpage){
    # From webpage, get the JEL labels:
    label_vec <- html_nodes(webpage, xpath = jel_xpath) %>%
                    html_text()
    paste(label_vec, collapse = " ")
  }
  
  get_oid <- function(webpage){
    # From the webpage, get the Digital Object Identifier
    title      <- html_nodes(webpage, xpath = doi_xpath) %>%
      html_text()
    strsplit(title, " ")[[1]][2]
  }
  
  killDbConnections <- function (){
    all_cons <- dbListConnections(MySQL())
    for(conns in all_cons){ 
      dbDisconnect(conns)
    }
    print(paste(length(all_cons), " connections killed."))
  }
  
  
###############################################################################
## 1) Open the database, read in the links, compare to existing database
###############################################################################
  
  killDbConnections()
  con          <- dbConnect(MySQL(), host = "localhost", port = 3306, user = "root",
                             password = "qwertz", dbname = "basecamp")
  quer         <- dbSendQuery(con, "SELECT * FROM aea_links_ext")
  all_links    <- fetch(quer, n = -1)
  dbClearResult(quer)
  
  quer         <- dbSendQuery(con, "SELECT * FROM abstract_set")
  gotten_links <- fetch(quer, n = -1)
  dbClearResult(quer)
  
  links_togo   <- all_links[!(all_links$oid %in% gotten_links$art_id), ]
 
  
###############################################################################
## 2) Scrape the links for JEL codes and abstracts
###############################################################################
  
  ## Constant values for inside the loop:
  abs_xpath  <- "//section[contains(@class, 'article-information abstract')]"
  jel_xpath  <- "//strong[contains(@class, 'code')]"
  doi_xpath  <- "//span[contains(@class, 'doi')]"
  types      <- list(art_id   = "VARCHAR(50)", 
                     labels   = "VARCHAR(50)",
                     abstract = "VARCHAR(1000)")
  
  ## Some tracking and printing feedback:
  tot       <- nrow(links_togo) 
  it        <- 1
  datebegin <- Sys.time()

  ## The actual scraping loop:
  while(dim(links_togo)[1] != 0){
    current_link <- links_togo[1, ]
    html_doc     <- try(grab_url(current_link[1,2]))
    
    # If the current_link is not a valid url, remove it from links_togo and proceed
    while("try-error" %in% class(html_doc) | ("Invalid URL" %in% html_doc)){
      links_togo   <- links_togo[-1, ]
      current_link <- links_togo[1, ]
      html_doc     <- try(grab_url(current_link[1,2]))
    }
    
    new_entry <- data.frame(art_id   = get_oid(html_doc), 
                            abstract = get_abstract(html_doc),
                            labels   = get_labels(html_doc)) 
    
    # Add the new_entry to the database:
    con        <- dbConnect(MySQL(), host = "localhost", port = 3306, 
                            user = "root",
                            password = "qwertz", dbname = "basecamp")
    dbWriteTable(con, name = "abstract_set", val = new_entry, append = TRUE, 
                 field.types = types, row.names = FALSE) 
    dbDisconnect(con)
    
    # Remove the current link from links_togo:
    links_togo <- links_togo[-1, ]
    # Some feedback for every iteration:
    print(paste0("Scraped ", it, "/", tot, " article infos"))
    it <- it + 1
    print(Sys.time() - datebegin)
    Sys.sleep(runif(n = 1, 0, 3))
  }  # End of the scraping loop

  
###############################################################################  
## 3) Recover some missing JEL codes:
###############################################################################  
  
  ## Recover if mentioned in abstract, based on regex
     # Load abstract_set table, which we just filled with scrapings:
     con         <- dbConnect(MySQL(), host = "localhost", port = 3306, 
                              user = "root",
                              password = "qwertz", dbname = "basecamp")
     df          <- dbGetQuery(con, "SELECT * FROM abstract_set")
     
     # Get the articles with abstract, without labels:
     no_labels   <- df[ (df$abstract != "") & (df$labels == ""), ]
     
     # See which ones have the JEL labels in the abstract and get them:
     jel_reg     <- "[A-Z][0-9]{1,2}(?=(,|\\)))"
     jel_matches <- gregexpr(jel_reg, text = no_labels$abstract, perl = TRUE)
     jel_dirty   <- regmatches(no_labels$abstract, jel_matches)
     jel_dirty   <- lapply(jel_dirty, function(x){paste(x, collapse = " ")}) 
     
     # Prepare jel_clean for integration in other table:
     no_labels$labels                       <- unlist(jel_dirty)
     
     # Insert the newly labeled entries into the database:
     newly_labeled <- no_labels[no_labels$labels != "",]
     sql           <- sprintf("UPDATE abstract_set SET labels = '%s' where art_id = '%s'", 
                              newly_labeled$labels, newly_labeled$art_id)
     for(i in 1:length(sql)){
       rs    = dbSendQuery(con, sql[i])
       dbClearResult(rs)
     }  
