library(RMySQL)
library(tm)
library(stringr)

###############################################################################
##  0) Import data to session
##       a) For part 1) & 2)
##       b) For part 3)
##  2) Create topics-per-document initialization
##  3) Create word-per-topic initialization
###############################################################################


###############################################################################
## 0) Data import to session
###############################################################################
## a) For part 1) & 2)
con <- dbConnect(MySQL(), host = "localhost", port = 3306, user = "root",
                 password = "qwertz", dbname = "basecamp")
    ## system('mysql.server start')
df  <- dbGetQuery(con, "SELECT * FROM narrow_abstracts")
dbDisconnect(con)

## b) For part 3)
#load("/Users/Ken/Desktop/Basecamp/jel_offline.Rdata")


###############################################################################
## 2) Topic count per document, initialization: theta_prior
###############################################################################
labelings   <- str_split(df$labels, " ")
label_set   <- unique(unlist(labelings))
label_set   <- label_set[order(label_set)]

#jel_labels  <- union(jel_labels, unlist(labelings))
#jel_labels  <- jel_labels[!(jel_labels %in% c("", " "))]

Cd <- sapply(labelings, function(x) (label_set %in% x) * 1)

rownames(Cd) <- label_set
colnames(Cd) <- paste0("doc_", 1:nrow(df))
Cd <- t(Cd)

#############################################################################
## 3) Word count per topic, initialization: phi_prior
#############################################################################

# Get all words from abstracts that 'survive'  tm-prep and keep_words
#  Nwords = strsplit(df$abstract, " ")
#  Nwords = lapply(Nwords, tm_prep)   
#  V      = keep_words(Nwords)

#  all_words <- all_words[order(unlist(unique(V)))]

##  real_names  <- !grepl("\\s", names(jel_stemmed), perl = TRUE)
##  jel_stemmed <- jel_stemmed[real_names]
#  phi_prior   <- sapply(jel_stemmed, function(x) (all_words %in% x) * 1)
  
#  rownames(phi_prior) <- all_words
#  phi_prior           <- apply(phi_prior, 2, function(x) x / sum(x))

### Alternative to Ct:

  
  

  