####  
## 0) Load the data & define functions
## 1) Set the hyper-priors for LDA
## 2) Set the initialization values for LDA
## 3) Set up the Gibbs collapsed sampling

## 0) Load the data & define function:
  source("LDA_start_state.R")

  # Function for textmining preparation
  tm_prep <- function(x){
    x <-  tolower(x)                                       %>%
      removePunctuation(preserve_intra_word_dashes = TRUE) %>%
      removeNumbers()                                      %>%
      removeWords(c(stopwords("en"), "\\s", "\\r"))        %>%
      stemDocument()
    return(x[x != ""])
  }
  
  # Only keep words that occur less than 90% of time & more than 1%:
  keep_words <- function(Nwords, 
                         lb = length(Nwords) * 0.01,
                         ub = length(Nwords) * 0.90){
    tab  <- table(unlist(Nwords))
    tab  <- tab[nchar(names(tab)) > 2]
    tab  <- tab[(tab > lb) & (tab < ub)]
    inds <- lapply(Nwords, function(x) x %in% names(tab))
    return(Map(`[`, Nwords, inds))
  }
  
  # Adjust the sample() function to avoid unexpected behaviour with length = 1:
  sample.vec <- function(x, ...){
    x[sample(length(x), ...)]
  }
  
## 1) Set the hyper-priors for LDA 
  # number of potential labels:
  K <- ncol(Cd)
  
  # alpha: dirichlet parameter for topic distribution
    # Based on the frequencies in the corpus, added 5 for all labels
    label_freqs = table(unlist(labelings)) 

    label_freqs = label_freqs[grepl("[A-Z]", names(label_freqs))]
    label_freqs = label_freqs[order(names(label_freqs))]
  
  alpha = label_freqs + 5
  
  

## 2: Setup requirements for collapsed gibbs sampler:
  Nwords = strsplit(df$abstract, " ")
  Nwords = lapply(Nwords, tm_prep)   
  V      = keep_words(Nwords)

  all_words = unique(unlist(V))[order(unique(unlist(V)))]
  
  # V are all words from abstracts that survived after filtering
  w_n = unlist(V)
  
  # beta: dirichlet parameter for word distr
  beta = rep(1, length(unique(w_n)))
  
  # Get the document nr for every word:
  doc_lengths = sapply(V, length)
  d_n         = rep(1:length(V), doc_lengths)
  
  # Get the label assignment for every word:
  labelings   = lapply(labelings, function(x) which(names(alpha) %in% x))
  z_n         = Map(sample.vec, labelings, doc_lengths, replace = T)
  z_n         = unlist(z_n)
  
  ## 3) Set up the collapsed Gibbs sampling:
  ## Ct:
  Pre_Ct <- data.frame(table(z_n, w_n))
  Ct     <- matrix(Pre_Ct$Freq, 
                   ncol = length(unique(w_n)), 
                   nrow = length(unique(z_n)))
  colnames(Ct) <- unique(w_n)[order(unique(w_n))]
  rownames(Ct) <- unique(z_n)[order(unique(z_n))]
  
  ## Remaining helpers in t loop:
  P    <- rep(0, K)  
  pi_n <- sample(length(w_n))
  V    <- length(unique(w_n)) 
  
  start_time <- Sys.time()
  for(i in 1:500){
    for(n in 1:length(w_n)){
      iter <- pi_n[n]
      
      w_pick  <- which(w_n[iter] == dimnames(Ct)[[2]])
      t_pick  <- z_n[iter] 
      d_pick  <- d_n[iter]
      
      # There's a mistake here, this should reassign a present word!
      Cd[d_pick, t_pick]  <- Cd[d_pick, t_pick] - 1
      Ct[t_pick, w_pick]  <- Ct[t_pick, w_pick] - 1
      
      # Check all parts of the loop for dimension sensibility
      for(k in 1:K){
        part_one_a <- (Ct[k, w_pick] + beta[1]) 
        part_one_b <- V + sum(Ct[k, ])
        part_one   <- part_one_a / part_one_b
        
        # There's another mistake here!
        part_two_a <- Cd[d_pick, k] + alpha[t_pick]
        part_two_b <- K * alpha[t_pick] + sum(Cd[d_pick, ])
        part_two   <- part_two_a / part_two_b
        
        P[k] <- part_one * part_two
      }
      
      P            <- P / sum(P)
      t_pick       <- sample(x = 1:K, prob = P, size = 1)
      z_n[iter]    <- t_pick
      
      Cd[d_pick, t_pick]  <- Cd[d_pick, t_pick] + 1
      Ct[t_pick, w_pick]  <- Ct[t_pick, w_pick] + 1 
      
      print(paste0("Updated word nr. ", n, " out of ", length(w_n), 
                   " (iteration nr. ", i, ")"))
      print(Sys.time() - start_time)
    }  # End of all words loop
  }  # End of 500 iterations through all words
  

  
  
