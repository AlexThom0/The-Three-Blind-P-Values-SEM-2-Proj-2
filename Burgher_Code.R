# Burgher Code

functionwords <- readLines("~/Desktop/SCS/SCS Final/wordfile200 (1).txt") #function word list
humantexts <- loadCorpusText("~/Desktop/SCS/SCS Final/essays/human/")

GPTtexts <- loadCorpusText("~/Desktop/SCS/SCS Final/essays/GPT500/")
mixedtexts <- loadCorpusText("~/Desktop/SCS/SCS Final/essays/mixed500/")
truecps <- loadCorpusText("~/Desktop/SCS/SCS Final/essays/changepoints500/")

GPTtexts100 <- loadCorpusText("~/Desktop/SCS/SCS Final/essays/GPT100/")
mixedtexts100 <- loadCorpusText("~/Desktop/SCS/SCS Final/essays/mixed100/")
truecps100 <- loadCorpusText("~/Desktop/SCS/SCS Final/essays/changepoints100/")

GPTtexts200 <- loadCorpusText("~/Desktop/SCS/SCS Final/essays/GPT200/")
mixedtexts200 <- loadCorpusText("~/Desktop/SCS/SCS Final/essays/mixed200/")
truecps200 <- loadCorpusText("~/Desktop/SCS/SCS Final/essays/changepoints200/")









## 500 length texts

skip <- 50
min_length <- 100
c = 0
for(i in 1:length(mixedtexts$texts))
  for(j in 1:length(mixedtexts$texts[[i]])){
    
    
    y <- text_to_fw_indices(mixedtexts$texts[[i]][[j]],functionwords)
    cps <- cp_posterior(y, humantheta, GPTtheta, skip = skip, min_length =min_length )
    est <- cps[1,][1:2]
    c = c + 1
    true <- truecps$texts[[i]][[j]]
    result <- paste('Est. change points are ', toString(est), ". True change point is", true, ". Count =",c)
    print(result)}

## 200 length texts 

alpha <- 1

GPTtheta200 <- apply(GPTcounts200,2,sum); 
GPTtheta200 <- (GPTtheta200 + alpha) / sum(GPTtheta200 + alpha)

humantheta <- apply(humancounts,2,sum); 
humantheta <- (humantheta + alpha) / sum(humantheta + alpha)

skip <- 50
min_length <- 100
c = 0
for(i in 1:length(mixedtexts200$texts))
  for(j in 1:length(mixedtexts200$texts[[i]])){
    
    
    y <- text_to_fw_indices(mixedtexts200$texts[[i]][[j]],functionwords)
    cps200 <- cp_posterior(y, humantheta, GPTtheta200, skip = skip, min_length =min_length )
    est200 <- cps200[1,][1:2]
    c = c + 1
    true200 <- truecps200$texts[[i]][[j]]
    result200 <- paste('Est. change points are ', toString(est200), ". True change point is", true200, ". Count =",c)
    print(result200)}


## 100 length texts 
GPTtheta100 <- apply(GPTcounts100,2,sum); 
GPTtheta100 <- (GPTtheta100 + alpha) / sum(GPTtheta100 + alpha)

humantheta <- apply(humancounts,2,sum); 
humantheta <- (humantheta + alpha) / sum(humantheta + alpha)

skip <- 50
min_length <- 100
c = 0
for(i in 1:length(mixedtexts100$texts))
  for(j in 1:length(mixedtexts100$texts[[i]])){
    
    
    y <- text_to_fw_indices(mixedtexts100$texts[[i]][[j]],functionwords)
    cps100 <- cp_posterior(y, humantheta, GPTtheta100, skip = skip, min_length =min_length )
    est100 <- cps100[1,][1:2]
    c = c + 1
    true100 <- truecps100$texts[[i]][[j]]
    result100 <- paste('Est. change points are ', toString(est100), ". True change point is", true100, ". Count =",c)
    print(result100)}




















# 1. Create a "Map" of which folder each essay belongs to
# This creates a vector like c(1, 1, 2, 2, 2, 3, ...) 
folder_map <- rep(1:length(humantexts$texts), times = sapply(humantexts$texts, length))

# 2. Pick which FOLDERS to use for Testing (30%)
set.seed(123)
n_folders <- length(mixedtexts$texts)
test_folder_idx <- sample(1:n_folders, size = floor(0.3 * n_folders))

# 3. Identify which ROWS in the flat count matrices belong to Training folders
# We keep rows where the folder_map is NOT in our test list
train_rows_idx <- which(!(folder_map %in% test_folder_idx))

# 4. Calculate Signatures using only those Training Rows
alpha <- 1
humantheta <- colSums(humancounts[train_rows_idx, ]) + alpha
humantheta <- humantheta / sum(humantheta)

GPTtheta <- colSums(GPTcounts[train_rows_idx, ]) + alpha
GPTtheta <- GPTtheta / sum(GPTtheta)

all_errors_start <- c()

for(i in test_folder_idx) { 
  for(j in 1:length(mixedtexts$texts[[i]])) {
    
    y <- text_to_fw_indices(mixedtexts$texts[[i]][[j]], functionwords)
    cps <- cp_posterior(y, humantheta, GPTtheta, skip = 50, min_length = 100)
    
    est1 <- cps$c1[1]
    
    # Matching the truth
    true_vals <- as.numeric(strsplit(truecps$texts[[i]][[j]], ",")[[1]])
    
    error <- abs(est1 - true_vals[1])
    all_errors_start <- c(all_errors_start, error)
    
    print(paste("Folder:", i, "Essay:", j, "| Error:", error))
  }
}




























