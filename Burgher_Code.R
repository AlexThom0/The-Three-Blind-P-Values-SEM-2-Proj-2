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



###### Figuring out training and test split stuff

set.seed(123)
n <- nrow(humancounts)

train_500 <- sample(1:n, size = floor(0.7 * n))
test_500  <- setdiff(1:n, train_500)


alpha <- 1

GPT_train500 <- GPTcounts[train_500, ]
human_train500 <- humancounts[train_500, ]

GPTtheta500 <- apply(GPT_train500, 2, sum)
GPTtheta500 <- (GPTtheta500 + alpha) / sum(GPTtheta500 + alpha)

humantheta500 <- apply(human_train500, 2, sum)
humantheta500 <- (humantheta500 + alpha) / sum(humantheta500 + alpha)



skip <- 50
min_length <- 100

row_id <- 0

all_errors_start <- c()
all_errors_end <- c()

for(i in 1:length(mixedtexts$texts)) {
  for(j in 1:length(mixedtexts$texts[[i]])) {
    
    row_id <- row_id + 1   # IMPORTANT: matches matrix rows
    
    if(!(row_id %in% test_500)) next
    
    y <- text_to_fw_indices(mixedtexts$texts[[i]][[j]], functionwords)
    
    cps <- cp_posterior(y, humantheta500, GPTtheta500, 
                        skip = skip, min_length = min_length)
    
    # estimates
    est1 <- cps[1, 1]
    est2 <- cps[1, 2]
    
    # truth
    true_vals <- as.numeric(strsplit(truecps$texts[[i]][[j]], ",")[[1]])
    true1 <- true_vals[1]
    true2 <- true_vals[2]
    
    # errors
    err1 <- abs(est1 - true1)
    err2 <- abs(est2 - true2)
    
    all_errors_start <- c(all_errors_start, err1)
    all_errors_end   <- c(all_errors_end, err2)
    
    print(paste(
      "Row:", row_id,
      "| Est:", est1, est2,
      "| True:", true1, true2,
      "| Err1:", err1,
      "| Err2:", err2
    ))
  }
}

mean_error_start <- mean(all_errors_start)
mean_error_end   <- mean(all_errors_end)

median_error_start <- median(all_errors_start)
median_error_end   <- median(all_errors_end)

print(paste("Mean Start Error:", round(mean_error_start, 2)))
print(paste("Mean End Error:", round(mean_error_end, 2)))

print(paste("Median Start Error:", median_error_start))
print(paste("Median End Error:", median_error_end))




























# Create folder map from human texts (works for all lengths)
# Because the folder structure is the same across all datasets
humantexts <- loadCorpusText("~/Desktop/SCS/SCS Final/essays/human/")
n_folders <- length(humantexts$texts)
folder_map <- rep(1:n_folders, times = sapply(humantexts$texts, length))

cat("Total folders:", n_folders, "\n")
cat("Total essays:", length(folder_map), "\n")

 # for reproducibility
test_folders <- sample(1:n_folders, size = floor(0.3 * n_folders))
train_folders <- setdiff(1:n_folders, test_folders)

cat("Training folders:", length(train_folders), " (", length(train_folders)/n_folders*100, "%)\n")
cat("Test folders:", length(test_folders), " (", length(test_folders)/n_folders*100, "%)\n")

# Get row indices
train_rows <- which(folder_map %in% train_folders)
test_rows <- which(folder_map %in% test_folders)

cat("\nTraining essays:", length(train_rows), " (", length(train_rows)/length(folder_map)*100, "%)\n")
cat("Test essays:", length(test_rows), " (", length(test_rows)/length(folder_map)*100, "%)\n")





# Function to analyze test set
analyze_test_set <- function(mixed_data, true_data, GPTtheta, 
                             test_folders, segment_name) {
  
  cat("\n", paste(rep("=", 60), collapse = ""))
  cat("\n", segment_name, "- TEST SET ONLY")
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  
  skip <- 50
  min_length <- 100
  total <- 0
  
  errors_start <- c()
  errors_end <- c()
  
  for(i in test_folders) {
    for(j in 1:length(mixed_data$texts[[i]])) {
      
      y <- text_to_fw_indices(mixed_data$texts[[i]][[j]], functionwords)
      
      cps <- tryCatch({
        cp_posterior(y, humantheta, GPTtheta, skip, min_length)
      }, error = function(e) NULL)
      
      if(is.null(cps) || nrow(cps) == 0) next
      
      est_start <- cps[1, 1]
      est_end <- cps[1, 2]
      
      true_vals <- as.numeric(strsplit(true_data$texts[[i]][[j]], ",")[[1]])
      
      errors_start <- c(errors_start, abs(est_start - true_vals[1]))
      errors_end <- c(errors_end, abs(est_end - true_vals[2]))
      
      total <- total + 1
      
      # Print progress every 50 essays
      if(total %% 50 == 0) {
        cat(sprintf("  Processed %d test essays...\n", total))
      }
    }
  }
  
  # Results
  cat("\n", paste(rep("-", 40), collapse = ""))
  cat("\nRESULTS FOR", segment_name)
  cat("\n", paste(rep("-", 40), collapse = ""), "\n")
  cat("Test essays analyzed:", total, "\n\n")
  
  cat("START POINT ERRORS:\n")
  cat(sprintf("  Mean: %.2f\n", mean(errors_start)))
  cat(sprintf("  Median: %.2f\n", median(errors_start)))
  cat(sprintf("  SD: %.2f\n", sd(errors_start)))
  cat(sprintf("  Range: [%.0f, %.0f]\n", min(errors_start), max(errors_start)))
  
  cat("\nEND POINT ERRORS:\n")
  cat(sprintf("  Mean: %.2f\n", mean(errors_end)))
  cat(sprintf("  Median: %.2f\n", median(errors_end)))
  cat(sprintf("  SD: %.2f\n", sd(errors_end)))
  cat(sprintf("  Range: [%.0f, %.0f]\n", min(errors_end), max(errors_end)))
  
  return(list(
    start = errors_start,
    end = errors_end,
    n = total
  ))
}

# Run for all three
cat("\n\nSTARTING EVALUATION ON TEST SET\n")
cat("================================\n")

results500 <- analyze_test_set(
  mixed_data = mixedtexts,
  true_data = truecps,
  GPTtheta = GPTtheta500,
  test_folders = test_folders,
  segment_name = "500-WORD SEGMENTS"
)

results200 <- analyze_test_set(
  mixed_data = mixedtexts200,
  true_data = truecps200,
  GPTtheta = GPTtheta200,
  test_folders = test_folders,
  segment_name = "200-WORD SEGMENTS"
)

results100 <- analyze_test_set(
  mixed_data = mixedtexts100,
  true_data = truecps100,
  GPTtheta = GPTtheta100,
  test_folders = test_folders,
  segment_name = "100-WORD SEGMENTS"
)




###### Figuring out training and test split stuff





# Calculate mean frequencies for each word
human_means <- colMeans(humancounts)
gpt_means <- colMeans(GPTcounts)

# Identify the indices of the most used words
top_indices <- order(human_means, decreasing = TRUE)[2:11]

# Create a comparison table
top_words_comparison <- data.frame(
  Word = functionwords[top_indices],
  Human_Avg = round(human_means[top_indices], 4),
  GPT_Avg = round(gpt_means[top_indices], 4)
)

print(top_words_comparison)





plot(human_means, gpt_means, 
     pch = 16, col = rgb(0, 0, 1, 0.5),
     xlab = "Human Word Frequency", 
     ylab = "GPT Word Frequency",
     main = "Word Frequency Comparison")
abline(a = 0, b = 1, col = "red", lty = 2) # The "Identity" line

# Label the words that are furthest from the line
text(human_means[top_indices], gpt_means[top_indices], 
     labels = functionwords[top_indices], pos = 3, cex = 0.8)




human_lengths <- rowSums(humancounts)
gpt_lengths <- rowSums(GPTcounts)

hist(human_lengths, col=rgb(0,0,1,0.5), main="Distribution of Function Word Counts", xlab="Number of Function Words")
hist(gpt_lengths, col=rgb(1,0,0,0.5), add=TRUE)
legend("topright", legend=c("Human", "GPT"), fill=c(rgb(0,0,1,0.5), rgb(1,0,0,0.5)))


hist(gpt_lengths)



# Combine data and run PCA
combined_counts <- rbind(humancounts, GPTcounts)
pca_res <- prcomp(combined_counts, scale. = TRUE)

# Plot the first two components
plot(pca_res$x[,1], pca_res$x[,2], 
     col = c(rep("blue", nrow(humancounts)), rep("red", nrow(GPTcounts))),
     pch = 16, cex = 0.6,
     xlab = "PC1", ylab = "PC2", 
     main = "Style Clustering (PCA)")
legend("topright", legend=c("Human", "GPT"), col=c("blue", "red"), pch=16)












