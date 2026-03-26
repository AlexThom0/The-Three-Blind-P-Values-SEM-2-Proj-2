#### Code.R ####
source("~/Desktop/SCS Final Project/assignment2-functions.R")


#### Extracting the stuff that needs extracted #####


# 500 word ChatGPT segment length extractions
functionwords <- readLines("~/Desktop/SCS/SCS Final/wordfile200 (1).txt") 
humantexts <- loadCorpusText("~/Desktop/SCS/SCS Final/essays/human/")

GPTtexts <- loadCorpusText("~/Desktop/SCS Final Project//essays/GPT500/")
mixedtexts <- loadCorpusText("~/Desktop/SCS Final Project/essays/mixed500/")
truecps <- loadCorpusText("~/Desktop/SCS Final Project/essays/changepoints500/")

K <- length(functionwords) + 1
GPTcounts <- NULL
humancounts <- NULL
mixedcounts <- NULL
truechangepoints <- NULL

for (i in 1:length(GPTtexts$texts)) {
  for (j in 1:length(GPTtexts$texts[[i]])) {
    GPTcounts <- rbind(GPTcounts,tabulate(text_to_fw_indices(GPTtexts$texts[[i]][[j]],functionwords),nbins=K))
  }
}

for (i in 1:length(mixedtexts$texts)) {
  print(i)
  for (j in 1:length(mixedtexts$texts[[i]])) {
    mixedcounts <- rbind(mixedcounts,tabulate(text_to_fw_indices(mixedtexts$texts[[i]][[j]],functionwords),nbins=K))
  }
}

for (i in 1:length(humantexts$texts)) {
  print(i)
  for (j in 1:length(humantexts$texts[[i]])) {
    humancounts <- rbind(humancounts,tabulate(text_to_fw_indices(humantexts$texts[[i]][[j]],functionwords),nbins=K))
  }
}

for (i in 1:length(truecps$texts)) {
  print(i)
  for (j in 1:length(truecps$texts[[i]])) {
    truechangepoints <- rbind(truechangepoints,  as.numeric(strsplit(truecps$texts[[i]][[j]],',')[[1]]))
  }
}

# Saving we don't have to rerun
save(humancounts, mixedcounts, GPTcounts, truechangepoints, functionwords, 
     file = "LLM_Project_Data.RData")




# Doing same for 100 word ChatGPT segment lengths
GPTtexts100 <- loadCorpusText("~/Desktop/SCS/SCS Final/essays/GPT100/")
mixedtexts100 <- loadCorpusText("~/Desktop/SCS/SCS Final/essays/mixed100/")
truecps100 <- loadCorpusText("~/Desktop/SCS/SCS Final/essays/changepoints100/")

K <- length(functionwords) + 1
GPTcounts100 <- NULL
humancounts <- NULL
mixedcounts100 <- NULL
truechangepoints100 <- NULL

for (i in 1:length(GPTtexts100$texts)) {
  for (j in 1:length(GPTtexts100$texts[[i]])) {
    GPTcounts100 <- rbind(GPTcounts100,tabulate(text_to_fw_indices(GPTtexts100$texts[[i]][[j]],functionwords),nbins=K))
  }
}

for (i in 1:length(mixedtexts100$texts)) {
  print(i)
  for (j in 1:length(mixedtexts100$texts[[i]])) {
    mixedcounts100 <- rbind(mixedcounts100,tabulate(text_to_fw_indices(mixedtexts100$texts[[i]][[j]],functionwords),nbins=K))
  }
}

for (i in 1:length(humantexts$texts)) {
  print(i)
  for (j in 1:length(humantexts$texts[[i]])) {
    humancounts <- rbind(humancounts,tabulate(text_to_fw_indices(humantexts$texts[[i]][[j]],functionwords),nbins=K))
  }
}

for (i in 1:length(truecps100$texts)) {
  print(i)
  for (j in 1:length(truecps100$texts[[i]])) {
    truechangepoints100 <- rbind(truechangepoints100,  as.numeric(strsplit(truecps100$texts[[i]][[j]],',')[[1]]))
  }
}

# Saving again so we don't need to rerun
save(humancounts, mixedcounts100, GPTcounts100, truechangepoints100, functionwords, 
     file = "counts_100.RData")





# Lastly, doing the same for the 200 word long ChatGPT segment lengths
GPTtexts200 <- loadCorpusText("~/Desktop/SCS/SCS Final/essays/GPT200/")
mixedtexts200 <- loadCorpusText("~/Desktop/SCS/SCS Final/essays/mixed200/")
truecps200 <- loadCorpusText("~/Desktop/SCS/SCS Final/essays/changepoints200/")


K <- length(functionwords) + 1
GPTcounts200 <- NULL
humancounts <- NULL
mixedcounts200 <- NULL
truechangepoints200 <- NULL

for (i in 1:length(GPTtexts200$texts)) {
  for (j in 1:length(GPTtexts200$texts[[i]])) {
    GPTcounts200 <- rbind(GPTcounts200,tabulate(text_to_fw_indices(GPTtexts200$texts[[i]][[j]],functionwords),nbins=K))
  }
}

for (i in 1:length(mixedtexts200$texts)) {
  print(i)
  for (j in 1:length(mixedtexts200$texts[[i]])) {
    mixedcounts200 <- rbind(mixedcounts200,tabulate(text_to_fw_indices(mixedtexts200$texts[[i]][[j]],functionwords),nbins=K))
  }
}

for (i in 1:length(humantexts$texts)) {
  print(i)
  for (j in 1:length(humantexts$texts[[i]])) {
    humancounts <- rbind(humancounts,tabulate(text_to_fw_indices(humantexts$texts[[i]][[j]],functionwords),nbins=K))
  }
}

for (i in 1:length(truecps200$texts)) {
  print(i)
  for (j in 1:length(truecps200$texts[[i]])) {
    truechangepoints200 <- rbind(truechangepoints200,  as.numeric(strsplit(truecps200$texts[[i]][[j]],',')[[1]]))
  }
}

# Saving
save(humancounts, mixedcounts200, GPTcounts200, truechangepoints200, functionwords, 
     file = "counts_200.RData")


####################################################################################


# Distribution of essay length plot
# Distribution of ChatGPT passage length plot




# Table of top function word differences relative to each other (only done for 500 word length GPT passages)

human_props <- humancounts / rowSums(humancounts)
gpt_props   <- GPTcounts / rowSums(GPTcounts)
mixed_props <- mixedcounts / rowSums(mixedcounts)

human_means_p <- colMeans(human_props)
gpt_means_p   <- colMeans(gpt_props)
mixed_means <- colMeans(mixed_props)


diff <- (human_means_p) / (gpt_means_p)

top_indices <- order(diff, decreasing = F)[1:10]

top_words_comparison <- data.frame(
  Word = functionwords[top_indices],
  Human_Prop = round(human_means_p[top_indices], 4),
  GPT_Prop   = round(gpt_means_p[top_indices], 4),
  Mixed_Props = round(mixed_means[top_indices], 4)
)

print(top_words_comparison)


# Histogram of function word proportions

human_fw_prop <- rowSums(human_props[,1:(K-1)])
gpt_fw_prop   <- rowSums(gpt_props[,1:(K-1)])
mixed_fw_prop <- rowSums(mixed_props[,1:(K-1)])

global_xlim <- c(0.30, 0.70) # setting a global lim so graphs are more easily comparable

par(mfrow=c(1,3))
hist(human_fw_prop, xlim=global_xlim, col=rgb(0,0,1,0.5), main="Human", xlab="FW proportion")
hist(gpt_fw_prop,   xlim=global_xlim, col=rgb(1,0,0,0.5), main="GPT",   xlab="FW proportion")
hist(mixed_fw_prop, xlim=global_xlim, col=rgb(0.5,0,0.5,0.5), main="Mixed", xlab="FW proportion")

# MDS plot

human_ratios <- humancounts / rowSums(humancounts)
gpt_ratios   <- GPTcounts / rowSums(GPTcounts)

combined_counts <- rbind(human_ratios, gpt_ratios)
d <- dist(combined_counts)
pts <- cmdscale(d)

library(ggplot2)

df <- data.frame(
  MDS1 = pts[, 1],
  MDS2 = pts[, 2],
  Author = c(rep("Human", nrow(humancounts)), rep("GPT", nrow(GPTcounts)))
)

ggplot(df, aes(x = MDS1, y = MDS2, color = Author)) +
  geom_point(alpha = 0.5, size = 0.5) + # Added slight transparency for overlapping points
  scale_color_manual(values = c("Human" = "steelblue4", "GPT" = "orangered4")) +
  labs(
    title = "Style Clustering (MDS)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal() + 
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold") 
  )



#### Actual methodology (using parallel to speed things up) for the 500 word length passages

library(parallel)
n_cores <- detectCores() - 1  # leave one core free

# setting parameters
skip       <- 50
min_length <- 100
target_fp  <- 0.05
alpha <- 1

# setting seed for reproducibility
set.seed(1)

# function to flatten the nested corpus structure into a simple vector
flatten_texts <- function(corpus_obj) {
  out <- character()
  for (i in seq_along(corpus_obj$texts)) {
    for (j in seq_along(corpus_obj$texts[[i]])) {
      out <- c(out, corpus_obj$texts[[i]][[j]])
    }
  }
  out
}

human_flat_texts <- flatten_texts(humantexts)
mixed_flat_texts  <- flatten_texts(mixedtexts)

stopifnot(length(human_flat_texts) == nrow(humancounts))
stopifnot(length(mixed_flat_texts) == nrow(mixedcounts))
stopifnot(nrow(humancounts) == nrow(GPTcounts))
stopifnot(nrow(humancounts) == nrow(mixedcounts))
stopifnot(nrow(humancounts) == nrow(truechangepoints))

# Training and test split
make_aligned_split <- function(n, train_prop = 0.7, seed = 1) {
  set.seed(seed)
  train_idx <- sample(seq_len(n), size = floor(train_prop * n), replace = FALSE)
  test_idx  <- setdiff(seq_len(n), train_idx)
  list(train = sort(train_idx), test = sort(test_idx))
}

n_docs    <- nrow(humancounts)
split_obj <- make_aligned_split(n_docs, train_prop = 0.7, seed = 1)
train_idx <- split_obj$train
test_idx  <- split_obj$test

# Estimating theta_c and theta_h from only the training data set

estimate_theta <- function(count_matrix, alpha = 1) {
  theta <- colSums(count_matrix)
  (theta + alpha) / sum(theta + alpha)
}
humantheta_train <- estimate_theta(humancounts[train_idx, , drop = FALSE], alpha = alpha)
GPTtheta_train   <- estimate_theta(GPTcounts[train_idx, , drop = FALSE], alpha = alpha)


# Computes the log Bayes factor for one text

compute_logBF_from_text <- function(text, functionwords, humantheta, GPTtheta,
                                    skip = 50, min_length = 100) {
  y   <- text_to_fw_indices(text, functionwords)
  cps <- cp_posterior(x = y, human_theta = humantheta, gpt_theta = GPTtheta,
                      skip = skip, min_length = min_length)
  M0     <- sequenceLikelihood(y, humantheta)
  M1     <- logmeanexp(cps[, "loglik"])
  logBF  <- M0 - M1
  list(logBF = as.numeric(logBF), M0 = as.numeric(M0),
       M1 = as.numeric(M1), cps = cps)
}

# Estimates the ChatGPT passage's start and end point based on MAP estimate for one text

locate_segment_from_text <- function(text, functionwords, humantheta, GPTtheta,
                                     skip = 50, min_length = 100) {
  y    <- text_to_fw_indices(text, functionwords)
  cps  <- cp_posterior(x = y, human_theta = humantheta, gpt_theta = GPTtheta,
                       skip = skip, min_length = min_length)
  best <- cps[1, ]
  list(c1 = as.numeric(best["c1"]), c2 = as.numeric(best["c2"]),
       posterior = as.numeric(best["posterior"]),
       loglik    = as.numeric(best["loglik"]), cps = cps)
}

# Tuning gamma on the training dataset

cat("Tuning gamma on human training essays...\n")

# to make it run quicker
cl <- makeCluster(n_cores) 
clusterExport(cl, varlist = c(
  "compute_logBF_from_text", "text_to_fw_indices", "cp_posterior",
  "sequenceLikelihood", "logmeanexp",
  "human_flat_texts", "functionwords",
  "humantheta_train", "GPTtheta_train",
  "skip", "min_length"
))

train_human_logBF <- parSapply(cl, train_idx, function(idx) {
  res <- compute_logBF_from_text(
    text         = human_flat_texts[idx],
    functionwords = functionwords,
    humantheta   = humantheta_train,
    GPTtheta     = GPTtheta_train,
    skip         = skip,
    min_length   = min_length
  )
  res$logBF
})

stopCluster(cl)

# Sets threshold to achieve target false positive rate on training data
log_gamma <- as.numeric(quantile(train_human_logBF, probs = target_fp, names = FALSE))
gamma     <- exp(log_gamma)

cat("Chosen log(gamma)=", round(log_gamma, 4), "\n")
cat("Chosen gamma  =", signif(gamma, 4), "\n")

# We use the held out test dataset and perform ChatGPT passage detection with the essays in it

cat("Evaluating detection on test set...\n")

cl <- makeCluster(n_cores)
clusterExport(cl, varlist = c(
  "compute_logBF_from_text", "text_to_fw_indices", "cp_posterior",
  "sequenceLikelihood", "logmeanexp",
  "human_flat_texts", "mixed_flat_texts", "functionwords",
  "humantheta_train", "GPTtheta_train",
  "skip", "min_length"
))

test_results <- parLapply(cl, test_idx, function(idx) {
  res_h <- compute_logBF_from_text(
    text  = human_flat_texts[idx],
    functionwords = functionwords,
    humantheta = humantheta_train,
    GPTtheta = GPTtheta_train,
    skip= skip,
    min_length = min_length
  )
  res_m <- compute_logBF_from_text(
    text = mixed_flat_texts[idx],
    functionwords = functionwords,
    humantheta = humantheta_train,
    GPTtheta = GPTtheta_train,
    skip = skip,
    min_length = min_length
  )
  list(
    human_logBF =res_h$logBF, human_M0= res_h$M0, human_M1= res_h$M1,
    mixed_logBF = res_m$logBF, mixed_M0= res_m$M0, mixed_M1= res_m$M1
  )
})
#
stopCluster(cl)

human_test_logBF <- sapply(test_results,`[[`,"human_logBF")
mixed_test_logBF <- sapply(test_results, `[[`,"mixed_logBF")
human_H0 <- sapply(test_results, `[[`,"human_M0")
human_H1 <- sapply(test_results, `[[`,"human_M1")
mixed_H0 <- sapply(test_results, `[[`,"mixed_M0")
mixed_H1<- sapply(test_results, `[[`,"mixed_M1")

human_pred_gpt <- human_test_logBF <= log_gamma
mixed_pred_gpt <- mixed_test_logBF <= log_gamma

false_positive_rate <- mean(human_pred_gpt)
true_positive_rate  <- mean(mixed_pred_gpt)
total <- sum(!human_pred_gpt) + sum(human_pred_gpt) + sum(!mixed_pred_gpt) + sum(mixed_pred_gpt)

confusion_mat <- matrix(
  c(sum(!human_pred_gpt), sum(human_pred_gpt),
    sum(!mixed_pred_gpt), sum(mixed_pred_gpt)),
  nrow = 2, byrow = TRUE,
  dimnames = list(
    Truth      = c("Human", "Mixed"),
    Prediction = c("PredHuman", "PredGPT")
  )
)

auc_value <- auroc(
  H0_human = human_H0, H1_human = human_H1,
  H0_mixed = mixed_H0, H1_mixed = mixed_H1
)

# Performing passage localisation on the test set

cat("Evaluating localisation on mixed test essays...\n")

cl <- makeCluster(n_cores)
clusterExport(cl, varlist = c(
  "locate_segment_from_text", "text_to_fw_indices", "cp_posterior",
  "mixed_flat_texts", "functionwords",
  "humantheta_train", "GPTtheta_train",
  "skip", "min_length"
))

loc_results <- parLapply(cl, test_idx, function(idx) {
  loc <- locate_segment_from_text(
    text          = mixed_flat_texts[idx],
    functionwords = functionwords,
    humantheta    = humantheta_train,
    GPTtheta      = GPTtheta_train,
    skip          = skip,
    min_length    = min_length
  )
  list(c1 = loc$c1, c2 = loc$c2, posterior = loc$posterior)
})

stopCluster(cl)

tolerance <- skip/2

est_c1    <- sapply(loc_results, `[[`, "c1")
est_c2    <- sapply(loc_results, `[[`, "c2")
best_post <- sapply(loc_results, `[[`, "posterior")

true_c1 <- truechangepoints[test_idx, 1]
true_c2 <- truechangepoints[test_idx, 2]

err_c1 <- abs(est_c1 - true_c1)
err_c2 <- abs(est_c2 - true_c2)

adj_err_c1 <- ifelse(err_c1 <= tolerance, 0, err_c1 - tolerance)
adj_err_c2 <- ifelse(err_c2 <= tolerance, 0, err_c2 - tolerance)


localisation_results <- data.frame(
  doc_id         = test_idx,
  true_c1        = true_c1,  true_c2   = true_c2,
  est_c1         = est_c1,   est_c2    = est_c2,
  err_c1         = err_c1,   err_c2    = err_c2,
  best_posterior = best_post
)

mean_err_c1   <- mean(err_c1)
mean_err_c2   <- mean(err_c2)
median_err_c1 <- median(err_c1)
median_err_c2 <- median(err_c2)

# Print of key summary statistics

cat("\n==============================\n")
cat("DETECTION RESULTS\n")
cat("==============================\n")
cat("False positive rate (Human -> GPT):", round(false_positive_rate, 4), "\n")
cat("True positive rate  (Mixed -> GPT):", round(true_positive_rate, 4), "\n")
cat("AUROC:", round(auc_value, 4), "\n\n")
print(confusion_mat)

cat("\n==============================\n")
cat("LOCALISATION RESULTS\n")
cat("==============================\n")
cat("Mean |tau1 - tau1_hat|:",   round(mean_err_c1,   2), "\n")
cat("Mean |tau2 - tau2_hat|:",   round(mean_err_c2,   2), "\n")
cat("Median |tau1 - tau1_hat|:", round(median_err_c1, 2), "\n")
cat("Median |tau2 - tau2_hat|:", round(median_err_c2, 2), "\n")

# Save everything so we dont need to rerun

project_results <- list(
  train_idx = train_idx,
  test_idx  = test_idx,
  humantheta_train = humantheta_train,
  GPTtheta_train  = GPTtheta_train,
  log_gamma = log_gamma,
  gamma = gamma,
  train_human_logBF = train_human_logBF,
  human_test_logBF = human_test_logBF,
  mixed_test_logBF  = mixed_test_logBF,
  false_positive_rate  = false_positive_rate,
  true_positive_rate = true_positive_rate,
  auc  = auc_value,
  confusion_matrix = confusion_mat,
  localisation_results = localisation_results,
  adj_err_c1  = mean(adj_err_c1),
  adj_err_c2  = mean(adj_err_c2),
  mean_err_c1  = mean_err_c1,
  mean_err_c2  = mean_err_c2,
  median_err_c1  = median_err_c1,
  median_err_c2  = median_err_c2,
  skip    = skip,
  min_length  = min_length,
  alpha    = alpha
)

# Did the same for passages of length 100 and 200 and experimented with changing min lenght and
# skip parameters







