source("assignment2-functions.R")

library(parallel)
n_cores <- detectCores() - 1  # leave one core free

skip       <- 50
min_length <- 100
target_fp  <- 0.05

#########
#### Load the data
###########

functionwords <- readLines("wordfile200 (1).txt")
humantexts <- loadCorpusText("essays/human/")
GPTtexts <- loadCorpusText("essays/GPT100/")
mixedtexts <- loadCorpusText("essays/mixed100/")
truecps <- loadCorpusText("essays/changepoints100/")

alpha <- 1

GPTtheta <- apply(GPTcounts100, 2, sum)
GPTtheta <- (GPTtheta + alpha) / sum(GPTtheta + alpha)

humantheta <- apply(humancounts, 2, sum)
humantheta <- (humantheta + alpha) / sum(humantheta + alpha)

set.seed(1)

###############################
### 1. Flatten text objects  ###
###############################

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
stopifnot(length(mixed_flat_texts) == nrow(mixedcounts100))
stopifnot(nrow(humancounts) == nrow(GPTcounts100))
stopifnot(nrow(humancounts) == nrow(mixedcounts100))
stopifnot(nrow(humancounts) == nrow(truechangepoints100))

#########################################
### 2. Proper aligned train/test split ###
#########################################

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

#####################################
### 3. Estimate theta from training ###
#####################################

estimate_theta <- function(count_matrix, alpha = 1) {
  theta <- colSums(count_matrix)
  (theta + alpha) / sum(theta + alpha)
}

alpha            <- 1
humantheta_train <- estimate_theta(humancounts[train_idx, , drop = FALSE], alpha = alpha)
GPTtheta_train   <- estimate_theta(GPTcounts100[train_idx, , drop = FALSE], alpha = alpha)

#########################################
### 4. Wrapper: one-text Bayes factor  ###
#########################################

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

##############################################
### 5. Wrapper: one-text localisation      ###
##############################################

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

##############################################
### 6. Tune gamma on human training essays ###
##############################################

cat("Tuning gamma on human training essays...\n")

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

log_gamma <- as.numeric(quantile(train_human_logBF, probs = target_fp, names = FALSE))
gamma     <- exp(log_gamma)

cat("Chosen log(gamma) =", round(log_gamma, 4), "\n")
cat("Chosen gamma      =", signif(gamma, 4), "\n")

#########################################
### 7. Detection on held-out test set ###
#########################################

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
    text          = human_flat_texts[idx],
    functionwords = functionwords,
    humantheta    = humantheta_train,
    GPTtheta      = GPTtheta_train,
    skip          = skip,
    min_length    = min_length
  )
  res_m <- compute_logBF_from_text(
    text          = mixed_flat_texts[idx],
    functionwords = functionwords,
    humantheta    = humantheta_train,
    GPTtheta      = GPTtheta_train,
    skip          = skip,
    min_length    = min_length
  )
  list(
    human_logBF = res_h$logBF, human_M0 = res_h$M0, human_M1 = res_h$M1,
    mixed_logBF = res_m$logBF, mixed_M0 = res_m$M0, mixed_M1 = res_m$M1
  )
})

stopCluster(cl)

human_test_logBF <- sapply(test_results, `[[`, "human_logBF")
mixed_test_logBF <- sapply(test_results, `[[`, "mixed_logBF")
human_H0         <- sapply(test_results, `[[`, "human_M0")
human_H1         <- sapply(test_results, `[[`, "human_M1")
mixed_H0         <- sapply(test_results, `[[`, "mixed_M0")
mixed_H1         <- sapply(test_results, `[[`, "mixed_M1")

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

############################################
### 8. Localisation on mixed test essays ###
############################################
tolerance <- skip/2  # allow for some tolerance in changepoint estimation
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
adj_mean_err_c1   <- mean(adj_err_c1)
adj_mean_err_c2   <- mean(adj_err_c2)
adj_median_err_c1 <- median(adj_err_c1)
adj_median_err_c2 <- median(adj_err_c2)

################################
### 9. Print key summaries   ###
################################

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

################################
### 10. Save all outputs     ###
################################

project_results <- list(
  train_idx            = train_idx,
  test_idx             = test_idx,
  humantheta_train     = humantheta_train,
  GPTtheta_train       = GPTtheta_train,
  log_gamma            = log_gamma,
  gamma                = gamma,
  train_human_logBF    = train_human_logBF,
  human_test_logBF     = human_test_logBF,
  mixed_test_logBF     = mixed_test_logBF,
  false_positive_rate  = false_positive_rate,
  true_positive_rate   = true_positive_rate,
  auc                  = auc_value,
  confusion_matrix     = confusion_mat,
  localisation_results = localisation_results,
  adj_err_c1          = adj_err_c1,
  adj_err_c2          = adj_err_c2,
  mean_err_c1          = mean_err_c1,
  mean_err_c2          = mean_err_c2,
  median_err_c1        = median_err_c1,
  median_err_c2        = median_err_c2,
  skip                 = skip,
  min_length           = min_length,
  alpha                = alpha
)

filename <- paste0("results_100_", skip, "_", min_length)
save(project_results, file = paste(filename, ".RData", sep = ''))
results <- get(load('results_100_50_100.RData'))
paste(results$mean_err_c1,'&', results$mean_err_c2,'&', mean(results$adj_err_c1),'&', mean(results$adj_err_c2))
paste(results$median_err_c1,'&', results$median_err_c2,'&', median(results$adj_err_c1),'&', median(results$adj_err_c2))

