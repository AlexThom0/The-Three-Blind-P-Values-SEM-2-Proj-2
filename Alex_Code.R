# Alex Code

#load in the functions I have provided. You will need to change this to point to the directory
#which you donloaded the files to.
source("assignment2-functions.R")

#########
#### Load the data
###########

functionwords <- readLines("wordfile200 (1).txt") #function word list
humantexts <- loadCorpusText("essays/human/")

GPTtexts <- loadCorpusText("essays/GPT500/")
mixedtexts <- loadCorpusText("essays/mixed500/")
truecps <- loadCorpusText("essays/changepoints500/")



row_normalise <- function(X) {
  rs <- rowSums(X)
  X / rs
}

norm_GPT <- row_normalise(GPTcounts)

### next we create n x 201 matrices where each row corresponds to one text, and the columns are function word counts
###Note: this can be slow since it is reading many files, I would advise running it once and saving the results
#
#on my laptop this takes around 10 minutes (each count is up to 110 since there are 110 topic folders)

K <- length(functionwords) + 1
GPTcounts <- NULL
humancounts <- NULL
mixedcounts <- NULL
truechangepoints <- NULL


#again doing rbind inside a loop is inefficient and slow, I am doing it here to keep the code simple
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

# Save the specific objects you need for the project
save(humancounts, mixedcounts, GPTcounts, truechangepoints, functionwords, 
     file = "LLM_Project_Data.RData")


#IMPORTANT NOTE (mentioned on assessment sheet): if you are subsetting these matrices to do a training-test split, then it is crucial
#to understand that the indexes of the essays are the same across each matrix.
#for example, mixedcounts[3,] is the function word counts for the text which combines the human
#essay with counts  humancounts[3,]  with the GPT text GPTcounts[3,].

#Therefore, you need to make sure sure that either all of these go into the training set, or all
#go into the test set. Do not allow a situation where you have (eg) humancounts[3,]  in the training et
#and  mixedcounts[3,]  in the teset set


#############################
### Parameter Estimation ###
#############################

#Important: note that I am not using a train-test set split here! You should
#modify this code to be more principled, in terms of estimating the parameters
#on different texts than you use for evaluation

#here I am going to add 1 to every count. This is a standard regularisation 'trick'
#which prevents any of the functoin words being estimated as having exactly 0 probabilty 
#of occuring. Having a 0 is a problem, because if this word ever occurs in a test text,
#then the likelihood becomes negative infinity.

alpha <- 1

GPTtheta <- apply(GPTcounts,2,sum); 
GPTtheta <- (GPTtheta + alpha) / sum(GPTtheta + alpha)

humantheta <- apply(humancounts,2,sum); 
humantheta <- (humantheta + alpha) / sum(humantheta + alpha)


####################################################
### Fit a change point model to a particular text###
####################################################

#this code iterates over every mixed text, and computes the change point posterior
#it then seaves the most likely change poitns for each text

#hte parameter 'skip' defines the grid which the change points are computed over
#eg if skip=50 then we only consider change points to occur at locations 1, 51, 101, 151, etc

#min_length is the minimum length of the GPT segment

skip <- 50
min_length <- 100

#choose a random text to analyse
i <- 1; j <- 1

y <- text_to_fw_indices(mixedtexts$texts[[i]][[j]],functionwords)
cps <- cp_posterior(y, humantheta, GPTtheta, skip = skip, min_length =min_length )

#the change points are returned in a sorted order, whree the first row is the one with the highest
#posterior probability. You should inspect this object to see how it works.
cps
cps[1,]

#now you can do this for every mixed document, and compare your change point 
#estimates to the true values in 'truechangepoints'



##########################
### Hypothesis Testing ###
##########################

#next we will test whether a particular text contains a GPT segment at all

#again it is important to note that I am not doing any train/test split here! You should do this, however


#here is how to compute the Bayes factor for a single text
i <- 1; j <- 1

skip <- 50
min_length <- 100

y <- text_to_fw_indices(humantexts$texts[[i]][[j]],functionwords)
cps <- cp_posterior(y, humantheta, GPTtheta, skip = skip, min_length =min_length )

M0 <- sequenceLikelihood(y,humantheta) #likelihood without any change points

#use logsumexp to average the posterior over all change point configurations
M1 <- logmeanexp(cps$loglik) 

#compute the Bayes Factor. Note this is p(y | M0) / p(y | M1). Sometimes in textbooks
#you will see this defined as the reciprocal  p(y | M1) / p(y | M0). Either approach is fine
#just be careful about how you define gamma and do the testing

logBF <- M0 - M1  #both M0 and M1 are in log space
exp(logBF)






###### --------- Estimated Vs True Change points -------

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





###############################
### FULL TRAIN/TEST PIPELINE ###
###############################

## Assumes these already exist in memory:
## humantexts, mixedtexts, GPTtexts
## humancounts, mixedcounts, GPTcounts, truechangepoints
## functionwords
## and the functions:
## text_to_fw_indices, cp_posterior, sequenceLikelihood, logmeanexp, auroc

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
mixed_flat_texts <- flatten_texts(mixedtexts)

# Optional checks
stopifnot(length(human_flat_texts) == nrow(humancounts))
stopifnot(length(mixed_flat_texts) == nrow(mixedcounts))
stopifnot(nrow(humancounts) == nrow(GPTcounts))
stopifnot(nrow(humancounts) == nrow(mixedcounts))
stopifnot(nrow(humancounts) == nrow(truechangepoints))

#########################################
### 2. Proper aligned train/test split ###
#########################################

make_aligned_split <- function(n, train_prop = 0.7, seed = 1) {
  set.seed(seed)
  train_idx <- sample(seq_len(n), size = floor(train_prop * n), replace = FALSE)
  test_idx  <- setdiff(seq_len(n), train_idx)
  list(train = sort(train_idx), test = sort(test_idx))
}

n_docs <- nrow(humancounts)
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

alpha <- 1
humantheta_train <- estimate_theta(humancounts[train_idx, , drop = FALSE], alpha = alpha)
GPTtheta_train   <- estimate_theta(GPTcounts[train_idx, , drop = FALSE], alpha = alpha)

#########################################
### 4. Wrapper: one-text Bayes factor  ###
#########################################

compute_logBF_from_text <- function(text, functionwords, humantheta, GPTtheta,
                                    skip = 50, min_length = 100) {
  y <- text_to_fw_indices(text, functionwords)
  
  cps <- cp_posterior(
    x = y,
    human_theta = humantheta,
    gpt_theta = GPTtheta,
    skip = skip,
    min_length = min_length
  )
  
  M0 <- sequenceLikelihood(y, humantheta)
  M1 <- logmeanexp(cps[, "loglik"])
  logBF <- M0 - M1
  
  list(
    logBF = as.numeric(logBF),
    M0 = as.numeric(M0),
    M1 = as.numeric(M1),
    cps = cps
  )
}

##############################################
### 5. Wrapper: one-text localisation      ###
##############################################

locate_segment_from_text <- function(text, functionwords, humantheta, GPTtheta,
                                     skip = 50, min_length = 100) {
  y <- text_to_fw_indices(text, functionwords)
  
  cps <- cp_posterior(
    x = y,
    human_theta = humantheta,
    gpt_theta = GPTtheta,
    skip = skip,
    min_length = min_length
  )
  
  best <- cps[1, ]
  list(
    c1 = as.numeric(best["c1"]),
    c2 = as.numeric(best["c2"]),
    posterior = as.numeric(best["posterior"]),
    loglik = as.numeric(best["loglik"]),
    cps = cps
  )
}

##############################################
### 6. Tune gamma on human training essays ###
##############################################

skip <- 50
min_length <- 100
target_fp <- 0.05

cat("Tuning gamma on human training essays...\n")

train_human_logBF <- numeric(length(train_idx))

for (k in seq_along(train_idx)) {
  idx <- train_idx[k]
  res <- compute_logBF_from_text(
    text = human_flat_texts[idx],
    functionwords = functionwords,
    humantheta = humantheta_train,
    GPTtheta = GPTtheta_train,
    skip = skip,
    min_length = min_length
  )
  train_human_logBF[k] <- res$logBF
  
  if (k %% 25 == 0) cat("  done", k, "of", length(train_idx), "\n")
}

# Choose gamma so about 5% of human training essays are flagged as GPT-containing
log_gamma <- as.numeric(quantile(train_human_logBF, probs = target_fp, names = FALSE))
gamma <- exp(log_gamma)

cat("Chosen log(gamma) =", round(log_gamma, 4), "\n")
cat("Chosen gamma      =", signif(gamma, 4), "\n")

#########################################
### 7. Detection on held-out test set ###
#########################################

cat("Evaluating detection on test set...\n")

human_test_logBF <- numeric(length(test_idx))
mixed_test_logBF <- numeric(length(test_idx))

human_H0 <- numeric(length(test_idx))
human_H1 <- numeric(length(test_idx))
mixed_H0 <- numeric(length(test_idx))
mixed_H1 <- numeric(length(test_idx))

for (k in seq_along(test_idx)) {
  idx <- test_idx[k]
  
  # Human test essay
  res_h <- compute_logBF_from_text(
    text = human_flat_texts[idx],
    functionwords = functionwords,
    humantheta = humantheta_train,
    GPTtheta = GPTtheta_train,
    skip = skip,
    min_length = min_length
  )
  human_test_logBF[k] <- res_h$logBF
  human_H0[k] <- res_h$M0
  human_H1[k] <- res_h$M1
  
  # Mixed test essay
  res_m <- compute_logBF_from_text(
    text = mixed_flat_texts[idx],
    functionwords = functionwords,
    humantheta = humantheta_train,
    GPTtheta = GPTtheta_train,
    skip = skip,
    min_length = min_length
  )
  mixed_test_logBF[k] <- res_m$logBF
  mixed_H0[k] <- res_m$M0
  mixed_H1[k] <- res_m$M1
  
  if (k %% 25 == 0) cat("  done", k, "of", length(test_idx), "\n")
}

# Decision rule:
# classify as GPT-containing if logBF <= log_gamma
human_pred_gpt <- human_test_logBF <= log_gamma
mixed_pred_gpt <- mixed_test_logBF <= log_gamma

false_positive_rate <- mean(human_pred_gpt)
true_positive_rate  <- mean(mixed_pred_gpt)

confusion_mat <- matrix(
  c(sum(!human_pred_gpt), sum(human_pred_gpt),
    sum(!mixed_pred_gpt), sum(mixed_pred_gpt)),
  nrow = 2, byrow = TRUE,
  dimnames = list(
    Truth = c("Human", "Mixed"),
    Prediction = c("PredHuman", "PredGPT")
  )
)

auc_value <- auroc(
  H0_human = human_H0,
  H1_human = human_H1,
  H0_mixed = mixed_H0,
  H1_mixed = mixed_H1
)

############################################
### 8. Localisation on mixed test essays ###
############################################

cat("Evaluating localisation on mixed test essays...\n")

est_c1 <- numeric(length(test_idx))
est_c2 <- numeric(length(test_idx))
best_post <- numeric(length(test_idx))

for (k in seq_along(test_idx)) {
  idx <- test_idx[k]
  
  loc <- locate_segment_from_text(
    text = mixed_flat_texts[idx],
    functionwords = functionwords,
    humantheta = humantheta_train,
    GPTtheta = GPTtheta_train,
    skip = skip,
    min_length = min_length
  )
  
  est_c1[k] <- loc$c1
  est_c2[k] <- loc$c2
  best_post[k] <- loc$posterior
  
  if (k %% 25 == 0) cat("  done", k, "of", length(test_idx), "\n")
}

true_c1 <- truechangepoints[test_idx, 1]
true_c2 <- truechangepoints[test_idx, 2]

err_c1 <- abs(est_c1 - true_c1)
err_c2 <- abs(est_c2 - true_c2)

localisation_results <- data.frame(
  doc_id = test_idx,
  true_c1 = true_c1,
  true_c2 = true_c2,
  est_c1 = est_c1,
  est_c2 = est_c2,
  err_c1 = err_c1,
  err_c2 = err_c2,
  best_posterior = best_post
)

mean_err_c1 <- mean(err_c1)
mean_err_c2 <- mean(err_c2)
median_err_c1 <- median(err_c1)
median_err_c2 <- median(err_c2)

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
cat("Mean |tau1 - tau1_hat|:", round(mean_err_c1, 2), "\n")
cat("Mean |tau2 - tau2_hat|:", round(mean_err_c2, 2), "\n")
cat("Median |tau1 - tau1_hat|:", round(median_err_c1, 2), "\n")
cat("Median |tau2 - tau2_hat|:", round(median_err_c2, 2), "\n")

################################
### 10. Save all outputs     ###
################################

project_results <- list(
  train_idx = train_idx,
  test_idx = test_idx,
  humantheta_train = humantheta_train,
  GPTtheta_train = GPTtheta_train,
  log_gamma = log_gamma,
  gamma = gamma,
  train_human_logBF = train_human_logBF,
  human_test_logBF = human_test_logBF,
  mixed_test_logBF = mixed_test_logBF,
  false_positive_rate = false_positive_rate,
  true_positive_rate = true_positive_rate,
  auc = auc_value,
  confusion_matrix = confusion_mat,
  localisation_results = localisation_results,
  mean_err_c1 = mean_err_c1,
  mean_err_c2 = mean_err_c2,
  median_err_c1 = median_err_c1,
  median_err_c2 = median_err_c2,
  skip = skip,
  min_length = min_length,
  alpha = alpha
)

save(project_results, file = "project_results_500.RData")



# ----------------- Gamma Tuning Attempt :0--------


gamma_tradeoff <- function(human_logBF, mixed_logBF, gamma_grid = NULL) {
  
  if (is.null(gamma_grid)) {
    all_scores <- c(human_logBF, mixed_logBF)
    gamma_grid <- seq(
      from = min(all_scores),
      to   = max(all_scores),
      length.out = 200
    )
  }
  
  out <- data.frame(
    log_gamma = gamma_grid,
    gamma = exp(gamma_grid),
    FPR = NA_real_,   # false positive rate on human
    TPR = NA_real_,   # true positive rate on mixed
    FNR = NA_real_,   # false negative rate on mixed
    TNR = NA_real_,   # true negative rate on human
    Precision = NA_real_,
    Recall = NA_real_,
    F1 = NA_real_,
    BalancedAccuracy = NA_real_
  )
  
  for (i in seq_along(gamma_grid)) {
    g <- gamma_grid[i]
    
    # Predict GPT if logBF <= g
    human_pred_gpt <- human_logBF <= g
    mixed_pred_gpt <- mixed_logBF <= g
    
    TP <- sum(mixed_pred_gpt)
    FN <- sum(!mixed_pred_gpt)
    FP <- sum(human_pred_gpt)
    TN <- sum(!human_pred_gpt)
    
    FPR <- if ((FP + TN) > 0) FP / (FP + TN) else NA_real_
    TPR <- if ((TP + FN) > 0) TP / (TP + FN) else NA_real_
    FNR <- if ((TP + FN) > 0) FN / (TP + FN) else NA_real_
    TNR <- if ((FP + TN) > 0) TN / (FP + TN) else NA_real_
    
    Precision <- if ((TP + FP) > 0) TP / (TP + FP) else NA_real_
    Recall    <- TPR
    F1 <- if (!is.na(Precision) && !is.na(Recall) && (Precision + Recall) > 0) {
      2 * Precision * Recall / (Precision + Recall)
    } else {
      NA_real_
    }
    
    BA <- mean(c(TPR, TNR), na.rm = TRUE)
    
    out$FPR[i] <- FPR
    out$TPR[i] <- TPR
    out$FNR[i] <- FNR
    out$TNR[i] <- TNR
    out$Precision[i] <- Precision
    out$Recall[i] <- Recall
    out$F1[i] <- F1
    out$BalancedAccuracy[i] <- BA
  }
  
  out
}

# Build the trade-off table on the TEST SET
gamma_results <- gamma_tradeoff(
  human_logBF = human_test_logBF,
  mixed_logBF = mixed_test_logBF
)



head(gamma_results)

############################################
### PLOT 1: TPR and FPR against log(gamma) ###
############################################

library(ggplot2)
library(tidyr)
library(tidyverse)
library(latex2exp)

plot_df_long <- pivot_longer(
  data.frame(
    log_gamma = gamma_results$log_gamma,
    TPR = gamma_results$TPR,
    FPR = gamma_results$FPR
  ),
  cols = c(TPR, FPR),
  names_to = "Metric",
  values_to = "Rate"
)

ggplot(plot_df_long, aes(x = log_gamma, y = Rate, colour = Metric)) +
  geom_line(size = 1) +
  
  # threshold line
  geom_vline(xintercept = log_gamma, linetype = "dashed") +
  
  geom_text(
    x = log_gamma,
    y = 0.5,
    label = TeX("Chosen $\\gamma$"),
    angle = 90,
    vjust = -0.5,
    size = 4,
    colour = "black",
    inherit.aes = FALSE
  )+
  
  labs(
    title = "Detection Trade-off as the Threshold Varies",
    x = expression(log(gamma)),
    y = "Rate",
    colour = "Metric"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal()





error_df <- data.frame(
  Error = c(err_c1, err_c2),
  Type = rep(c("Start Point (τ1)", "End Point (τ2)"), each = length(err_c1))
)

error_df$Type <- factor(error_df$Type, 
                        levels = c("Start Point (τ1)", "End Point (τ2)"))

library(tidyverse)
library(ggplot2)

stats_df <- error_df %>%
  group_by(Type) %>%
  summarise(
    mean_error = 200,
    pct_above_mean = mean(Error > mean_error) * 100
  )

ggplot(error_df, aes(x = Type, y = Error, fill = Type)) +
  geom_boxplot(alpha = 0.7) +
  
  # Mean line
  geom_crossbar(
    data = stats_df,
    aes(x = Type, y = mean_error, ymin = mean_error, ymax = mean_error),
    width = 0.5,
    colour = "black",
    fatten = 0
  ) +
  
  # % above mean label
  geom_text(
    data = stats_df,
    aes(
      x = Type,
      y = mean_error,
      label = paste0(round(pct_above_mean, 1), "% above 200 words")
    ),
    vjust = -1,
    size = 4
  ) +
  
  labs(
    title = "Change Point Localisation Errors for 500 Word ChatGPT",
    x = NULL,
    y = "Absolute Error (words)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



library(ggplot2)

library(fitdistrplus)
library(dplyr)
library(dplyr)
library(ggplot2)
library(MASS)

start_error <- error_df %>%
  dplyr::filter(Type == "Start Point (τ1)") %>%
  dplyr::pull(Error)

end_error <- error_df %>%
  dplyr::filter(Type == "End Point (τ2)") %>%
  dplyr::pull(Error)

eps <- 1e-6
fit_weib_km <- fitdistr(start_error + eps, densfun = "weibull")

shape_w_km <- as.numeric(fit_weib_km$estimate["shape"])
scale_w_km <- as.numeric(fit_weib_km$estimate["scale"])

library(MASS)
library(ggplot2)
library(dplyr)

# Extract data
start_error <- error_df %>%
  dplyr::filter(Type == "Start Point (τ1)") %>%
  dplyr::pull(Error)

end_error <- error_df %>%
  dplyr::filter(Type == "End Point (τ2)") %>%
  dplyr::pull(Error)

eps <- 1e-6

# =========================
# START ERROR FITS
# =========================

# Weibull
fit_weib_start <- fitdistr(start_error + eps, "weibull")
shape_w_start <- fit_weib_start$estimate["shape"]
scale_w_start <- fit_weib_start$estimate["scale"]



p1 <- ggplot(data.frame(x = start_error), aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 40, colour = "black", fill = "steelblue") +
  
  # Weibull
  stat_function(
    fun = dweibull,
    args = list(shape = shape_w_start, scale = scale_w_start),
    colour = "red", linewidth = 1
  ) +
  
  labs(title = "Start Error: Distribution Fits",
       x = "Absolute Error",
       y = "Density") +
  theme_minimal()

# =========================
# END ERROR FITS
# =========================

# Weibull
fit_weib_end <- fitdistr(end_error + eps, "weibull")
shape_w_end <- fit_weib_end$estimate["shape"]
scale_w_end <- fit_weib_end$estimate["scale"]


p2 <- ggplot(data.frame(x = end_error), aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 40, colour = "black", fill = "steelblue") +
  
  # Weibull
  stat_function(
    fun = dweibull,
    args = list(shape = shape_w_end, scale = scale_w_end),
    colour = "red", linewidth = 1
  ) +
  
  labs(title = "End Error: Distribution Fits",
       x = "Absolute Error",
       y = "Density") +
  theme_minimal()

# Combine
p1 + p2

# Theoretical mean/variance of fitted Weibull

weibull_mean_end <- scale_w_end * gamma(1 + 1/shape_w_end)
weibull_var_end  <- scale_w_end^2 * (gamma(1 + 2/shape_w_end) - (gamma(1 + 1/shape_w_end))^2)

weibull_mean_end
weibull_var_end

weibull_mean_start <- scale_w_start * gamma(1 + 1/shape_w_start)
weibull_var_start  <- scale_w_start^2 * (gamma(1 + 2/shape_w_start) - (gamma(1 + 1/shape_w_start))^2)

weibull_mean_start
weibull_var_start

# Compare with empirical
mean(start_error)
var(start_error)



proj_100_results$auc

proj_200_results$auc

proj_500_results$auc








