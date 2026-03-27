#### Code.R ####
source("assignment2-functions.R")


#### Extracting the stuff that needs extracted #####


# 500 word ChatGPT segment length extractions
functionwords <- readLines("wordfile200 (1).txt") 
humantexts <- loadCorpusText("essays/human/")

GPTtexts <- loadCorpusText("essays/GPT500/")
mixedtexts <- loadCorpusText("essays/mixed500/")
truecps <- loadCorpusText("essays/changepoints500/")

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
GPTtexts100 <- loadCorpusText("essays/GPT100/")
mixedtexts100 <- loadCorpusText("essays/mixed100/")
truecps100 <- loadCorpusText("essays/changepoints100/")

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
GPTtexts200 <- loadCorpusText("essays/GPT200/")
mixedtexts200 <- loadCorpusText("essays/mixed200/")
truecps200 <- loadCorpusText("essays/changepoints200/")


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

lengths = rowSums(humancounts)
cpdf <- as.data.frame.array(truechangepoints)
library(patchwork)
p1<-ggplot(data = as.data.frame(lengths))+
  geom_histogram(aes(x = lengths), binwidth = 100, fill = "steelblue4", color = "black")+
  labs(title = "Distribution of Essay Lengths (Word Count)", x = "Essay Length (words)", y = "Frequency")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Distribution of ChatGPT passage length plot

p2<-ggplot(data = cpdf)+
  geom_histogram(aes(x = (V2-V1)), binwidth = 2, fill = "steelblue4", color = "black")+
  labs(title = "Distribution of ChatGPT Segment Lengths", x = "Segment Length (words)", y = "Frequency")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

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
hist(gpt_fw_prop,   xlim= global_xlim, col=rgb(1,0,0,0.5), main ="GPT",   xlab= "FW proportion")
hist(mixed_fw_prop, xlim=global_xlim, col=rgb(0.5,0,0.5,0.5), main= "Mixed", xlab="FW proportion")

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
  geom_point(alpha = 0.5, size = 0.5) + # Slight transparency for overlapping points
  scale_color_manual(values = c("Human" = "steelblue4", "GPT" = "orangered4")) +
  labs(
    title = "Style Clustering (MDS)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal() + 
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold"))



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
mixed_flat_texts <- flatten_texts(mixedtexts)

# Tests
stopifnot(length(human_flat_texts) == nrow(humancounts))
stopifnot(length(mixed_flat_texts) == nrow(mixedcounts))
stopifnot(nrow(humancounts) == nrow(GPTcounts))
stopifnot(nrow(humancounts) == nrow(mixedcounts))
stopifnot(nrow(humancounts) == nrow(truechangepoints))

# Training and test split
make_aligned_split <- function(n, train_prop = 0.7, seed = 1) {
  set.seed(seed)
  train_idx <- sample(seq_len(n), size = floor(train_prop * n), replace = FALSE)
  test_idx <- setdiff(seq_len(n), train_idx)
  list(train = sort(train_idx), test = sort(test_idx))
}

n_docs <- nrow(humancounts)
split_obj <- make_aligned_split(n_docs, train_prop = 0.7, seed = 1)
train_idx <- split_obj$train
test_idx <- split_obj$test

# Estimating theta_c and theta_h from only the training data set

estimate_theta <- function(count_matrix, alpha = 1) {
  theta <- colSums(count_matrix)
  (theta + alpha) / sum(theta + alpha)
}
humantheta_train <- estimate_theta(humancounts[train_idx, , drop = FALSE], alpha = alpha)
GPTtheta_train <- estimate_theta(GPTcounts[train_idx, , drop = FALSE], alpha = alpha)


# Computes the log Bayes factor for one text

compute_logBF_from_text <- function(text, functionwords, humantheta, GPTtheta,
                                    skip = 50, min_length = 100) {
  y <- text_to_fw_indices(text, functionwords)
  cps <- cp_posterior(x = y, human_theta = humantheta, gpt_theta = GPTtheta,
                      skip = skip, min_length = min_length)
  M0 <- sequenceLikelihood(y, humantheta)
  M1 <- logmeanexp(cps[, "loglik"])
  logBF <- M0 - M1
  list(logBF = as.numeric(logBF), M0 = as.numeric(M0),
       M1 = as.numeric(M1), cps = cps)
}

# Estimates the ChatGPT passage's start and end point based on MAP estimate for one text

locate_segment_from_text <- function(text, functionwords, humantheta, GPTtheta,
                                     skip = 50, min_length = 100) {
  y <- text_to_fw_indices(text, functionwords)
  cps <- cp_posterior(x = y, human_theta = humantheta, gpt_theta = GPTtheta,
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
# sends everything each worker will need to the cluster
clusterExport(cl, varlist = c(
  "compute_logBF_from_text", "text_to_fw_indices", "cp_posterior",
  "sequenceLikelihood", "logmeanexp",
  "human_flat_texts", "functionwords",
  "humantheta_train", "GPTtheta_train",
  "skip", "min_length"
))

# Compute log Bayes factor for each training document (in parallel)
train_human_logBF <- parSapply(cl, train_idx, function(idx) {
  res <- compute_logBF_from_text(
    text = human_flat_texts[idx],
    functionwords = functionwords,
    humantheta = humantheta_train,
    GPTtheta = GPTtheta_train,
    skip= skip,
    min_length = min_length
  )
  res$logBF # only care about the logBF, don't need the other stuff
})

stopCluster(cl) # end the work in parallel

# Sets threshold to achieve target false positive rate on training data
log_gamma <- as.numeric(quantile(train_human_logBF, probs = target_fp, names = FALSE))
gamma <- exp(log_gamma)

cat("Chosen log(gamma)=", round(log_gamma, 4), "\n")
cat("Chosen gamma  =", signif(gamma, 4), "\n")

# We use the held out test dataset and perform ChatGPT passage detection with the essays in it

cl <- makeCluster(n_cores)
clusterExport(cl, varlist = c(
  "compute_logBF_from_text", "text_to_fw_indices", "cp_posterior",
  "sequenceLikelihood", "logmeanexp",
  "human_flat_texts", "mixed_flat_texts", "functionwords",
  "humantheta_train", "GPTtheta_train",
  "skip", "min_length"
))
# Runs detection on all test documents
test_results <- parLapply(cl, test_idx, function(idx) {
  # Human essays
  res_h <- compute_logBF_from_text(
    text = human_flat_texts[idx],
    functionwords = functionwords,
    humantheta = humantheta_train,
    GPTtheta = GPTtheta_train,
    skip= skip,
    min_length = min_length
  )
  # Mixed essays
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

stopCluster(cl)

# Apply array indexing to extract the results

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
    Truth = c("Human", "Mixed"),
    Prediction = c("PredHuman", "PredGPT")
  )
)

auc_value <- auroc(
  H0_human= human_H0, H1_human = human_H1,
  H0_mixed = mixed_H0, H1_mixed = mixed_H1
)

# Performing passage localisation on the test set using parallel again
cl <- makeCluster(n_cores)
clusterExport(cl, varlist = c(
  "locate_segment_from_text", "text_to_fw_indices", "cp_posterior",
  "mixed_flat_texts", "functionwords",
  "humantheta_train", "GPTtheta_train",
  "skip", "min_length"))

# Run for each text in test set
loc_results <- parLapply(cl, test_idx, function(idx) {
  loc <- locate_segment_from_text(
    text = mixed_flat_texts[idx],
    functionwords = functionwords,
    humantheta = humantheta_train,
    GPTtheta = GPTtheta_train,
    skip = skip,
    min_length = min_length
  )
  list(c1 =loc$c1, c2=loc$c2, posterior = loc$posterior)
})

stopCluster(cl)

tolerance<- skip/2

# Estimated change points and extract using array indexing
est_c1 <- sapply(loc_results, `[[`, "c1")
est_c2 <- sapply(loc_results, `[[`, "c2")
best_post <- sapply(loc_results, `[[`, "posterior")

# True change points
true_c1 <- truechangepoints[test_idx, 1]
true_c2 <- truechangepoints[test_idx, 2]

# Errors
err_c1 <- abs(est_c1 - true_c1)
err_c2 <- abs(est_c2 - true_c2)

# Adjusted error accounting for tolerance
adj_err_c1 <- ifelse(err_c1 <= tolerance, 0, err_c1 - tolerance)
adj_err_c2 <- ifelse(err_c2 <= tolerance, 0, err_c2 - tolerance)

# Put results into a dataframe
localisation_results <- data.frame(
  doc_id= test_idx,
  true_c1 = true_c1,  
  true_c2 = true_c2,
  est_c1= est_c1, est_c2 = est_c2,
  err_c1 = err_c1,
  err_c2= err_c2,
  best_posterior = best_post)

# overall errors, median and mean of each
mean_err_c1 <- mean(err_c1)
mean_err_c2 <- mean(err_c2)
median_err_c1 <- median(err_c1)
median_err_c2 <- median(err_c2)

# Save everything so we dont need to rerun

project_results <- list(
  train_idx = train_idx,
  test_idx = test_idx,
  humantheta_train = humantheta_train,
  GPTtheta_train = GPTtheta_train,
  log_gamma = log_gamma,
  gamma = gamma,
  train_human_logBF = train_human_logBF,
  human_test_logBF = human_test_logBF,
  mixed_test_logBF  = mixed_test_logBF,
  false_positive_rate  = false_positive_rate,
  true_positive_rate = true_positive_rate,
  auc = auc_value,
  confusion_matrix = confusion_mat,
  localisation_results = localisation_results,
  adj_err_c1 = mean(adj_err_c1),
  adj_err_c2 = mean(adj_err_c2),
  mean_err_c1 = mean_err_c1,
  mean_err_c2 = mean_err_c2,
  median_err_c1 = median_err_c1,
  median_err_c2 = median_err_c2,
  skip = skip,
  min_length = min_length,
  alpha = alpha)

# Did the same for passages of length 100 and 200 and experimented with changing min length and
# skip parameters


library(latex2exp)

plot_df_long <- pivot_longer(
  data.frame(
    log_gamma= gamma_results$log_gamma,
    TPR= gamma_results$TPR,
    FPR = gamma_results$FPR
  ),
  cols = c(TPR, FPR),
  names_to = "Metric",
  values_to = "Rate"
)

ggplot(plot_df_long, aes(x = log_gamma, y = Rate, colour = Metric)) +
  geom_line(linewidth = 1) +
  
  # threshold line
  geom_vline(xintercept = log_gamma, linetype = "dashed") +
  
  annotate(
    "text",
    x = log_gamma, y = 0.5, 
    label = as.character(TeX("Chosen $\\gamma$")), angle = 90, vjust = -0.5, 
    size = 4,
    colour = "black",
    parse = TRUE 
  ) +
  
  labs(
    title = "Detection Trade-off as the Threshold Varies",
    x = expression(log(gamma)),
    y = "Rate",
    colour = "Metric"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal()



# Box plot for change point errors for 500 word ChatGPT passages

error_df <- data.frame(
  Error = c(err_c1, err_c2),
  Type = rep(c("Start Point", "End Point"), each = length(err_c1)))

error_df$Type <- factor(error_df$Type, levels = c("Start Point", "End Point"))


stats_df <- error_df %>%
  group_by(Type) %>%
  summarise(
    mean_error = 200,
    pct_above_mean = mean(Error > mean_error) * 100)

ggplot(error_df, aes(x = Type, y = Error, fill = Type)) +
  geom_boxplot(alpha = 0.7) +
  
  # Mean line
  geom_crossbar( data = stats_df, aes(x = Type, y = mean_error, ymin = mean_error, ymax = mean_error),
    width = 0.5,
    colour = "black",
    fatten = 0) +
  
  geom_text(data = stats_df,
    aes(x = Type, y = mean_error, label = paste0(round(pct_above_mean, 1), "% above 200 words")),
    vjust = -1,
    size = 4) +
  
  labs(
    title = "Change Point Localisation Errors for 500 Word ChatGPT",
    x = NULL,
    y = "Absolute Error (words)") +
  theme_minimal() +
  theme(legend.position = "none")


