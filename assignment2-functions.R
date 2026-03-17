

loadCorpusText <- function(filedir) {
  authornames <- list.files(filedir)
  # robust: ignore hidden entries and keep only directories
  authornames <- authornames[!grepl("^\\.", authornames)]
  authornames <- authornames[dir.exists(file.path(filedir, authornames))]
  
  booknames <- list()
  texts <- list()
  count <- 0
  
  for (i in 1:length(authornames)) {
    authordir <- file.path(filedir, authornames[i])
    
    files <- list.files(authordir)
    # robust: ignore hidden files and drop subdirectories
    files <- files[!grepl("^\\.", files)]
    files <- files[!file.info(file.path(authordir, files))$isdir]
    
    if (length(files) == 0) { next }
    
    booknames[[i]] <- character()
    texts[[i]] <- list()
    for (j in 1:length(files)) {
      path <- file.path(authordir, files[j])
      
      booknames[[i]] <- c(booknames[[i]], files[j])
      texts[[i]][[j]] <- paste(readLines(path, warn = FALSE), collapse = " ")
    }
  }
  return(list(texts = texts, booknames = booknames, authornames = authornames))
}


text_to_fw_indices <- function(text, func_words) {
  stopifnot(is.character(text), length(text) == 1)
  stopifnot(is.character(func_words), length(func_words) >= 1)
  
  K <- length(func_words)
  
  # Tokenise by whitespace exactly as in earlier word_spans() logic
  m <- gregexpr("\\S+", text, perl = TRUE)[[1]]
  if (length(m) == 1 && m[1] == -1) return(integer(0))
  lens <- attr(m, "match.length")
  tokens <- substring(text, m, m + lens - 1)
  
  # Normalisation function: lowercase, remove apostrophes, strip non-letters
  norm_token <- function(x) {
    x <- tolower(x)
    # remove common apostrophe characters (ASCII + “smart”)
    x <- gsub("[’`´']", "", x, perl = TRUE)
    # drop everything except letters (so punctuation/numbers disappear)
    x <- gsub("[^a-z]+", "", x, perl = TRUE)
    x
  }
  
  # Normalise function word list the same way (important!)
  fw_norm <- norm_token(func_words)
  
  # Build a fast lookup: name -> first index
  # (If duplicates exist after normalisation, keep the first.)
  lookup <- match(fw_norm, fw_norm)  # 1..K
  names(lookup) <- fw_norm
  
  # Normalise tokens
  tok_norm <- norm_token(tokens)
  
  # Map to indices; if empty token after cleaning or not found -> K+1
  idx <- rep.int(K + 1L, length(tok_norm))
  ok <- tok_norm != ""
  idx[ok] <- lookup[tok_norm[ok]]
  idx[is.na(idx)] <- K + 1L
  
  as.integer(idx)
}


#this function implements the logic of the multinomial change point algorithm from the lecture
#note that while the logic is the same, this code looks quite different since it is optimised
#to be fast, i.e. rather than computing dmultinomi from scratch every time we change tau, we 
#instead track running quantities. You do not need to worry about any of this, just trust that 
#the function is computing the same change point posterior you saw in the lecture.
cp_posterior<- function(x, human_theta, gpt_theta, min_length=200, skip = 10) {
  x <- as.integer(x)
  K <- length(human_theta)
  stopifnot(length(gpt_theta) == K)
  stopifnot(all(x >= 1L), all(x <= K))
  stopifnot(skip >= 1L)
  
  n <- length(x)
  
  # Candidate change points are allowed only at skip, 2*skip, ... (and must allow non-empty segments)
  cuts <- seq(skip, n - skip, by = skip)
  if (length(cuts) < 2) stop("Not enough candidate cut points; reduce skip or use longer text.")
  
  # Precompute per-token log probabilities under each regime
  # (this IS the multinomial/categorical likelihood: product theta[x_t], computed in log space)
  logp_H <- log(human_theta[x])
  logp_G <- log(gpt_theta[x])
  
  # Prefix sums so segment log-likelihoods are O(1)
  # S_H[t] = sum_{i=1}^t logp_H[i]
  S_H <- c(0, cumsum(logp_H))
  S_G <- c(0, cumsum(logp_G))
  
  segsum <- function(S, a, b) {
    # sum_{t=a}^b logp[t], inclusive; assumes 1 <= a <= b <= n
    S[b + 1L] - S[a]
  }
  
  # Enumerate all allowed (c1, c2) with c1 < c2
  pairs <- expand.grid(c1 = cuts, c2 = cuts, KEEP.OUT.ATTRS = FALSE)
  pairs <- pairs[pairs$c1 < pairs$c2, , drop = FALSE]
  pairs <- pairs[(pairs$c2 - pairs$c1) >= min_length, , drop = FALSE]
  
  rownames(pairs) <- NULL
  
  loglik <- numeric(nrow(pairs))
  
  if (nrow(pairs) == 0) stop("No admissible (c1,c2) pairs: reduce min_length or skip, or use longer texts.")
  
  for (i in seq_len(nrow(pairs))) {
    c1 <- pairs$c1[i]
    c2 <- pairs$c2[i]
    
    # Human segment: 1..c1
    ll1 <- S_H[c1 + 1L] - S_H[1L]
    
    # GPT segment: (c1+1)..c2
    ll2 <- segsum(S_G, c1 + 1L, c2)
    
    # Human segment: (c2+1)..n  (if c2 == n, this would be empty; our cuts avoid that)
    ll3 <- segsum(S_H, c2 + 1L, n)
    
    loglik[i] <- ll1 + ll2 + ll3
  }
  
  # Posterior ∝ exp(loglik), normalised (log-sum-exp for stability)
  m <- max(loglik)
  w <- exp(loglik - m)
  post <- w / sum(w)
  
  out <- cbind(pairs, loglik = loglik, posterior = post)
  out <- out[order(out$posterior, decreasing = TRUE), ]
  rownames(out) <- NULL
  out
}

sequenceLikelihood <- function(x, theta) {
  x <- as.integer(x)
  sum(log(theta[x]))
}

logmeanexp <- function(logv) {
  n <- length(logv)
  if (n == 0) stop("logv must have length > 0")
  m <- max(logv)
  if (!is.finite(m)) return(m)  # all -Inf -> -Inf
  m + log(mean(exp(logv - m)))
}


# ROC curve
auroc <- function(H0_human, H1_human, H0_mixed, H1_mixed) {
  score_h <- as.numeric(H1_human - H0_human)
  score_m <- as.numeric(H1_mixed - H0_mixed)
  
  n0 <- length(score_h)
  n1 <- length(score_m)
  if (n0 == 0 || n1 == 0) stop("Need at least one human and one mixed score.")
  
  r <- rank(c(score_h, score_m), ties.method = "average")
  R1 <- sum(r[(n0 + 1):(n0 + n1)])  # ranks of mixed
  as.numeric((R1 - n1 * (n1 + 1) / 2) / (n0 * n1))
}

