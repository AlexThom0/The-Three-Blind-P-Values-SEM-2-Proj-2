# Burgher Code

functionwords <- readLines("~/Desktop/SCS/SCS Final/wordfile200 (1).txt") #function word list
humantexts <- loadCorpusText("~/Desktop/SCS/SCS Final/essays/human/")

GPTtexts <- loadCorpusText("~/Desktop/SCS/SCS Final/essays/GPT500/")
mixedtexts <- loadCorpusText("~/Desktop/SCS/SCS Final/essays/mixed500/")
truecps <- loadCorpusText("~/Desktop/SCS/SCS Final/essays/changepoints500/")

get_map_cps <- function(text, humantheta, GPTtheta, functionwords,
                        skip = 50, min_length = 100) {
  
  y <- text_to_fw_indices(text, functionwords)
  
  cps <- cp_posterior(y, humantheta, GPTtheta,
                      skip = skip,
                      min_length = min_length)
  
  # Return most likely change points (first row)
  c(tau1_hat = cps$c1[1],
    tau2_hat = cps$c2[1])
}



skip <- 50
min_length <- 100

estimated_cps <- matrix(NA, nrow = nrow(truechangepoints), ncol = 2)

row_index <- 1

for (i in 1:length(mixedtexts$texts)) {
  for (j in 1:length(mixedtexts$texts[[i]])) {
    
    text <- mixedtexts$texts[[i]][[j]]
    
    est <- get_map_cps(text,
                       humantheta,
                       GPTtheta,
                       functionwords,
                       skip = skip,
                       min_length = min_length)
    
    estimated_cps[row_index, ] <- est
    
    row_index <- row_index + 1
  }
}




start_error <- abs(estimated_cps[,1] - truechangepoints[,1])
end_error   <- abs(estimated_cps[,2] - truechangepoints[,2])

mean_start_error <- mean(start_error)
mean_end_error   <- mean(end_error)

mean_start_error
mean_end_error



k <- 1
truechangepoints[k, ]
estimated_cps[k, ]


summary(start_error)
summary(end_error)

mean(start_error > 500)
mean(end_error > 500)
