# $$ Code

functionwords <- readLines("~/Desktop/Uni/Stats Case Studies/Stylo/The-Three-Blind-P-Values-SEM-2-Proj-2/wordfile200 (1).txt") #function word list
humantexts <- loadCorpusText("~/Desktop/Uni/Stats Case Studies/Stylo/The-Three-Blind-P-Values-SEM-2-Proj-2/essays/human/")

GPTtexts <- loadCorpusText("~/Desktop/Uni/Stats Case Studies/Stylo/The-Three-Blind-P-Values-SEM-2-Proj-2/essays/GPT500/")
mixedtexts <- loadCorpusText("~/Desktop/Uni/Stats Case Studies/Stylo/The-Three-Blind-P-Values-SEM-2-Proj-2/essays/mixed500/")
truecps <- loadCorpusText("~/Desktop/Uni/Stats Case Studies/Stylo/The-Three-Blind-P-Values-SEM-2-Proj-2/essays/changepoints500/")

data <- load("~/Desktop/Uni/Stats Case Studies/Stylo/The-Three-Blind-P-Values-SEM-2-Proj-2/LLM_Project_Data.RData")


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
c = 0
errors = 0
tolerance = 25

# Helper to safely convert "1000,1474" → c(1000, 1474)
parse_true <- function(x) {
  as.numeric(strsplit(x, ",")[[1]])
}

results <- lapply(seq_along(mixedtexts$texts), function(i) {
  lapply(seq_along(mixedtexts$texts[[i]]), function(j) {
    
    c <<- c+1
    print(paste('Completed ',c, ' of ', length(mixedtexts$texts)))
    
    y <- text_to_fw_indices(mixedtexts$texts[[i]][[j]], functionwords)
    cps <- cp_posterior(y, humantheta, GPTtheta,
                        skip = skip, min_length = min_length)
    
    est <- as.numeric(cps[1, 1:2])
    true <- parse_true(truecps$texts[[i]][[j]])
    
    # Guard against bad values
    if (length(true) < 2 || any(is.na(est)) || any(is.na(true))) {
      return(NA)
    }
    
    # Return error contribution
    if (abs(est[1] - true[1]) > tolerance || abs(est[2] - true[2]) > tolerance) {
      return(abs(est[1] - true[1]) + abs(est[2] - true[2]))
    } else {
      return(0)
    }
  })
})

# Flatten and sum
errors <- sum(unlist(results), na.rm = TRUE)

cat("Total error:", errors, "\n")


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



###### Figuring out training and test split stuff





# Calculate mean frequencies for each word
human_means <- colMeans(humancounts)
gpt_means <- colMeans(GPTcounts)

# Identify the indices of the most used words
top_indices <- order(human_means, decreasing = TRUE)[2:11]

# Create a comparison table
top_words_comparison <- data.frame(
  Word = functionwords[top_indices],
  Human_Avg = (round(human_means[top_indices], 4)/sum(human_means))*100,
  GPT_Avg = (round(gpt_means[top_indices], 4)/sum(gpt_means))*100
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

# MDS PLOT

human_ratios <- humancounts / rowSums(humancounts)
gpt_ratios   <- GPTcounts / rowSums(GPTcounts)

# Combine data and run MDS
combined_counts <- rbind(human_ratios, gpt_ratios)
d <- dist(combined_counts)
pts <- cmdscale(d)

library(ggplot2)

# 1. Prepare the data
df <- data.frame(
  MDS1 = pts[, 1],
  MDS2 = pts[, 2],
  Author = c(rep("Human", nrow(humancounts)), rep("GPT", nrow(GPTcounts)))
)

# 2. Create the plot
ggplot(df, aes(x = MDS1, y = MDS2, color = Author)) +
  geom_point(alpha = 0.5, size = 0.5) + # Added slight transparency for overlapping points
  scale_color_manual(values = c("Human" = "steelblue4", "GPT" = "orangered4")) +
  labs(
    title = "Style Clustering (MDS)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal() + # Clean background
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold") # Centers and bolds the title
  )

library(ggplot2)
