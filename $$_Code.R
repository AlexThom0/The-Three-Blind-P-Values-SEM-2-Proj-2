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
for(i in 1:length(mixedtexts$texts))
  for(j in 1:length(mixedtexts$texts[[i]])){


    y <- text_to_fw_indices(mixedtexts$texts[[i]][[j]],functionwords)
    cps <- cp_posterior(y, humantheta, GPTtheta, skip = skip, min_length =min_length )
    est <- cps[1,][1:2]
    c = c + 1
    true <- truecps$texts[[i]][[j]]
    result <- paste('Est. change points are ', toString(est), ". True change point is", true, ". Count =",c)
    print(result)}

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
