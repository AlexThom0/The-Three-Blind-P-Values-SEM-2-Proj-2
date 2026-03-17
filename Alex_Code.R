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
