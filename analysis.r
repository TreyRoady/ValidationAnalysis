#Takes argument 'k', the signal set to be tested
#returns a .csv with the top 10 signal sets and their score
#Run in folder with 'mel_total_mat.csv' & 'sync_total_mat.csv'


#Load participant ratings
#Rows and columns are index matched on Signal 1- Signal 10
#Negative score favors X
#Positive score favors y
mel_rating <- read.csv('mel_total_mat.csv', header = FALSE)
sync_rating <- read.csv('sync_total_mat.csv', header = FALSE)

library('gtools') #For permutations()

#Create R scoring function
#Takes array of signals and returns the score
score <- function(arg1, arg2){ 
  #arg1: 10x10 array of participant ratings
  #arg2: 1xn array of signals, least to greatest
  sum <- 0
  #i for each entry in arg2, except the last
  for(i in 1:(length(arg2)-1)){
    #j for each entry, after i, in arg2, loop through
    for(j in (i+1):(length(arg2))){
      sum <- sum + arg1[arg2[i],arg2[j]]
    }
  }
  return(sum)
}


perms <- permutations(10,5) #load all permutations for 3 conditions

#for each perm, calculate a score
sync_scores<-NULL
mel_scores <-NULL

for(i in 1:nrow(perms)){ #for each row in perms, calculate a score
  sync_scores[i] <- score(sync_rating, perms[i,])
  mel_scores[i] <- score(mel_rating, perms[i,])
}

#Sort scores and return the indices of the new ordering
mel <- list(Perm=perms[tail(sort(mel_scores,index.return=TRUE)$ix, 10),], Scores=tail(sort(mel_scores), 10))
sync <- list(Perm=perms[tail(sort(sync_scores,index.return=TRUE)$ix, 10),],Scores= tail(sort(sync_scores), 10))

write.table(mel, "mel5.txt", col.names = TRUE, row.names=FALSE, sep="\t")
write.table(sync, "sync5.txt", col.names = TRUE, row.names=FALSE, sep="\t")
