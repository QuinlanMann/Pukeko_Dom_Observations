reg_randomizer<-function(x){
  #quick check to see if the matrix is square
  x<-as.matrix(x)
  if(length(rownames(x))!=length(colnames(x)))
    return("the matrix is not square")
  #adds the values of the matrix, and transposed matrix together to get
  #the sum of interactions
  x<-x+t(x)
  #removes the lower half so I am not adding the sum twice
  x[lower.tri(x)] <- 0  
  x<-as.data.frame(x)
  #creates the winner ID column so I can make the dataframe long
  x<-tibble::rownames_to_column(x, "BIRD1")
  #unique total value of columns per matrix
  N<-length(colnames(x))
  #creates a unique dataframe that has a list of all interaction pairs (totals)
  x<-pivot_longer(as.data.frame(x), 
                  cols = c(2:N), 
                  names_to = "BIRD2", 
                  values_to = "INT")
  x<-filter(x, INT > 0)
  #creating a random number of wins/losses conditional on how many interactions
  x$WIN<-sapply(x$INT, function(y) sample(0:y, 1))
  x$LOSS<-x$INT-x$WIN
  #creating two dataframes of wins and losses
  wins<-x[,c(1,2,4)]
  wins<-wins%>%
    rename(Dominant=BIRD1, 
           Subordinate=BIRD2, 
           count=WIN)
  loss<-x[,c(2,1,5)]
  loss<-loss%>%
    rename(Dominant=BIRD2, 
           Subordinate=BIRD1, 
           count=LOSS)
  #binding the new dataframes
  x<-rbind(wins,loss)
  #making them back to a matrix
  myMat <- matrix(0, #filled with zeros
                  length(sort(unique(unlist(x[1:2])))), #length of all unique bird name values in rows and columns
                  length(sort(unique(unlist(x[1:2])))), #length of all unique bird name values in rows and columns
                  dimnames = list(sort(unique(unlist(x[1:2]))), #naming rows and columns after all unique bird name values in dataframe
                                  sort(unique(unlist(x[1:2])))))
  # fill in the matrix with matrix indexing on row and column names
  myMat[as.matrix(x[c("Dominant", "Subordinate")])] <- x[["count"]]
  #DS calculation from EloRating Package
  rn <- rownames(myMat)
  cn <- colnames(myMat)
  tmat <- t(myMat)
  summatrix <- myMat + tmat
  diag(summatrix) <- 0
  summatrix <- replace(summatrix, summatrix == 0, NA)
  l1 <- which(is.na(summatrix), arr.ind = TRUE)
  #this is using the Dij calculation change if you decide to use the Pij calculation
  propmatrix <- (myMat + 0.5)/(tmat + myMat + 1)
  propmatrix <- replace(propmatrix, l1, 0)
  w <- rowSums(propmatrix)
  w2 <- propmatrix %*% w
  l <- rowSums(t(propmatrix))
  l2 <- t(propmatrix) %*% l
  DS <- w + w2 - l - l2
  normDS <- ((DS + ((length(DS)) * (length(DS) - 1))/2))/length(DS)
  good_ranks <- order(order(normDS, decreasing=TRUE))
  res <- data.frame(ID = rn, DS = DS, normDS = normDS, good_ranks = good_ranks)
  res <- res[order(res$DS, decreasing = TRUE), ]
  rownames(res) <- NULL
  #return(res)
  r2<-summary(lm(normDS ~ good_ranks, data = res))[["coefficients"]][2]
  return(r2)
}

results <- data.frame(matrix(ncol = length(list_of_objects2), nrow = 1000))
x <- unique(ls(list_of_objects2))
colnames(results) <- x

for(i in 1:length(list_of_objects2))
{
  values <- list_of_objects2[[i]]
  results[, i] <- as.numeric(replicate(1000, 
                                  reg_randomizer(values), 
                                  simplify = FALSE))
  
}

results<-tibble::rownames_to_column(results, "iteration")
