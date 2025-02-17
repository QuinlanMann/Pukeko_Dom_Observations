library(tidyverse)

DS_HAY1_N %>% remove_rownames %>% column_to_rownames(var="Dominant")

test_COl<-function(x){
  x<-x%>%
   remove_rownames%>%
    column_to_rownames(var = "Dominant")
}

list_of_objects<-lapply(list_of_objects, test_COl)
NA_2_0<-function(x)
{x[is.na(x)<-0]}

lapply(list_of_objects, NA_2_0)
list_of_objects[is.na(list_of_objects)]<-0

list_of_objects<-lapply(list_of_objects, function(x) replace(x, is.na(x), 0))


## set up storage matrix
# get names for row and columns
matrix_maker<-function(x){
  # construct 0 matrix of correct dimensions with row and column names
  myMat <- matrix(0, 
                  length(sort(unique(unlist(x[1:2])))), 
                  length(sort(unique(unlist(x[1:2])))), 
                  dimnames = list(sort(unique(unlist(x[1:2]))), 
                                  sort(unique(unlist(x[1:2])))))
  # fill in the matrix with matrix indexing on row and column names
  myMat[as.matrix(x[c("Dominant", "Subordinate")])] <- x[["count"]]
}
matrixces<-lapply(list_of_objects, matrix_maker)

matrixces$DS_EAST_CAMP_SE
