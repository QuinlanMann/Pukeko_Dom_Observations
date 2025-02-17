MyFunc <- function(x) {
  reshape2::dcast(data = x, formula = Dominant~Subordinate, value.var = "count")
}
list_of_objects <- lapply(X = list_of_objects, FUN = MyFunc)

col2name<-function(x){
  x<-x%>%
    remove_rownames%>%
    column_to_rownames(var = "Dominant")
}

list_of_objects<-lapply(list_of_objects, col2name)

list_of_objects<-lapply(list_of_objects, function(x) replace(x, is.na(x), 0))

list2env(list_of_objects, .GlobalEnv)