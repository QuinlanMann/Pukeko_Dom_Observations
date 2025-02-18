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

#Bootstrappign
library(tidyverse)
library(EloRating)

doms<-read.csv("C:\\Users\\quinl\\Desktop\\Book1.csv")

set.seed(101) ## for reproducibility

matrix_maker<-function(x){
  # construct 0 matrix of correct dimensions with row and column names
  myMat <- matrix(0, #filled with zeros
                  length(sort(unique(unlist(x[1:2])))), #length of all unique bird name values in rows and columns
                  length(sort(unique(unlist(x[1:2])))), #length of all unique bird name values in rows and columns
                  dimnames = list(sort(unique(unlist(x[1:2]))), #naming rows and columns after all unique bird name values in dataframe
                                  sort(unique(unlist(x[1:2])))))
  # fill in the matrix with matrix indexing on row and column names
  myMat[as.matrix(x[c("Dominant", "Subordinate")])] <- x[["count"]]
  return(myMat)
}


#doms<-doms%>%
#group_by(Group, Dominant, Subordinate)%>%
#summarize(count=length(Interaction))



N=nrow(doms)
B=999
boot.NOBA<-rep(0, B)
boot.BYYN<-rep(0, B)
boot.YRGY<-rep(0, B)
boot.XNRB<-rep(0, B)
boot.YBYG<-rep(0, B)
boot.RGBG<-rep(0, B)

for(i in 1:B){
  idx=sample(1:N, N, replace = T)
  doms2=doms[idx,]
  
  doms2<-doms2%>%
    group_by(Group, Dominant, Subordinate)%>%
    summarize(count=length(Interaction))
  
  boot.NOBA[i]=subset(DS(matrix_maker(doms2[,c(2:4)])), ID=="NOBA")$normDS
  boot.BYYN[i]=subset(DS(matrix_maker(doms2[,c(2:4)])), ID=="BYYN")$normDS
  boot.YRGY[i]=subset(DS(matrix_maker(doms2[,c(2:4)])), ID=="YRGY")$normDS
  boot.XNRB[i]=subset(DS(matrix_maker(doms2[,c(2:4)])), ID=="XNRB")$normDS
  boot.YBYG[i]=subset(DS(matrix_maker(doms2[,c(2:4)])), ID=="YBYG")$normDS
  boot.RGBG[i]=subset(DS(matrix_maker(doms2[,c(2:4)])), ID=="RGBG")$normDS
  
}

doms3<-doms%>%
  group_by(Group, Dominant, Subordinate)%>%
  summarize(count=length(Interaction))

DS(matrix_maker(doms3[,c(2:4)]))
subset(DS(matrix_maker(doms3[,c(2:4)])), ID=="BYYN")$normDS

theme_set(ggthemes::theme_clean())
gridExtra::grid.arrange(
  (ggplot(as.data.frame(boot.BYYN), aes(x=boot.BYYN))+
     geom_density()+
     geom_vline(xintercept = subset(DS(matrix_maker(doms3[,c(2:4)])), ID=="BYYN")$normDS)),
  (ggplot(as.data.frame(boot.NOBA), aes(x=boot.NOBA))+
     geom_density()+
     geom_vline(xintercept = subset(DS(matrix_maker(doms3[,c(2:4)])), ID=="NOBA")$normDS)),
  (ggplot(as.data.frame(boot.RGBG), aes(x=boot.RGBG))+
     geom_density()+
     geom_vline(xintercept = subset(DS(matrix_maker(doms3[,c(2:4)])), ID=="RGBG")$normDS)),
  (ggplot(as.data.frame(boot.YRGY), aes(x=boot.YRGY))+
     geom_density()+
     geom_vline(xintercept = subset(DS(matrix_maker(doms3[,c(2:4)])), ID=="YRGY")$normDS)),
  (ggplot(as.data.frame(boot.XNRB), aes(x=boot.XNRB))+
     geom_density()+
     geom_vline(xintercept = subset(DS(matrix_maker(doms3[,c(2:4)])), ID=="XNRB")$normDS)),
  (ggplot(as.data.frame(boot.YBYG), aes(x=boot.YBYG))+
     geom_density()+
     geom_vline(xintercept = subset(DS(matrix_maker(doms3[,c(2:4)])), ID=="YBYG")$normDS)),
  ncol=3)

doms4<-subset(doms, note!="C")

boot.NOBA2<-rep(0, B)
boot.BYYN2<-rep(0, B)
boot.YRGY2<-rep(0, B)
boot.XNRB2<-rep(0, B)
boot.YBYG2<-rep(0, B)
boot.RGBG2<-rep(0, B)

N2<-nrow(doms4)

for(i in 1:B){
  idx=sample(1:N2, N, replace = T)
  doms5=doms4[idx,]
  
  doms5<-doms5%>%
    group_by(Group, Dominant, Subordinate)%>%
    summarize(count=length(Interaction))
  
  boot.NOBA2[i]=subset(DS(matrix_maker(doms5[,c(2:4)])), ID=="NOBA")$normDS
  boot.BYYN2[i]=subset(DS(matrix_maker(doms5[,c(2:4)])), ID=="BYYN")$normDS
  boot.YRGY2[i]=subset(DS(matrix_maker(doms5[,c(2:4)])), ID=="YRGY")$normDS
  boot.XNRB2[i]=subset(DS(matrix_maker(doms5[,c(2:4)])), ID=="XNRB")$normDS
  boot.YBYG2[i]=subset(DS(matrix_maker(doms5[,c(2:4)])), ID=="YBYG")$normDS
  boot.RGBG2[i]=subset(DS(matrix_maker(doms5[,c(2:4)])), ID=="RGBG")$normDS
  
}

ggplot(as.data.frame(boot.BYYN2), aes(x=boot.BYYN2))+
  geom_density()+
  geom_vline(aes(xintercept = quantile(boot.BYYN2, prob = c(0.025, 0.975))))

quantile(boot.BYYN2, prob = c(0.025, 0.975))

gridExtra::grid.arrange(
  (ggplot(as.data.frame(boot.BYYN2), aes(x=boot.BYYN2))+
     geom_density()+
     geom_vline(xintercept = subset(DS(matrix_maker(doms3[,c(2:4)])), ID=="BYYN")$normDS)),
  (ggplot(as.data.frame(boot.NOBA2), aes(x=boot.NOBA2))+
     geom_density()+
     geom_vline(xintercept = subset(DS(matrix_maker(doms3[,c(2:4)])), ID=="NOBA")$normDS)),
  (ggplot(as.data.frame(boot.RGBG2), aes(x=boot.RGBG2))+
     geom_density()+
     geom_vline(xintercept = subset(DS(matrix_maker(doms3[,c(2:4)])), ID=="RGBG")$normDS)),
  (ggplot(as.data.frame(boot.YRGY2), aes(x=boot.YRGY2))+
     geom_density()+
     geom_vline(xintercept = subset(DS(matrix_maker(doms3[,c(2:4)])), ID=="YRGY")$normDS)),
  (ggplot(as.data.frame(boot.XNRB2), aes(x=boot.XNRB2))+
     geom_density()+
     geom_vline(xintercept = subset(DS(matrix_maker(doms3[,c(2:4)])), ID=="XNRB")$normDS)),
  (ggplot(as.data.frame(boot.YBYG2), aes(x=boot.YBYG2))+
     geom_density()+
     geom_vline(xintercept = subset(DS(matrix_maker(doms3[,c(2:4)])), ID=="YBYG")$normDS)),
  ncol=3)