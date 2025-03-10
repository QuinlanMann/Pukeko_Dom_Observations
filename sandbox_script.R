#Bootstrappign
library(tidyverse)
library(EloRating)

doms<-read.csv("C:\\Users\\quinl\\Desktop\\Book1.csv")

set.seed(19970227) ## for reproducibility

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
  idx=sample(1:N2, N2, replace = T)
  doms5=doms4[idx,]
  
  doms5<-doms5%>%
    group_by(Group, Dominant, Subordinate)%>%
    summarize(count=length(Interaction))
  
  boot.NOBA2[i]=ifelse(length(subset(DS(matrix_maker(doms5[,c(2:4)])), ID=="NOBA")$normDS) == 0, 
                       NA, (subset(DS(matrix_maker(doms5[,c(2:4)])), ID=="NOBA")$normDS))
  boot.BYYN2[i]=ifelse(length(subset(DS(matrix_maker(doms5[,c(2:4)])), ID=="BYYN")$normDS) == 0, 
                       NA, (subset(DS(matrix_maker(doms5[,c(2:4)])), ID=="BYYN")$normDS))
  boot.YRGY2[i]=ifelse(length(subset(DS(matrix_maker(doms5[,c(2:4)])), ID=="YRGY")$normDS) == 0, 
                       NA, (subset(DS(matrix_maker(doms5[,c(2:4)])), ID=="YRGY")$normDS))
  boot.XNRB2[i]=ifelse(length(subset(DS(matrix_maker(doms5[,c(2:4)])), ID=="XNRB")$normDS) == 0, 
                       NA, (subset(DS(matrix_maker(doms5[,c(2:4)])), ID=="XNRB")$normDS))
  boot.YBYG2[i]=ifelse(length(subset(DS(matrix_maker(doms5[,c(2:4)])), ID=="YBYG")$normDS) == 0, 
                       NA, (subset(DS(matrix_maker(doms5[,c(2:4)])), ID=="YBYG")$normDS))
  boot.RGBG2[i]=ifelse(length(subset(DS(matrix_maker(doms5[,c(2:4)])), ID=="RGBG")$normDS) == 0, 
                       NA, (subset(DS(matrix_maker(doms5[,c(2:4)])), ID=="RGBG")$normDS))
}


boot.vals<-data.frame(
  boot= c(rep(c("with"), B), rep(c('w/out'), B)), 
  byyn=c(boot.BYYN, boot.BYYN2), 
  noba=c(boot.NOBA, boot.NOBA2),
  rgbg=c(boot.RGBG, boot.RGBG2), 
  yrgy=c(boot.YRGY, boot.YRGY2), 
  ybyg=c(boot.YBYG, boot.YBYG2), 
  xnrb=c(boot.XNRB, boot.XNRB2)
)

min(na.omit(unlist(boot.vals[c(2:7)])))
max(na.omit(unlist(boot.vals[c(2:7)])))

theme_set(ggthemes::theme_few())
ggpubr::ggarrange(
  ggplot(boot.vals, aes(x=byyn, fill=boot))+
    geom_density(alpha=0.5)+
    geom_vline(xintercept = subset(DS(matrix_maker(doms3[,c(2:4)])), ID=="BYYN")$normDS, 
               linetype="dashed", size=1)+
    scale_x_continuous(limits = c(0.9, 3.92))+
    labs(subtitle = "BY/YN", x="David's Score"), 
  ggplot(boot.vals, aes(x=noba, fill=boot))+
    geom_density(alpha=.5)+
    geom_vline(xintercept = subset(DS(matrix_maker(doms3[,c(2:4)])), ID=="NOBA")$normDS, 
               linetype="dashed", size=1)+
    scale_x_continuous(limits = c(0.9, 3.92))+
    labs(subtitle = "NOBA", x="David's Score"), 
  ggplot(boot.vals, aes(x=rgbg, fill=boot))+
    geom_density( alpha=.5)+
    geom_vline(xintercept = subset(DS(matrix_maker(doms3[,c(2:4)])), ID=="RGBG")$normDS, 
               linetype="dashed", size=1)+
    scale_x_continuous(limits = c(0.9, 3.92))+
    labs(subtitle = "RG/BG", x="David's Score"), 
  ggplot(boot.vals, aes(x=xnrb, fill=boot, alpha=.5))+
    geom_density()+
    geom_vline(xintercept = subset(DS(matrix_maker(doms3[,c(2:4)])), ID=="XNRB")$normDS, 
               linetype="dashed", size=1)+
    scale_x_continuous(limits = c(0.9, 3.92))+
    labs(subtitle = "XN/RB", x="David's Score"), 
  ggplot(boot.vals, aes(x=ybyg, fill=boot, alpha=.5))+
    geom_density()+
    geom_vline(xintercept = subset(DS(matrix_maker(doms3[,c(2:4)])), ID=="YBYG")$normDS, 
               linetype="dashed", size=1)+
    scale_x_continuous(limits = c(0.9, 3.92))+
    labs(subtitle = "YB/YG", x="David's Score"), 
  ggplot(boot.vals, aes(x=yrgy, fill=boot, alpha=.5))+
    geom_density()+
    geom_vline(xintercept = subset(DS(matrix_maker(doms3[,c(2:4)])), ID=="YRGY")$normDS, 
               linetype="dashed", size=1)+
    scale_x_continuous(limits = c(0.9, 3.92))+
    labs(subtitle = "YR/GY", x="David's Score"), 
  ncol=2, 
  nrow=3, 
  common.legend = T
)

df1<-DS(matrix_maker(doms3[,c(2:4)]))

df<-as.data.frame(lapply(subset(boot.vals, boot=="w/out")[,c(2:7)], 
       FUN=mean, na.rm=T))

df<-tidyr::pivot_longer(df, 
                    cols = everything())
df$name<-toupper(df$name)
names(df)[names(df) == "name"] <- "ID"
names(df)[names(df) == "value"] <- "Boot_Mean"

df2<-merge(df1, df, by=c("ID"))

ggplot(df2, aes(Boot_Mean, normDS, label=ID))+
  geom_text(check_overlap = T, hjust = 0, nudge_x = 0.05)+
  geom_point()+
  geom_abline(intercept=0, slope=1, linetype="dashed")+
  scale_x_continuous(limits=c(1.5, 3.75))+
  scale_y_continuous(limits=c(1.5, 3.75))+
  labs(x="Mean of Bootstrapped David's\nscores without copulations", 
       y="Calculated David's scores\nfrom complete dataset")+
  ggthemes::theme_clean()

doms4<-doms4%>%
  group_by(Group, Dominant, Subordinate)%>%
  summarize(count=length(Interaction))

df3<-DS(matrix_maker(doms4[,c(2:4)]))
df2<-merge(df2, df3, by=c("ID"))

ggplot(df2, aes(normDS.y, normDS.x, label=ID))+
  geom_text(check_overlap = T, hjust = 0, nudge_x = 0.05)+
  geom_point()+
  geom_abline(intercept=0, slope=1, linetype="dashed")+
  scale_x_continuous(limits=c(1.5, 3.75))+
  scale_y_continuous(limits=c(1.5, 3.75))+
  labs(x="Calculated David's scores\nwithout copulations", 
       y="Calculated David's scores\nfrom complete dataset")+
  ggthemes::theme_clean()

df2$rank<-rank(-df2$normDS.x)
df2$rank.y<-rank(-df2$normDS.y)

plot(df2$normDS.x~df2$rank)
abline(lm(df2$normDS.x~df2$rank))

summary((lm(df2$normDS.x~df2$rank)))

plot(df2$normDS.y~df2$rank.y)
abline(lm(df2$normDS.y~df2$rank.y))

summary((lm(df2$normDS.y~df2$rank.y)))

David_Score2<-David_Score

David_Score<-David_Score %>%
  group_by(Group) %>%
  mutate(good_ranks = order(order(normDS, decreasing=TRUE)))

library(ggpmisc)

ggplot(David_Score2, aes(x=good_ranks, y=normDS))+
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point()+
  facet_wrap(~Group)

ggplot(David_Score, aes(y=normDS, good_ranks, color=Group))+
  geom_point(show.legend = FALSE, aes(shape=Group))+
  geom_smooth(method="lm", show.legend = FALSE)

list_of_Scores = split(David_Score, David_Score$Group)

regression_maker = function(data, var1, var2) {
  fit = lm(normDS ~ good_ranks, data = data)
  coef(fit)[2]
  }

regression_maker(David_Score)

slopes<-map_dfr(list_of_Scores, regression_maker, .id = "id")
slopes<-rename(slopes, Slopes="good_ranks")


DS(bonobos)
bonobos<-bonobos
bonobos2<-DS(bonobos)
bonobos2$dush_norm<-dush_norm <- bonobos2$DS/length(bonobos2$ID)
bonobos2$`bounded_normalization(0 - 1)`<-(bonobos2$DS+((length(bonobos2$ID)*(length(bonobos2$ID)-1)/2)))/(length(bonobos2$ID)*(length(bonobos2$ID)-1))
bonobos2$`bounded_normalization(- - +)`<-bonobos2$DS/((length(bonobos2$ID)*(length(bonobos2$ID)-1)/2))
bonobos2$ranks<-c(1:7)

bonobos2
plot(data=bonobos2, dush_norm~ranks)

trial<-matrix(data=c(0,0,0,0,0,0,0,
                     1,0,0,0,0,0,0,
                     1,1,0,0,0,0,0,
                     1,1,1,0,0,0,0,
                     1,1,1,1,0,0,0,
                     1,1,1,1,1,0,0,
                     1,1,1,1,1,1,0),
              nrow=7, 
              ncol =7, 
              dimnames = list(c("a","b","c","d","e","f","g"), c("a","b","c","d","e","f","g")))

ds<-DS(trial, prop="Pij")
((length(ds$ID)*(length(ds$ID)-1))/2)/2
ds
ds$rank<-rank(-ds$DS)
ds
plot(data=ds, normDS~rank)
summary(lm(data=ds, normDS~rank))

library(EloRating)
bonobos

DSPij<-function(x){
  EloRating::DS(x, prop = c("Pij"))
}

DS(bonobos, prop = 'Pij')
DSPij(bonobos)


list_of_objects2<-lapply(list_of_objects, DSPij)
David_Score2<-bind_rows(list_of_objects2, .id = "Group")
David_Score2<-David_Score2 %>%
  group_by(Group) %>%
  mutate(good_ranks = order(order(normDS, decreasing=TRUE)))
library(ggpmisc)

ggplot(David_Score2, aes(x=good_ranks, y=normDS))+
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point()+
  facet_wrap(~Group)+
  ggthemes::theme_few()



#trying things
WCNW<-readxl::read_xlsx("C:\\Users\\quinl\\Downloads\\Observation\\Observation\\WEST CAMP NW.xlsx")

WCNW$Dominant[WCNW$Dominant == 'YWBW'] <- 'YNBN'
WCNW$Subordinate[WCNW$Subordinate == 'YWBW'] <- 'YNBN'
WCNW$Dominant[WCNW$Dominant == 'YNBW'] <- 'YNBN'
WCNW$Subordinate[WCNW$Subordinate == 'YNBW'] <- 'YNBN'
WCNW$Dominant[WCNW$Dominant == 'YNBB'] <- 'YNBN'
WCNW$Subordinate[WCNW$Subordinate == 'YNBB'] <- 'YNBN'
WCNW$Dominant[WCNW$Dominant == 'BBYG'] <- 'YGBB'
WCNW$Subordinate[WCNW$Subordinate == 'BBYG'] <- 'YGBB'
WCNW$Dominant[WCNW$Dominant == 'R-RMBW'] <- 'B-RMBW'
WCNW$Subordinate[WCNW$Subordinate == 'R-RMBW'] <- 'B-RMBW'

#WCNW<-subset(WCNW, Subordinate!= "YWBW" & Dominant !="YWBW")[,-c(1)]

WCNW<-WCNW[,-c(1)]%>%
  group_by(Dominant, Subordinate)%>%
  summarize(count=length(`Interaction (P=Posture, D=Displace/Avoid, C=Charge, B=Bite/Kick)`))

DS(matrix_maker(WCNW))
