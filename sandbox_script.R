#this code is all supplementary versions
#if anything is in here it may be used at a later date/time
#all code will eventually be deleted or moved to the main script "domobs.Rmd"

#Bootstrapping Stuff-----
library(tidyverse)
library(EloRating)

doms<-read.csv("C:\\Users\\quinl\\Desktop\\Book1.csv")

set.seed(19970227) ## for reproducibility

#this is all bootstrapping to see about including the weird copulations-----
# this may be brought back for a future analysis but right now the decision is to keep it
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

indiv.relate<-DS(matrix_maker(doms4[,c(2:4)]))
df2<-merge(df2, indiv.relate, by=c("ID"))

ggplot(df2, aes(normDS.y, normDS.x, label=ID))+
  geom_text(check_overlap = T, hjust = 0, nudge_x = 0.05)+
  geom_point()+
  geom_abline(intercept=0, slope=1, linetype="dashed")+
  scale_x_continuous(limits=c(1.5, 3.75))+
  scale_y_continuous(limits=c(1.5, 3.75))+
  labs(x="Calculated David's scores\nwithout copulations", 
       y="Calculated David's scores\nfrom complete dataset")+
  ggthemes::theme_clean()

#this was me playing with DS calculations and may come back to the normalization technique used-----
DS(bonobos)
bonobos<-bonobos
bonobos2<-DS(bonobos)
bonobos2$dush_norm<-dush_norm <- bonobos2$DS/length(bonobos2$ID)
bonobos2$`bounded_normalization(0 - 1)`<-(bonobos2$DS+((length(bonobos2$ID)*(length(bonobos2$ID)-1)/2)))/(length(bonobos2$ID)*(length(bonobos2$ID)-1))
bonobos2$`bounded_normalization(- - +)`<-bonobos2$DS/((length(bonobos2$ID)*(length(bonobos2$ID)-1)/2))
bonobos2$ranks<-c(1:7)

bonobos2
plot(data=bonobos2, dush_norm~ranks)

DSPij<-function(x){
  EloRating::DS(x, prop = c("Pij"))
}

DS(bonobos, prop = 'Pij')
DSPij(bonobos)

#If I want to make it so that they Norm Score is NOT corrected for chance-----
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

#Validating weird cody dey data-----
NPBNW<-readxl::read_xlsx("C:\\Users\\quinl\\Downloads\\Observation\\Observation\\NPB NW.xlsx")
#DO NOT CHANGE
NPBNW$Dominant[NPBNW$Dominant == 'WNBB'] <- 'YNBB'
NPBNW$Subordinate[NPBNW$Subordinate == 'WNBB'] <- 'YNBB'
#DO NOT CHANGE
NPBNW$Dominant[NPBNW$Dominant == 'WYRG'] <- 'WYGR'
NPBNW$Subordinate[NPBNW$Subordinate == 'WYRG'] <- 'WYGR'

#SEE HOW TO FIX THESE ONES
NPBNW$Dominant[NPBNW$Dominant == 'GGWW'] <- 'YYGB'
NPBNW$Subordinate[NPBNW$Subordinate == 'GGWW'] <- 'YYGB'

NPBNW$Dominant[NPBNW$Dominant == 'WYYB'] <- 'WBYY'
NPBNW$Subordinate[NPBNW$Subordinate == 'WYYB'] <- 'WBYY'

NPBNW$Dominant[NPBNW$Dominant == 'YGYB'] <- 'YWYB'
NPBNW$Subordinate[NPBNW$Subordinate == 'YGYB'] <- 'YWYB'

NPBNW<-NPBNW[,-c(1)]%>%
  group_by(Dominant, Subordinate)%>%
  summarize(count=length(`Interaction (P=Posture, D=Displace/Avoid, C=Charge, B=Bite/Kick)`))

(DS(matrix_maker(NPBNW))
) %>% map_df(rev)

try2<-t(matrix(c(0,1,1,4,2,6,10,
                 0,0,4,5,0,10,4,
                 0,2,0,4,65,8,3,
                 2,3,2,0,0,80,10,
                 1,0,0,0,0,6,7,
                 1,8,5,0,2,0,6,
                 4,0,1,8,5,3,0), 
               nrow = 7))

rownames(try2)<-c("a", "b", "c", "d", "e", "f", "g")
colnames(try2)<-c("a", "b", "c", "d", "e", "f", "g")

#BOOTSTRAPPING DS
# Create a list to store all results
BOOT_davids <- list()

# Loop through each matrix in the list
for (H in seq_along(list_of_objects2)) {
  mat <- list_of_objects2[[H]]
  
  # Run reg_randomizer `C` times on this matrix
  reps <- replicate(C, reg_randomizer(mat), simplify = FALSE)
  
  # For each replicate, extract Scores and annotate with IDs
  rep_scores <- lapply(seq_along(reps), function(S) {
    df <- reps[[S]]$Scores
    df$MatrixID <- H
    df$Replicate <- S
    return(df)
  })
  
  # Combine replicates for this matrix
  BOOT_davids[[H]] <- do.call(rbind, rep_scores)
}

# Combine all matrices into one big data frame
BOOT_davids <- do.call(rbind, BOOT_davids)

Doms %>%
  filter()

Doms<-Doms[-Doms$Interaction %in% c("X"), ]

length(Doms$Interaction=="X")

ggplot(David_Score, aes(x=good_ranks, y=normDS))+
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq")), size=4) +
  geom_point()+
  facet_wrap(~Group, ncol=4)+
  labs(x="Within Group Rank", y="Normalized David's Score")+
  ggthemes::theme_few()

print(unique(David_Score$Group))
# New facet label names for
ggplot(results)+
  geom_density(aes(value), color="darkblue", fill="lightblue")+
  facet_wrap(~ id, ncol=4)+
  geom_vline(data = slopes,
             mapping = aes(xintercept = Slopes), 
             linetype = 'dashed', 
             color="red")+
  ggthemes::theme_few()+
  geom_text(data = slopes, aes(y = 20, x = -.55, 
                               label = ifelse(round(p, 3) == 0, 
                                              "p<0.001", 
                                              paste("p=", round(p, 3), sep=""))))



##individual pariwise Dij
test<-DS_SPBTrigGate

extract_dyads <- function(matrix_data) {
  names <- rownames(matrix_data)
  n <- length(names)
  
  dyads <- data.frame(
    bird1 = character(),
    bird2 = character(),
    wins = numeric(),
    losses = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        dyads <- rbind(dyads, data.frame(
          bird1 = names[i],
          bird2 = names[j],
          wins = matrix_data[i, j],
          losses = matrix_data[j, i],
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  dyads$Dij <- (dyads$wins / (dyads$wins + dyads$losses))-(((dyads$wins / (dyads$wins + dyads$losses))-0.5)*(1/((dyads$wins + dyads$losses)+1)))
  return(dyads)
}

indiv.dyad<-lapply(list_of_objects2, extract_dyads)

process_dyad <- function(df) {
  df %>%
    filter(bird1 != bird2) %>%
    rename(from = bird1, to = bird2, prop1 = Dij) %>%
    left_join(
      df %>%
        rename(from = bird2, to = bird1, prop2 = Dij),
      by = c("from", "to")
    ) %>%
    mutate(
      bird1 = pmin(from, to),
      bird2 = pmax(from, to),
      prop_diff = prop1 - prop2
    ) %>%
    select(bird1, bird2, prop1, prop2, prop_diff) %>%
    distinct()
}

dyadic_dom <- bind_rows(
  (lapply(indiv.dyad, 
          process_dyad)),
  .id = "group")




#CLEAN ME UP

test<-subset(dyadic_dom, group=="DS_EAST_CAMP_SE")

test2<-merge(test, IDS, by.x="bird1", by.y = "COMBO", all=T)
names(test2)[names(test2) == 'PPO'] <- 'PPO1'

test2<-merge(test2, IDS, by.x="bird2", by.y = "COMBO", all=T)
names(test2)[names(test2) == 'PPO'] <- 'PPO2'
test2<-subset(test2, GRP.x==GRP.y)

ggplot(test2, aes(x=R_1.2, y=prop_diff))+
  geom_point()
unlist(unique(test2$bird2))

test<-indiv.dyad[["DS_EAST_CAMP_SE"]]

cleanup<-function(matrix){
  matrix[upper.tri(matrix, diag= T)] <- NA
  x<-as.data.frame(matrix)
  
  x<-as.data.frame(x)
  #creates the winner ID column so I can make the dataframe long
  x<-tibble::rownames_to_column(x, "BIRD1")
  #unique total value of columns per matrix
  N<-length(colnames(x))
  #creates a unique dataframe that has a list of all interaction pairs (totals)
  x<-pivot_longer(as.data.frame(x), 
                  cols = c(2:N), 
                  names_to = "BIRD2", 
                  values_to = "Dyad_Slope")
}

indiv.dyad<-lapply(indiv.dyad, cleanup)
indiv.dyad<-na.omit(indiv.dyad)

test<-subset(indiv.dyad, group =="DS_EAST_CAMP_SE" | )


indiv.dyad$group<-factor(indiv.dyad$group, levels = c("DS_E_Hay","DS_EAST_CAMP_SE",
                                                      "DS_Hay_3W","DS_HAY1_N",
                                                      "DS_LookoutSE","DS_NCoastSE",
                                                      "DS_NPB NW","DS_NPB_E",
                                                      "DS_NPB_S","DS_Rd_Flats_Swede_gate",
                                                      "DS_RdFlatAnchorGate",
                                                      "DS_ROAD_FLATS_SE","DS_SPB_N_CURVE",
                                                      "DS_SPBTrigGate","DS_staff_3",
                                                      "DS_SY1_C","DS_SY1_NE",
                                                      "DS_SY1_NW","DS_W_camp_C",
                                                      "DS_WEST_CAMP_NW"),
                         labels = c("East Hay", "East Camp SE", 
                                    "Hay 3W", "Hay 1N", 
                                    "Lookout SE", "North Coast SE", 
                                    "N. Punchbowl NW", "N. Punchbowl E", 
                                    "N. Punchbowl S", "Road Flats: Swede gate",
                                    "Road Flats: Parking", 
                                    "Road Flats SE", "S. Punchbowl N", 
                                    "S. Punchbowl: Trig gate", "Staff 3", 
                                    "Stockyard 1 C", "Stockyard 1 NE", 
                                    "Stockyard 1 NW", "West Camp C", 
                                    "West Camp NW"))


Relate2<-Relate3[,c(1:5,7)]
names(Relate2)[names(Relate2)=="GRP2"]<-"group"
names(Relate2)[names(Relate2)=="COMBO.x"]<-"BIRD1"
names(Relate2)[names(Relate2)=="COMBO.y"]<-"BIRD2"

indiv.relate = merge(indiv.dyad, Relate2, by.x=c("BIRD1", "BIRD2"), by.y=c("BIRD1", "BIRD2"))
indiv.relate$inv<-abs(indiv.relate$Dyad_Slope)
indiv.relate$SZN<-if_else(indiv.relate$group.y=="East Hay" | indiv.relate$group.y=="Lookout SE" | indiv.relate$group.y=="North Coast SE" | indiv.relate$group.y=="Road Flats: Swede gate" | indiv.relate$group.y=="Road Flats: Parking" | indiv.relate$group.y=="S. Punchbowl N" | indiv.relate$group.y=="S. Punchbowl: Trig gate" | indiv.relate$group.y=="Staff 3", 
                 "SPring", 
                 "Summer")

indiv.relate<-indiv.relate %>% #removes the unknown groups
  group_by(group.x) %>%
  filter(n() > 5)

ggplot(indiv.relate, 
       aes(R_1.2, inv))+
  geom_point()+
  geom_smooth(method="lm", se=T)+
  theme(legend.position="none")+
  coord_cartesian(ylim=c(0,1))+
  ggthemes::theme_few()+
  theme(legend.position="none")

library(glmmTMB)
relatedness<-glmmTMB(slopes~R_1.2+SZN+diag(1+R_1.2+SZN|group.x), 
                     data=Relate3)

performance::check_model(relatedness)

summary(relatedness)

Relate3<-Relate3%>%
  group_by(GRP2)%>%
  reframe(mean=mean(R_1.2), 
          slope=Slopes, 
          p=p)
Relate3<-unique(Relate3)

ggplot(Relate3, aes(mean, slope))+
  geom_point()+
  geom_smooth(method="lm")

Relate3$SZN<-if_else(Relate3$GRP2=="East Hay" | Relate3$GRP2=="Lookout SE" | Relate3$GRP2=="North Coast SE" | Relate3$GRP2=="Road Flats: Swede gate" | Relate3$GRP2=="Road Flats: Parking" | Relate3$GRP2=="S. Punchbowl N" | Relate3$GRP2=="S. Punchbowl: Trig gate" | Relate3$GRP2=="Staff 3", 
                     "SPring", 
                     "Summer")

Relate3$inv<-abs(Relate3$slope)

relatedness2<-glmmTMB(Slopes~R_1.2*SZN,
                      data=Relate3)

performance::check_model(relatedness2)

summary(relatedness2)

plot(ggeffects::ggpredict(relatedness2, terms = "R_1.2"))+
  geom_point(data = Relate3, aes(x=R_1.2, y=Slopes))
plot(ggeffects::ggpredict(relatedness2, terms = "SZN"))

slopes2<-DF%>%
  group_by(ITER)%>%
  summarize(
    reg=lm(DS~RAND)[["coefficients"]][["RAND"]]
  )


  