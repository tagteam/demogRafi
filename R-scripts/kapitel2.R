source('https://raw.githubusercontent.com/tagteam/demogRafi/main/R-scripts/demofunk.R')

# R-koder til beregning af summariske mortalitetsrater 
x <- hent_data("FOLK1a",tid = "2011K3",køn = c(2,1))
# fjern TID fordi den er konstant
x$TID <- NULL
# Risikotid i 2011 baseret på middelfolketal metode 1
# ganget med 1 år 
x <- rename(x,"risiko_tid"="INDHOLD")
# Antal dødsfald i 2011
d <- hent_data("DOD",tid="2011","køn" = c("K","M"))
# Fjern TID fordi den er konstant
d$TID <- NULL
# Ændre variable navn
d <- rename(d,"antal_dod"="INDHOLD")
# join folktal og antal døde
dat <- left_join(x,d,by="KØN")
# Summariske mortalitetsrater per 1000 personår
dat <- mutate(dat,
              "Summariske mortalitetsrate"=1000*antal_dod/risiko_tid)
dat

# R-koder der genererer alderspyramiden for Danmark den 1. januar 2023
folk <- hent_data("FOLK1a","alder"=0:125,"køn"=1:2,tid="2023K3")
# Fjern tomme aldre
folk <- subset(folk,alder<106)
# Mænd skal vises på venstre siden, derfor bliver INDHOLD negativt 
folk_m <- subset(folk,KØN=="Mænd") %>% mutate(INDHOLD=-INDHOLD)
# For kvinder på højre siden er INDHOLD  positivt
folk_k <- subset(folk,KØN=="Kvinder")
# Grafik
g <- ggplot(folk, aes(x = alder, y = INDHOLD, fill = KØN)) +
  geom_bar(data=folk_m, stat = "identity") +
  geom_bar(data=folk_k, stat = "identity") +
  coord_flip() + theme_economist() +
  ylab("Folketal N(t)")+xlab("Alder (år)") +
  theme(legend.title=element_blank())
g <- g+ggtitle("Alderspyramide Danmark 1 juli 2023")
g <- g+theme(axis.title.y=element_text(margin=margin(t=0,r=20,b=0,l=0)))
g <- g+theme(axis.title.x=element_text(margin=margin(t=20,r=0,b=0,l=0)))
g

# R-koder til beregning af aldersfordeling af folketal
folk <- hent_data("FOLK1a","alder"=0:125,tid="2023K3")
# Aldersintervaller
folk <- mutate(folk,Aldersinterval=cut(alder,
                                   breaks=c(0,25,50,75,125),
                                   include.lowest = TRUE))
# antal person i de 4 aldersintervaller
af <- folk %>% group_by(Aldersinterval) %>%
      summarise(Antal=sum(INDHOLD))
# beregne procenter 
af <- af %>% mutate(Procent=100*Antal/sum(Antal))
af

# R-koder til aldersfordeling af folketal for given bystørrelse
info <- register_info("BY2")
names(info)
b2 <- hent_data(register = "BY2",alder=0:125,
                BYST=c("HOVEDS","LAND"),tid="2023")
# Aldersintervaller
b2 <- mutate(b2,Aldersinterval=cut(alder,
                                   breaks=c(0,25,50,75,125),
                                   include.lowest = TRUE))
# Antal person i de 4 aldersintervaller
af <- b2 %>% group_by(BYST,Aldersinterval) %>%
                                summarise(Antal=sum(INDHOLD))
# Procent 
af <- af %>% mutate(Procent=100*Antal/sum(Antal))
af

# Beregningen af aldersfordelingen af risikotiden
folk <- hent_data("FOLK1a", alder=0:125, tid=c("2022K1","2023K1"))
# Risikotid= 1* Middelfolketal metode 2
folk <- folk %>% group_by(alder) %>%
                            summarise(Risikotid=1*mean(INDHOLD))
# Aldersintervaller
folk <- mutate(folk,Aldersinterval=cut(alder,
                                           breaks=c(0,25,50,75,125),
                                           include.lowest = TRUE))
# Antal personår i de 4 aldersintervaller
af <- folk %>% group_by(Aldersinterval) %>%
                                 summarise(Personår=sum(Risikotid))
# Aldersfordelingen af risikotiden i procent 
af <- af %>% mutate(Procent=100*Personår/sum(Personår))
af

# R-koder der henter aldersfordelingen af antal døde
dd <- hent_data("dod",alder="all_no_total",tid="2022")
# Aldersintervaller
dd <- mutate(dd,Aldersinterval=cut(alder,
                                   breaks=c(0,25,50,75,125),
                                   include.lowest = TRUE))
# Antal døde i de 4 aldersintervaller
group_dd <-dd %>% group_by(Aldersinterval) %>%
  summarise(antal_døde=sum(INDHOLD))
group_dd

# Fortsættelse af sidste R-chunk
x <- left_join(af,group_dd,by="Aldersinterval")
# Aldersspecifikke mortalitetsrater
x <- x %>% mutate(mrate=1000*antal_døde/Personår)
x

# R-koder der genererer grafik som viser køns- og aldersspecifikke mortalitetsrater 
folk <- hent_data("FOLK1a",alder=0:125,køn=c(2,1),tid="2011K3")
# Ændre variable navn
folk <- folk %>% rename("risikotid"="INDHOLD")
# Samle antal personer over 99 (fordi register DOD gør det samme)
folk <- samle_alder(folk,variable = "risikotid",value = "99plus",by = "køn")
# Hent dødstal fra 2011
dd <- hent_data(register="dod",alder=0:99,køn=c("K","M"),tid="2011")
# Ændre variable navn
dd <- dd %>% rename("antal_døde"="INDHOLD")
# Join folketal og antal dødsfald
x <- left_join(folk,dd,by=c("alder","KØN"))
# Aldersspecifikke mortalitetsrater
x <- x %>% group_by(KØN) %>% mutate(mrate=1000*antal_døde/risikotid) 
# Grafik
g <- ggplot(x,aes(x=alder,y=mrate,color= KØN))+geom_line()+geom_point()
g <- g+theme_economist()+scale_colour_wsj("colors6") +theme(legend.title=element_blank())
g <- g+ylab("Mortalitetsrate per 1000 personår")+xlab("Alder (år)")+ggtitle("Danmark 2011")
g <- g+theme(axis.title.y=element_text(margin=margin(t=0,r=20,b=0,l=0)))
g <- g+theme(axis.title.x=element_text(margin=margin(t=20,r=0,b=0,l=0)))
g <- g+theme(plot.background=element_rect(fill="gray88",colour=NA))
g <- g+theme(legend.title=element_blank())+theme(legend.position="bottom")
g + scale_y_log10()

# R-koder der beregner direkte standardisering
af <- hent_data("FOLK1a","alder"=0:125,tid="2011K3",
                  køn = c(1,2,"TOT"))
# Fordeling af risikotid i aldersintervaller
af <- rename(af,R = INDHOLD)
af <- intervAlder(af,breaks=c(0,25,50,75,125),by="KØN",vars="R")
# Antal døde i aldersintervaller
dd <- hent_data("DOD","alder"="all_no_total",tid="2011",køn=c("M","K"))
dd <- rename(dd,D = INDHOLD)
dd <- intervAlder(dd,breaks=c(0,25,50,75,125),by="KØN",vars="D")
# Aldersspecifikke mortalitetsrater
# Kvinder
A <- left_join(filter(af,KØN == "Kvinder"),
               filter(dd, KØN == "Kvinder"),
               by = c("KØN","aldersinterval"))
A <- mutate(A,M = 1000*D/R)
A <- select(A,aldersinterval,M)
# Mænd
B <- left_join(filter(af,KØN == "Mænd"),
               filter(dd, KØN == "Mænd"),
               by = c("KØN","aldersinterval"))
B <- mutate(B,M = 1000*D/R)
B <- select(B,aldersinterval,M)
# Aldersfordeling i standardbefolkning 
S <- select(filter(af,KØN == "I alt"),!KØN)
S <- mutate(S,V=(R/sum(R)))
# Join
A <- left_join(A,S,by = "aldersinterval")
B <- left_join(B,S,by = "aldersinterval")
# Direkte standardisering
tibble("srate_kvinder" = pull(summarise(A,sum(M * V))),
       "srate_maend" = pull(summarise(B,sum(M * V))))
