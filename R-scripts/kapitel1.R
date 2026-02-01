source('https://raw.githubusercontent.com/tagteam/demogRafi/main/R-scripts/demofunk.R')

# R-code
t_1= as.Date("2019-01-01")
t_2= as.Date("2023-01-01")
N_1 = 5806081
N_2 = 5932654
data.frame("dato"=c(t_1,t_2),"Folketal"=c(N_1,N_2),
           "Antal dage"=c(NA,t_2-t_1),"Vækst"=c(NA,N_2-N_1))

# load kursets funktioner direkte fra Internet
source("https://raw.githubusercontent.com/tagteam/demogRafi/main/R-scripts/demofunk.R")

# Brug funktionen til at hente folketal fra 2023 1. kvartal 
hent_data(register = "folk1a",tid = "2023K1")

hent_data(register = "befolk1",tid = 2023)

hent_data(register = "HISB3",tid = 2023,bevægelse = "M+K")

# R-koder der producerer figur 1
dt=hent_data(register = "FT",tid = "all")
ggplot(dt,aes(TID,INDHOLD))+geom_line()+geom_point()+
  theme_wsj()+ scale_colour_wsj("colors6")

dt=hent_data("BEFOLK1",tid=c(2009,2016,2023))
dt

# Metode 2
(5511451 + 5932654)/2

# Metode 3
(2016-2009)/(2023-2009)*(5511451+5707251)/2 + (2023-2016)/(2023-2009)*(5707251+5932654)/2

# Metode 3 baseret på 57 folketal mellem 2009 og 2023
# konstruere vektor 2009K1, 2009K2, ..., 2022K4
kvartal_years <- paste0(rep(2009:2022,rep(4,14)),"K",1:4)
# tilføj 2023K1
kvartal_years <- c(kvartal_years,"2023K1")
# hent data fra FOLK1a 
dt <- hent_data(register = "FOLK1a",tid=kvartal_years)
# transform årstal + kvartal til dato
dt <- format_dato(dt,variable = "TID")
# anvend middelfolketal metode 3
summarise(dt,{
  len <- length(TID)
  len_periode_total <- as.numeric(TID[length(TID)]-TID[1])
  len_periode <- as.numeric(TID[-1]-TID[-len])
  ft_gennemsnit <-(INDHOLD[-len]+INDHOLD[-1])/2 
  sum(len_periode*ft_gennemsnit)/len_periode_total
})%>% pull()

N <- hent_data("FOLK1a",tid = c("2022K1","2023K1"))[["INDHOLD"]]
D <- hent_data("DOD",tid=2022)[["INDHOLD"]]
F <- hent_data("FOD",tid = 2022)[["INDHOLD"]]
I <- hent_data("INDVAN",tid=2022)[["INDHOLD"]]
U <- hent_data("UDVAN",tid=2022)[["INDHOLD"]]
# data for ligevægtsligningen
tibble(X=c("Folketal jan 2022",
           "Folketal jan 2023",
           "Fødsler 2022",
           "Dødsfald 2022",
           "Indvandring 2022",
           "Udvandre 2022"),
       Antal=c(N[1],N[2],F,D,I,U))

# R-koder der producerer figur 3
V = hent_data("BEFOLK1",tid=1980:2023)
V = V %>% mutate(INDHOLD = INDHOLD- c(INDHOLD[1],INDHOLD[-length(INDHOLD)]))
D = hent_data("dod",tid=1980:2023)
D = D %>% mutate(INDHOLD = -INDHOLD)
F = hent_data("FOD",tid=1980:2023)
I = hent_data("INDVAN",tid=1980:2023)
U = hent_data("UDVAN",tid=1980:2023)
U = U %>% mutate(INDHOLD = -INDHOLD)
# samle data
dat <- tibble(rbind(cbind(X="Vækst",V),
                    cbind(X="Dødsfald",D),
                    cbind(X="Fødsler",F),
                    cbind(X="Indvandring",I),
                    cbind(X="Udvandring",U)))
dat <- dat %>% mutate(X = factor(X))
ggplot(dat,aes(TID,INDHOLD,color=X,group=X))+geom_line()+
  geom_point()+theme_wsj()+ scale_colour_wsj("colors6") +
theme(legend.title=element_blank())

# folketal den 1. jan 2022 og 1 jan 2023
N_bornholm=hent_data(register = "folk1a",
                       tid = c("2022K1","2023K1"),
                     område ="bornholm")
# middelfolketal metode 2
N_bornholm_metode2 = mean(N_bornholm$INDHOLD)
# antal døde
D_bornholm = hent_data(register = "fod207",tid = "2022",område ="bornholm")$INDHOLD
# mortalitetsrate per 1000 personår
1000*D_bornholm/N_bornholm_metode2

# Antal flytninger indenfor Danmark i årene 2020, 2021, 2022
FL <- hent_data("FLY",tid=2020:2022)
# Antal flytninger i perioden [2020,2022]
X <- pull(summarise(FL,sum(INDHOLD)))
X

# Folketal for den danske befolkning i perioden
N <- hent_data("FOLK1a",tid = c("2020K1","2023K1"))
# Middelfolketal metode 2
NN <-  summarise(N,middelfolketal=mean(INDHOLD))
# Risikotid
Risikotid <-  summarise(NN,R= middelfolketal * as.numeric(as.Date("2023-01-01")-as.Date("2020-01-01"))/365.25)
R <- pull(Risikotid)
R

# Flytningsrate per personår
X/R
# Flytningsrate per 1000 personår
1000*X/R

N <- hent_data("FOLK1a",tid=c("2022K1","2023K1"))[["INDHOLD"]]
vækst <- N[2]-N[1]
middelfolketal  <- mean(N)
risikotid <- middelfolketal*1
risikotid

# mortalitetsrate
D <- hent_data("DOD",tid=2022)[["INDHOLD"]]
Drate <- 1000*D/risikotid
# fødselsrate
F <- hent_data("FOD",tid=2022)[["INDHOLD"]]
Frate <- 1000*F/risikotid
# indvandringsrate
I <- hent_data("INDVAN",tid=2022)[["INDHOLD"]]
Irate <- 1000*I/risikotid
# udvandringsrate
U <- hent_data("UDVAN",tid=2022)[["INDHOLD"]]
Urate <- 1000*U/risikotid
# væksrate
Vrate <- 1000*vækst/risikotid
# naturlige væksrate
NaturVrate <- Frate-Drate
# nettovandringsrate
NettoVrate <- Irate-Urate
x=tibble(X=c("Vækstrate",
           "Mortalitetsrate",
           "Fødselsrate",
           "Indvandringsrate",
           "Udvandringsrate",
           "Naturlige_vækst_rate",
           "Netto_vandrings_rate"),
       Rate=c(Vrate,Drate,Frate,Irate,Urate,NaturVrate,NettoVrate))
x
