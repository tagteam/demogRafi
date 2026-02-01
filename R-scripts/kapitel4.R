source('https://raw.githubusercontent.com/tagteam/demogRafi/main/R-scripts/demofunk.R')

x <- hent_data("fod11",tid = 2022,alder = c(610,617))
with(x,paste(ALDER,INDHOLD))

f2020 <- hent_fertilitetsrate_data(2020)
f2020

options(pillar.sigfig = 5)
f2020 <- mutate(f2020,frate = 1000*Fødsler/R)
f2020

summarize(f2020,samlet_fertilitet = sum(frate*5))

pige2020 <- hent_fertilitetsrate_data(2020,barnkon = "Piger")
pige2020 <- mutate(pige2020,frate_piger = 1000*Fødsler/R)
summarize(pige2020,BRT = sum(frate_piger*5))

fx <- fertilitets_tavle(tid = 2020)
select(fx,aldersinterval,L,frate_piger)

fx <- mutate(fx,bidrag_NRT=frate_piger*L/100000)
summarize(fx,NRT = sum(bidrag_NRT))
