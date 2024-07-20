
devtools::load_all()
getShearParameters(.newdata=data.table(Gravels=c(90,95,100),Sands=c(5,10,25),Hs=c(90,100,110),Water=0,POP=100))


devtools::load_all()
geSiteTable(Hs=78,USCS=c("GW","GP","GM","ML","SM"),NR=100,levels=c(0.16,0.50,"mean",0.84))

buildDSRA(Hs=123,h=1,Water=0,USCS=c(ValidGravels,ValidClays),POP=100)[["Gravels"]]

