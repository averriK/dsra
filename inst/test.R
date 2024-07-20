
rm(list=ls())
devtools::load_all()
fitModel.Shear(.newdata=data.table(Gravels=c(90,95,100),Sands=c(5,10,25),Hs=c(90,100,110),Water=0,POP=100))
