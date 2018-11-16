
plasticity<-rep(c(-0.5,-0.25,0,0.25,0.5),times=1350) #Different plasticity levels
mutRate<-rep(c(0.05, 0.001, 0.0015, 0.0001, 0.00015),times=1350) #different mutation rates
selection<-rep(c("{{3}}","{{5}}","{{10}}","{{15}}","{{20}}"),times=1350)#different selectino rates

for(i in 1:6750){#write the files
  write(muRate[i],paste("/muRate/",i,sep=";"))
  write(selection[i],paste("/selection/",i,sep=";"))
  write(plasticity[i],paste("/plasticity/",i,sep=";"))
}
