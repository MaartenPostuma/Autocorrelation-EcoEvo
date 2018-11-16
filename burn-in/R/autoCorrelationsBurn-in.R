set.seed(12345)
autoCors<-seq(0.1,0.9,0.1)
autoRel<-c(-.8,-.6,-.4,-.2,0,.2,.4,.6,.8)
auto<-data.frame(values=c(1:length(autoCors)),autoCors=autoCors,real=autoRel)
listCors2<-list()
for(j in 1:length(autoCors)){
  listCors<-list()
  while(length(listCors)<5){
    b<-data.frame(environment=NA,n=seq(1:7000))
    
    for(i in 1:7000){
      b[i,1]<-rbinom(1,1,.5) #draw the first number from a binomial distribution
      if(autoCors[j]>0){
        if(i>2){              #for every other number
          if(b[i-1,1] == 0){  #if the previous number was equal to 0
            b[i,1]<-rbinom(1,1,1-autoCors[j])} else { #average of the binomial changes according to the autocorrelations
              b[i,1]<-rbinom(1,1,autoCors[j])} #if the previous number was not equal 
        }
      }
    }
    test<-b[1:7000,]
    cor<-acf(test$environment,plot=F)
    
    if(mean(test$environment)>0.499&mean(test$environment)<0.501 & #check whether the mean is equal to - and the auto correlation is equal to the desired one
       cor$acf[2]>autoRel[j]-0.01&cor$acf[2]<autoRel[j]+0.01){ 
      b$sel[b$opt==1]<-"{{1}}" #Format for Nemo
      b$sel[b$opt==0]<-"{{0}}"
      b$sel[b$opt==-1]<-"{{-1}}"
      listCors[[length(listCors)+1]]<-b
      print(paste(j,"found"))
    write(paste("(@g0 {{0}},",paste("@g",listCors[[length(listCors)]]$n," ",listCors[[length(listCors)]]$sel,",",sep="",collapse = ""),")"),file = paste("burn-in/autoCors/auto",length(listCors),"_",j,sep=""))#Write the matrices
}
}
listCors2[[j]]<-listCors
}