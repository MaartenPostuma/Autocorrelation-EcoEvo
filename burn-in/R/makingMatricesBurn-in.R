library(popbio)
### keep juvenile survival and transitions fixed
survJ=rep(seq(0.11,0.99,length.out=5),times=30) #Specify juvenile survival
transJ=rep(seq(0.11,0.99,length.out=5),each=5,times=6) #Specify juvenile transition
b=survJ*transJ #Transitions from juvenile to adult
a=survJ*(1-transJ) #Juvenile to juvenile survival
d=rep(c(0,.25,.5,.75,.95,.99),each=25) # Adult Survival
### keep juvenile survival and transitions fixed
mats<-list()
for(i in 1:length(d)){
  c=(1.2-a[i])*(1.2-d[i])/b[i] #Solve the matrix
  projectionMatrix<-matrix(c(a[i],b[i],c,d[i]),nrow=2) #create matrix
  if(round(Re(eigen(projectionMatrix)$values[1]),3)!=1.2) print("Something's wrong")#test if lambda is correct
  
  mats[[i]]<-projectionMatrix #save matrix in a list
}
mat2<-NA #create a vector in which to store the matrices in nemo's format

for(i in 1:length(mats)){                             #Make life histories in nemo's format
  mat2[i]<-paste("{{0,0,",mats[[i]][1,2]*2,"}{1,",
                 mats[[i]][1,1],",0}{0,",mats[[i]][2,1],",",
                 mats[[i]][2,2],"}}",sep="")
}


for(i in 1:length(mat2)){ 
  write(paste(mat2.1[i]),paste("/Nemo/Matrices",i,sep=""))}


  #Making lifeHistory metrics #########################

a<-b<-c<-vector()
for(i in 1:length(mats)){
  a2<-net.reproductive.rate(mats[[i]]) #net reproductive rate
  a<-c(a,a2)
  b2<-generation.time(mats[[i]]) #generation time
  b<-c(b,b2)
  c2<-mats[[i]][1,2]# reproduction
  c<-c(c,c2)
}

lifeHistPar4<-data.frame(reprRate=a,genTime=b,
                         juvSurv=survJ,
                         adltSurv=d,
                         juvJuv=survJ*(1-transJ),
                         juvAdlt=survJ*transJ,
                         repr=c)
write.table(lifeHistPar4,"lifehistoryParam.txt")
