## load library
library(DJL)

## load dataset / parameters
data(dataset.engine.2015)
fis<-subset(dataset.engine.2015,grepl("^.C..",dataset.engine.2015[,8]))
n<-subset(fis,select=1)
x<-subset(fis,select=4)
y<-subset(fis,select=6:7)
d<-subset(fis,select=2)

## Table 1. Descriptive statistics
table.1<-matrix(NA,nrow=4,ncol=3,
                dimnames=list(c("Min","Med","Max","Std"),
                              c("Disp","Power","Torque")))
table.1[1,]<-c(min(x),min(y[,1]),min(y[,2]))
table.1[2,]<-c(median(x[,1]),median(y[,1]),median(y[,2]))
table.1[3,]<-c(max(x),max(y[,1]),max(y[,2]))
table.1[4,]<-c(sd(x[,1]),sd(y[,1]),sd(y[,2]))
t(table.1)

## Table 2. 2015 SOA
m1<-roc.dea(x,y,d,2015,"vrs","o","min")
soa<-which(m1$roc_local>1)
table.2<-data.frame(Vehicle_name=n[soa,],
                    MY=d[soa,],
                    x[soa,,drop=FALSE],
                    y[soa,],
                    LocalRoC=round(m1$roc_local[soa],digits=4))
rownames(table.2)<-soa
table.2

## Table 3. Specs & Eff of DMU 262
dmu<-262;dt<-3
x_f<-rbind(x[soa,,drop=FALSE],x[dmu,])
y_f<-rbind(y[soa,]*m1$roc_local[soa,]^dt,y[dmu,])
m2<-dm.dea(x_f,y_f,"vrs","o")
table.3<-data.frame(DMU=dmu,
                    x[dmu,,drop=FALSE],
                    y[dmu,],
                    Eff_2015=round(m1$eff_t[dmu],digits=4),
                    Eff_2018=round(m2$eff[9],digits=4))
rownames(table.3)<-n[dmu,]
table.3

## Annual rate of obsolescence
(m2$eff[9]/m1$eff_t[dmu])^(1/3)

## Lower bound of input change
y_l<-y_f;y_l[9,]<-y_f[9,]*m1$eff_t[dmu]
x_f[9,]*(dm.dea(x_f,y_l,"vrs","i")$eff[9])

## Table 4. 2018 performance targets
table.4<-matrix(NA,nrow=18,ncol=10,
                dimnames=list(c(),
                              c("Target",names(x),"W_Power","W_Torque",names(y),"Validation","S_Dispk","S_Power","S_Torque")))
table.4[,1]<-c(rep(round(m1$eff_t[dmu],digits=5),9),rep(1,9))
table.4[,2]<-c(rep(c(rep(3.5,3),rep(3.8,3),rep(4.0,3)),2))
table.4[,3]<-c(rep(c(0.3,0.5,0.7),6))
table.4[,4]<-1-table.4[,3]
for(i in 1:18){
  if(i<10){target<-m1$eff_t[dmu]}else{target<-1}
  a<-matrix(table.4[i,2])
  w<-matrix(c(table.4[i,3],table.4[i,4]),1)
  table.4[i,5:6]<-target.spec.dea(x_f,y_f,dmu=9,et=target,alpha=a,wv=w,rts="vrs")$beta
  x_v<-rbind(as.matrix(x_f[1:8,]),table.4[i,2])
  y_v<-rbind(y_f[1:8,],table.4[i,5:6])
  table.4[i,5:6]<-round(table.4[i,5:6],1)
  post<-dm.dea(x_v,y_v,"vrs","o")
  table.4[i,7]<-round(post$eff[9],5)
  table.4[i,8]<-round(post$xslack[9,],5)
  table.4[i,9:10]<-round(post$yslack[9,],5)
}
table.4
