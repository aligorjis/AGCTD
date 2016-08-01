kebccf <-function(x,y,lgm=NULL)
{
  corr<-vector(length = 2*lgm+1)
  lagg<-vector(length = 2*lgm+1)
  o <- lgm +1
  for ( lg in lgm:0 ){ 
    s1<- y[1:(length(y)-lg)]
    s2<- x[(lg+1):length(x)]
    Cor<-cor(s2,s1,method="kendall")
    corr[o+lg]<- Cor
    lagg[o+lg]<- lg
    kccfm<- data.frame(corr,lagg)
  }
  for ( lg in lgm:0 ){ 
    s1<- x[1:(length(x)-lg)]
    s2<- y[(lg+1):length(y)]
    Cor<-cor(s2,s1,method="kendall")
    corr[o-lg]<- Cor
    lagg[o-lg]<- (-lg)
    kccfm<- data.frame(corr,lagg)
  }
  return(kccfm)
}

for ( i in 1:8){ 
  listgene[[i]]<- floor(listgene[[i]])
} 

cc<-array(0,dim=8)
for (i in 1:8 )
{
  cc[i]<- cov(listgene[[i]][1:4],listgene[[i]][1:4],method = "pearson")
}
cc

TM<-array(0,dim=c(8,8,5))
for (i in 1:8){ 
  for ( j in 1:8){
    for ( k in 1:5){
      if (cc[j]>cc[i]){
        c=kebccf(listgene[[i]] , listgene[[j]], lgm = 2 )
        TM[i,j,k]=c$corr[k]}}}
}
A<-TM[,,3:5]
tr<-mean(abs(A))

for (i in 1:8){ 
  for ( j in 1:8){
    for ( k in 1:3){
      T <- A[i,j,k] 
      if ( ((T>0)&&T <(2*tr)))
        A[i,j,k]=0
    }}
}
A
for (i in 1:8){ 
  for ( j in 1:8){
    for ( k in 1:3){
      T <- A[i,j,k] 
      if ( ((T<0)&&T >-(2*tr)))
        A[i,j,k]=0
    }}
}
for (i in 1:8){ 
  for ( j in 1:3){
    A[i,i,j]=0 
  } 
}
A


for (i in 1:8){ 
  for ( j in 1:8){
    for ( m in 1:3){
      tmax<- abs(A[i,j,])
      wmax <- max(tmax)
      if (abs(A[i,j,m])< wmax)
        A[i,j,m]=0
    }
  }
}
A
for (i in 1:8){ 
  for ( j in 1:8){
    for ( k in 1:3){
      for (l in 1:8){ 
        for ( m in 1:3){
          for ( n in 1:3){
            if ((A[i,j,k]>0 && A[l,i,m]>0 && A[l,j,n]>0 ) || (A[i,j,k]<0 && A[l,i,m]<0 && A[l,j,n]<0 ) && n==k+m)
              A[l,i,]=0
          }
        }
      }
    }
  }
}
A
