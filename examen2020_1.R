#1
x<-seq(-10,10,0.5)
x
tanh<-function(v){
  r<-(exp(2*v)-1)/(exp(2*v)+1)
  return(r)
}
sol<-tanh(x)
plot(x,sol,type="l")

#2

PERP<-function(v_1,v_2){
  X_p<-c()
  Y_p<-c()
  for(i in 1:length(v_1)){
    x<-log10(v_1[i])
    y<-log10(v_2[i])
    X_p<-c(X_p,x)
    Y_p<-c(Y_p,y)
  }
  prod<-X_p*Y_p
  X_p_alcuadrado<-X_p^2
  n<-length(v_1)
  B<-((n*sum(prod))-(sum(X_p)*sum(Y_p)))/((n*sum(X_p_alcuadrado))-(sum(X_p))^2)
  A<-10^(mean(Y_p)-B*mean(X_p))
  return(c(A,B))
}
x<-c(2.5,3,4,5,5.5,6,7)
y<-c(12.5,10,7,4.5,4,3,3.5)
PERP(x,y)

#3
Areayperimetro<-function(b,a,h){
  area<- b*a/2
  p<- b+a+h
  return(c(area,p))
}
Areayperimetro(4,3,5)

#4
Tc<-function(L,CN,S){
  solucion<-(0.0136*(L^0.8)*(((1000/CN)-9)^0.7))/S^0.5
  return(solucion)
}


#5
CSSE<-function(v,r){
  m<-matrix(v,ncol = 3)
  ds<-det(m)
  for (n in 1:length(r)) {
    if(n==1){
      m<-matrix(v,ncol = 3)
      m[,n]<-r
      dx<-det(m)
    }else if(n==2){
      m<-matrix(v,ncol = 3)
      m[,n]<-r
      dy<-det(m)
    }else{
      m<-matrix(v,ncol = 3)
      m[,n]<-r
      dz<-det(m)
    }
  }
  x<-dx/ds
  y<-dy/ds
  z<-dz/ds
  r<-c(x,y,z)
  return(r)
}
variables_xyz<-c(2,5,1,1,-4,-1,3,1,-4)
resul_de_SE<-c(7,-19,4)
CSSE(variables_xyz,resul_de_SE)


#7
codigo<-c("A","B","C","D","E","F")
ESTE<-c(272841.7,272893.6,272892.5,272913.8,272911.2,272837.5)
NORTE<-c(8666459.9,8666456.9,8666446.1,8666441.5,8666399.9,8666407.9)
d_f<-data.frame(codigo,ESTE,NORTE)

poligono<-function(df){
  este<-c(df[,"ESTE"],df[1,2])
  norte<-c(df[,"NORTE"],df[1,3])
  return(plot(este,norte,type="l"))
}
poligono(d_f)
