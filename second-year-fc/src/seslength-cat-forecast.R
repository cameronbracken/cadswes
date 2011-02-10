library(robust)
library(VGAM)
# Apr-May NINO4
nino4am=scan("http://iridl.ldeo.columbia.edu/expert/SOURCES/.Indices/.nino/.EXTENDED/.NINO4/T/%28Apr%201948%29%28May%202009%29RANGE/T/2/boxAverage/T/12/STEP/data.ch")
yy=seslengthg

iyear = 1948:2009

nino4mj = scan("http://iridl.ldeo.columbia.edu/expert/SOURCES/.Indices/.nino/.EXTENDED/.NINO4/T/%28May%201948%29%28Jun%202009%29RANGE/T/2/boxAverage/T/12/STEP/data.ch")
nino4jja = scan("http://iridl.ldeo.columbia.edu/expert/SOURCES/.Indices/.nino/.EXTENDED/.NINO4/T/%28Jun%201948%29%28Aug%202009%29RANGE/T/3/boxAverage/T/12/STEP/data.ch")


################  Multinomial Logistic..


ylog = yy
ylog[ylog <= 120] = 1
ylog[ylog > 120 & ylog < 140]=2
ylog[ylog >= 140] = 3
test2 = cbind(nino4am,ylog)
test2=as.data.frame(test2)
#zz=glm(ylog ~ ., data=test2, family=binomial(link="logit"))
zz=vglm(ylog ~ ., multinomial, data=test2)
zz1=predict(zz,type="response")
ypredcamf=zz1

N=length(yy)
index=1:N
ypredc = zz1
for(i in 1:N){
index1=index[index != i]

zz=vglm(ylog ~ ., multinomial, data=test2[index1,])
ypredc[i,]=predict(zz,test2[i,],type="response")

}
ypredcamcv=ypredc

### MJ NINO4
test2 = cbind(nino4mj,ylog)
test2=as.data.frame(test2)
#zz=glm(ylog ~ ., data=test2, family=binomial(link="logit"))
zz=vglm(ylog ~ ., multinomial, data=test2)
zz1=predict(zz,type="response")
ypredcmjf=zz1

N=length(yy)
index=1:N
ypredc = zz1
for(i in 1:N){
index1=index[index != i]

zz=vglm(ylog ~ ., multinomial, data=test2[index1,])
ypredc[i,]=predict(zz,test2[i,],type="response")

}
ypredcmjcv=ypredc


zz1=ypredcmjcv

p1=length(ylog[ylog == 1])/62
p2=length(ylog[ylog == 2])/62
p3=length(ylog[ylog == 3])/62
climo = c(p1,p2,p3)
rps = 0
rpsc = 0
n1=0
zpred = array()
obs = c(0,0,0)
for(i in 1:62){
#if(ylog[i] == 1 || ylog[i] == 3){

#if(ylog[i] == 2){

n1=n1+1
obs[ylog[i]]=1
zpred[n1]=zz1[i,ylog[i]]

rps=rps + sum((obs-zz1[i,])^2)
rpsc=rpsc + sum((obs-climo)^2)

#rps=rps + sum((obs[ylog[i]]-zz1[i,ylog[i]])^2)
#rpsc=rpsc + sum((obs[ylog[i]]-climo[ylog[i]])^2)

#}


obs=c(0,0,0)
}

rpss = 1 - (rps/rpsc)

#######  cat 1 and 3
 z13=matrix(0,ncol=2,nrow=11)
 z13[1:9,1] = zpred[yyl == 1]
 z13[,2] = zpred[yyl == 3]
 z13[10,1]=NaN
 z13[11,1]=NaN

####

boxplot(z13[,1])
points(climo[1], col="red")
boxplot(zpred)
points(climo[2],col="red")
boxplot(z13[,2])
points(climo[3],col="red")

#########


zz1=ypredcamf

rps = 0
rpsc = 0
n1=0


cat3  = cbind(apply(zz1[,1:2],1,sum),zz1[,3])
obs=c(0,0)
climo3 = c((climo[1]+climo[2]),climo[3])

cat1  = cbind(zz1[,1],apply(zz1[,2:3],1,sum))
obs=c(0,0)
climo1 = c(climo[1],(climo[2]+climo[3]))


cat2  = cbind(apply(cbind(zz1[,1],zz1[,3]),1,sum),zz1[,2])
obs=c(0,0)
climo2 = c(climo[1]+climo[3],climo[2])

xll = 0.
xcc=0.
for(i in 1:62){
obcat = ylog[i]
xll = xll + log(zz1[i,obcat])
xcc = xcc + log(climo[obcat])

#xll = xll + log(zz1[i,obcat]/climo[obcat])

#if(ylog[i] == 3) {
if(ylog[i] == 1) {
#if(ylog[i] == 2) {

#rps=rps + sum((1-cat3[i,2])^2);
#rpsc=rpsc + sum((1-climo3[2])^2);

rps=rps + sum((1-cat1[i,1])^2);
rpsc=rpsc + sum((1-climo1[1])^2);

#rps=rps + sum((0-cat2[i,1])^2);
#rpsc=rpsc + sum((0-climo2[1])^2);


}

else {

#rps=rps + sum((0-cat3[i,2])^2);
#rpsc=rpsc + sum((0-climo3[2])^2);

rps=rps + sum((0-cat1[i,1])^2);
rpsc=rpsc + sum((0-climo1[1])^2);


#rps=rps + sum((1-cat2[i,1])^2);
#rpsc=rpsc + sum((1-climo2[1])^2);

}



obs=c(0,0)

}

1-(rps/rpsc)


index=1:62
zz2=1:62
zz3=1:62
for(i in 1:62){
zx=zz1[i,]
i1 = order(zx,decreasing=TRUE)[1]
zz2[i]=zz1[i,i1]
zz3[i]=i1

}
plot(ylog,zz2,xlab="Observed Category",ylab="Predicted Probability")


 index=1:62
 index=index[ylog == 1 | ylog == 3]
next = length(index)
rps=0
rpsc=0
obs=c(0,0,0)
for(i in 1:next){
obs[ylog[i]]=1
obs1 = c(obs[1],obs[3])
climo1 = c(climo[1],climo[3])
zz2 = c(
rps=rps + sum((obs-zz1[index[i],])^2)
rpsc=rpsc + sum((obs-climo)^2)
obs=c(0,0,0)
}
rpss = 1 - (rps/rpsc)
