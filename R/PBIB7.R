PBIB7 <-
function(n,l,S,ty="a"){
if (S>=l || S<=1) {"Choose : 1 < S < l"}
else {

Op<-function(mt,n,l,S){
mt<-matrix(1:V, nrow=n,byrow=TRUE)
n<-dim(mt)[1];l<-dim(mt)[2];V<-n*l
a<-combn(1:l, S);b<-combn(1:n, 2)
v<-dim(b)[2];vv<-dim(a)[2]
MAT<-NULL;A<-1;y<-1
while(A<=vv) {
for (k in 1:v){
s<-a[,A];ss<-b[,k]
MAT[[y]]<-as.vector(t(mt[ss,s]))
y<-y+1}
A<-A+1}
return(Reduce("rbind",MAT))}
if (ty=="a"){
V<-n*l;mat<-NULL;lamda<-NULL
D<-matrix(1:V, nrow=n,byrow=TRUE)
for (i in 1:2){
j<-i-1
mt<-D+(V*j)
mat[[i]]<-Op(mt,n,l,S)}
BIB<-Reduce("cbind",mat)
T<-BIB[1,1];R<-length(which(T==BIB))
lamda[1]<-(n-1)*choose(l-2,S-2);lamda[2]<-choose(l-1,S-1);lamda[3]<-choose(l-2,S-2)
lamda[4]<-R;lamda[5]<-lamda[1];lamda[6]<-lamda[2];lamda[7]<-lamda[3]
return(list(PBIB=BIB,Type="Rectangular right angular PBIB7 designs",V=2*V,B=dim(BIB)[1],R=R,K=2*S,lamda=lamda))}
if (ty=="b")
{DDD<-NULL;PBIB<-NULL;V<-n*l;lamda<-NULL
DDD[[1]]<-matrix(1:V, nrow=n,byrow=TRUE)
DDD[[3]]<-DDD[[1]];DDD[[2]]<-DDD[[1]]+V
#par lignes
fiflin<-function(DDD,S,n,l){
#par lignes
zzz<-NULL;mat<-NULL;BIB<-NULL
for (m in 1:2) {D<-DDD[[m]];mt<-DDD[[m+1]]
n<-dim(D)[1]
for (o in 1:n) {MT<-mt[-o,]
DD<-rbind(D[o,],MT)
QQ<-D[o,]
at<-Op(DD,n,l,S)
k<-dim(at)[1]
for (j in 1:k) {if (any(at[j,1]==QQ)) {zzz[[j]]<-at[j,]}}
mat[[o]]<-Reduce("rbind",zzz)}
BIB[[m]]<-Reduce("rbind",mat)}
return(Reduce("rbind",BIB))}
#par colonnes
fifcol<-function(DDD,S,n,l){zzz<-NULL
mat<-NULL;BIB<-NULL
for (m in 1:2) {D<-DDD[[m]];mt<-DDD[[m+1]]
for (o in 1:l) {
mt[,o]<-D[,o]
QQ<-D[,o]
DD<-mt
at<-Op(DD,n,l,S)
k<-dim(at)[1]
for (j in 1:k) {if (any(at[j,1]==QQ)) {zzz[[j]]<-at[j,]}}
mat[[o]]<-Reduce("rbind",zzz)}
BIB[[m]]<-Reduce("rbind",mat)}
return(Reduce("rbind",BIB))}

PBIB[[1]]<-fiflin(DDD,S,n,l)
PBIB[[2]]<-fifcol(DDD,S,n,l)
PBIB<-Reduce("rbind",PBIB)
T<-PBIB[1,1];R<-length(which(T==PBIB))
lamda[1]<-(2*(n-1)*choose(l-2,S-2))+((l-2)*choose(l-3,S-3));lamda[2]<-(choose(l-1,S-1))+((l-1)*choose(l-2,S-2))
lamda[3]<-(l-2)*choose(l-3,S-3);lamda[4]<-0;lamda[5]<-2*(n-1)*choose(l-2,S-2);lamda[6]<-2*choose(l-1,S-1)
lamda[7]<-4*choose(l-2,S-2)
return(list(PBIB=PBIB,Type="Rectangular right angular PBIB7 design",V=2*V,B=dim(PBIB)[1],R=R,K=2*S,lamda=lamda))}}}
