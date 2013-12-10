PBIB4 <-
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
BIB<-Reduce("rbind",mat)
T<-BIB[1,1];R<-length(which(T==BIB))
lamda[1]<-(n-1)*choose(l-2,S-2);lamda[2]<-choose(l-1,S-1);lamda[3]<-choose(l-2,S-2);lamda[4]<-0
return(list(PBIB=BIB,Type="Rectangular right angular PBIB4 design",V=2*V,B=dim(BIB)[1],R=R,K=2*S,lamda=lamda))}
if (ty=="b")
{DDD<-NULL;PBIB<-NULL;zzz<-NULL
mat<-NULL;BIB<-NULL;V<-n*l;lamda<-NULL
DDD[[1]]<-matrix(1:V, nrow=n,byrow=TRUE)
DDD[[3]]<-DDD[[1]];DDD[[2]]<-DDD[[1]]+V
for (m in 1:2) {D<-DDD[[m]];mt<-DDD[[m+1]]
for (o in 1:l) {
Q<-c(1,D[,o],D[,o])
for(i in 1:l){
P<-Q[-(1:i)]
QQ<-P[1:n]
DD<-cbind(QQ,mt)
at<-Op(DD,n,l,S)
k<-dim(at)[1]
for (j in 1:k) {if (any(at[j,1]==QQ)) {zzz[[j]]<-at[j,]}}
mat[[i]]<-Reduce("rbind",zzz)}
BIB[[o]]<-Reduce("rbind",mat)}
PBIB[[m]]<-Reduce("rbind",BIB)}
PBIB<-Reduce("rbind",PBIB)
T<-PBIB[1,1];R<-length(which(T==PBIB))
lamda[1]<-l*n*(n-1)*choose(l-2,S-3);lamda[2]<-n*(choose(l,S-1)+(l*choose(l-1,S-2)));lamda[3]<-n*l*choose(l-2,S-3);lamda[4]<-4*(n-1)*choose(l-1,S-2)
return(list(PBIB=PBIB,Type="Rectangular right angular PBIB4 design (nonzero lamda)",V=2*V,B=dim(PBIB)[1],R=R,K=2*S,lamda=lamda))}}}
