PBIB5 <-
function(n,l,S){
if (S>=l || S<2) {"Choose : 2 < S < l"}
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

DDD<-NULL;zzz<-NULL;lamda<-NULL
mat<-NULL;BIB<-NULL;V<-n*l
DDD[[1]]<-matrix(1:V, nrow=n,byrow=TRUE)
DDD[[3]]<-DDD[[1]];DDD[[2]]<-DDD[[1]]+V
for (m in 1:2) {D<-DDD[[m]];mt<-DDD[[m+1]]
for (o in 1:l) {
QQ<-D[,o]
DD<-cbind(QQ,mt)
at<-Op(DD,n,l,S)
k<-dim(at)[1]
for (j in 1:k) {if (any(at[j,1]==QQ)) {zzz[[j]]<-at[j,]}}
mat[[o]]<-Reduce("rbind",zzz)}
BIB[[m]]<-Reduce("rbind",mat)}
PBIB<-Reduce("rbind",BIB)
T<-PBIB[1,1];R<-length(which(T==PBIB))
lamda[1]<-l*(n-1)*choose(l-2,S-3);lamda[2]<-choose(l,S-1)+(l*choose(l-1,S-2))
lamda[3]<-l*choose(l-2,S-3);lamda[4]<-2*(n-1)*choose(l-1,S-2);lamda[5]<-2*choose(l-1,S-2)
return(list(PBIB=PBIB,Type="Rectangular right angular PBIB5 design",V=2*V,B=dim(PBIB)[1],R=R,K=2*S,lamda=lamda))}}
