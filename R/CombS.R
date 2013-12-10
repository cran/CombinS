CombS <-
function(n,l,S){
V<-n*l
if (S>l || S<=1) {"Choose : 1 < S <= l"}
else {
lamda<-NULL
Op<-function(n,l,S){
V<-n*l
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
BIB<-Op(n,l,S)
T<-BIB[1,1];R<-length(which(T==BIB))
if (l==S){lamda[[1]]<-(n-1)*choose(l-2,S-2);lamda[[2]]<-choose(l-1,S-1);lamda[[3]]<-choose(l-2,S-2)
return(list(PBIB=BIB,Type="Singular group divisible design",V=V,B=dim(BIB)[1],R=R,K=2*S,lamda=lamda))}
else {lamda[[1]]<-R;lamda[[2]]<-1
return(list(PBIB=BIB,Type="Rectangular PBIB design",V=V,B=dim(BIB)[1],R=R,K=2*S,lamda=lamda))}}}
