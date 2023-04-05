source("dm.R")
source("../lib/k_nn.R")
#### Q1 start ###
#### Q1a start ###

# read file and save to dc
dc<-read.csv("bank-market1.csv", na.strings="")

#Change the last column in dc to numeric: deposit=1 or 0 for deposit="yes" or "no" respectively and save them in d.

dc$deposit[dc$deposit=="yes"] <- as.numeric(1)
dc$deposit[dc$deposit=="no"] <- as.numeric(0)
d <- transform(dc, deposit = as.numeric(deposit))

#### Q1a end ###
#### Q1b start ###
set.seed(82475)
# save random 80% data of d to d0 as training set
out<-ransub(d, 0.8)
d0<-out$train
d1<-out$test
y0<-factor(d0[,12])
#### Q1b end ###

#### Q1c start ###
z0<-scale.con(d0[,c(1,4,7:10)])		# transform continuous or ordinal var.
z1<-scale.con(d1[,c(1,4,7:10)])
# print(z1)

#### Q1c end ###

#### Q1d start ###
bank.knn<-k_nn(z0,z1,y0,d1[,12],v=10)
(tab<-table(bank.knn,d1[,12]))		# classification table
print(tab)
print(erate(tab))	
#### Q1d end ###

#### Q1e start ###
f1sc<-function(tab) { # assume the input tab is 2x2 with 1st row an column as negative
	tp<-tab[2,2]
	fp<-tab[1,2]
	fn<-tab[2,1]
	prec<-tp/(fp+tp)
	recall<-tp/(fn+tp)
	f1=2*prec*recall/(prec+recall)
	er<-erate(tab)
	cat('erate =',er, 'precision =', prec, ' recall =', recall, 'F1 score =', f1, '\n')
}
f1sc(tab)
#### Q1e end ###
#### Q1 end ###


#### Q2 start ###

#### Q2a start ###
if(!require(e1071)) {install.packages('e1071')}
library(e1071)		
cl<-factor(d0[,12])	
bank.nb<-naiveBayes(d0[,1:11],cl)
#### Q2a end ###

#### Q2b start ###
prob<-predict(bank.nb, d1[,1:11], type="raw")
#### Q2b end ###

#### Q2c start ###
pr<-(prob[,2]>0.5)
(tab<-table(pr,d1[,12])) # classification table
print(tab)
print(erate(tab))
#### Q2c end ###

#### Q2d start ###
f1sc(tab)
#### Q2d end ###

#### Q2e start ###
for (x in 1:9) {
	print(x/10)
  	pr_temp<-(prob[,2]>(x/10))
  	f1sc(table(pr_temp,d1[,12]))
}
#### Q2e end ###
#### Q2 end ###

#### Q3 start ###

#### Q3a start ###
lreg<-glm(deposit~age+balance+duration+campaign+pdays+previous,data=d0,binomial)
bank.lreg<-step(lreg)
#### Q3a end ###

#### Q3b start ###
pr0<-(bank.lreg$fit>0.5)+0
(tab<-table(pr0,d0$deposit))
print(tab)			
f1sc(tab)
#### Q3b end ###

#### Q3c start ###
prob<-predict(bank.lreg,d1,type='response')	# predition on d1 using type=response
pr<-(prob>0.5)+0				# out-sample prediction
(tab<-table(pr,d1$deposit))	
print(tab)
f1sc(tab)
#### Q3c end ###

#### Q3d start ###
for (x in 1:9) {
	print(x/10)
	pr_temp<-(prob>(x/10))+0	
  	f1sc(table(pr_temp,d1[,12]))
}
#### Q3d end ###
#### Q3 end ###





