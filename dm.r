# Contains functions useful for pre-processing data
# scale.con(d), scale.dum(d), stand(d), mdist(d), ransub(d,r,seed), cat2int(d)

# par(mfrow=c(1,1))		# reset multiframe graph to 1x1
# arrangeWindows("vertical")	# arrange window to vertical mode

# function to scale continuous or ordinal variables to [0,1]
scale.con<-function(d) {
  n<-nrow(d); p<-ncol(d)	# get row and col. dim of d
  cmin<-apply(d,2,min)		# column min of d
  cmax<-apply(d,2,max)		# column max of d
  range<-cmax-cmin		# column range
  cmin<-matrix(cmin,nr=n,nc=p,byrow=T)	  # change cmin to a nxp matrix
  range<-matrix(range,nr=n,nc=p,byrow=T)  # change range to a nxp matrix
  (d-cmin)/range		# transform d
}  


# function to convert categorical variable with k levels to (k-1) dummy variables
cat2dum<-function(v) {
  v<-factor(v)			# change v to factor
  lab<-levels(v)	  	# get value in v
  k<-length(lab)	  	# get no. levels in v
  outer(v,lab,"==")+0  		# create matrix with k columns
}  

# function to convert nominal variables to dummy variables
# use cat2dum()
scale.dum<-function(d) {
  n<-nrow(d); p<-ncol(d)	# get row and col dim of d
  x<-NULL		# initalize x
  for (i in 1:p) {
    v<-d[,i]		# get the i-th col in d
    z<-cat2dum(v)	# convert into z, matrix dummy var.
    x<-cbind(x,z)	# column-binding of z
  }
  k<-dim(x)[2]		# col. dim of x
  nh<-apply(x,2,sum)			# compute frequency of each category
  nk<-matrix(nh,nrow=n,ncol=k,byrow=T)	# create nxk matrix with each rows is nh
  x/(2*nk)				# output
}

# function to standardize continuous variables
  stand<-function(d) {
  n<-nrow(d); p<-ncol(d)		# get row and col dim of d
  m<-apply(d,2,mean)			# compute column mean
  s<-apply(d,2,sd)			# compute column sd
  m<-matrix(m,nr=n,nc=p,byrow=T)	# convert m into nxp matrix, each row is m
  s<-matrix(s,nr=n,nc=p,byrow=T)	# convert s into nxp matrix, each row is s
  (d-m)/s				# output standardize score
}


# input data matrix x, assume no missing values
# output mahalanobis distance of x
mdist<-function(x) {
   t<-as.matrix(x)	# convert to matrix
   m<-apply(t,2,mean)	# get column mean
   s<-var(t)		# get cov matrix
   mahalanobis(t,m,s)	# compute mdist
}


# create random partition for training and testing dataset wtih sampling ratio r
# input: d, r, seed	output: id, train, test
ransub<-function(d,r,seed=12345) {
  set.seed(seed)  				# set random seed, default=12345
  n<-nrow(d)					# get row size of d
  id<-sample(1:n,size=round(n*r),replace=F)	# generate id
  train<-d[id,]					# training data
  test<-d[-id,]					# testing data
  out<-list(id,train,test)			# create output list
  names(out)<-c('id','train','test')		# apply names to out
  out						# output 
}


# function to recode cat. var. to integer in data.frame x
# assign 1,2,.. to levels with alphabetical order

cat2int<-function(x) {
  out<-x				# save a copy of x
  for (i in 1:ncol(x)) {		# for each column i  
    v<-x[,i]				# save x[,i] to v
    if (is.factor(v)|is.character(v)) 	# if v is factor or character
      out[,i]<-as.integer(factor(v)) 	# convert v to integer
  }
  out					# output
}


# compute error rate of tab
erate<-function(tab) {1-sum(diag(tab))/sum(tab)} 
