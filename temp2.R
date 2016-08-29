
# Now we find the whole network connectivity measures for each:
k1=SoftConnectivity.fromSimilarity(datExpr1,6)
k2=SoftConnectivity(datExpr2,6)

# We would like to normalize the connectivity measures.  We do this by dividing by the 
# maximum values.
K1=k1/max(k1)
K2=k2/max(k2)

# Now we find the difference between the two connectivity values.
DiffK=K1-K2 # Note that we did not take the absolute value here.  
# Negative values of this difference imply that normalized Lean connectivity (k2) is 
# greater than K1, fat normalized connectivity.

poolRest=as.logical(rest1+rest2)
factorLevels=rep(NA,length(poolRest))
factorLevels[rest1]="lowWeight" # trait 1 is low weight
factorLevels[rest2]="highWeight" # trait 2 is high weight

ttest=rep(NA, dim(datExpr)[[2]])
# Let's determine the t-statistic.
for (i in 1:dim(datExpr)[[2]]){
  ttest[i]=t.test(datExpr[poolRest,i]~factorLevels[poolRest])$statistic
}
