# Load the relevant library
library(scatterplot3d)

n = 50 # Sample size

# Set the seed for reproducibility
set.seed(2)

#Create data
x1 = rnorm(n,3,1)
x2 = rbinom(n, size = 100, prob = 0.05)
x3 = runif(n,0,1)
x = rbind(x1,x2,x3)
ep = rnorm(n,0,0.5)

# Create y
# The true values of beta are (1,2,3)^T and (-1,-2,-3)^T
y = rep(0,n)
for (i in 1:n/2) {
  y[i] = x1[i]+2*x2[i]+3*x3[i]+ep[i]
}
for (i in (1+(n/2)):n){
  y[i] = 2*x1[i]+x2[i]+2*x3[i]+ep[i]
}


# Define the range of the virtual feature space and step size
beta1 = seq(0,4,by=0.05)
beta2 = seq(0,4,by=0.05)
beta3 = seq(0,4,by=0.05)

# Create an array to store the residuals
resijk = array(0,dim = c(length(beta1),length(beta2),length(beta3),n))

# Create an array to store the number of residuals within a threshold
resn = array(0,dim = c(length(beta1),length(beta2),length(beta3)))

# Calculate residuals and count the number of residuals within a threshold
for (i in 1:length(beta1)) {
  for (j in 1:length(beta2)) {
    for (k in 1:length(beta3)) {
      for (m in 1:n){
        resijk[i,j,k,m] = y[m] - beta1[i]*x1[m]-beta2[j]*x2[m]-beta3[k]*x3[m]
        if(abs(resijk[i,j,k,m])<0.7) {resijk[i,j,k,m] = 0}
        else {resijk[i,j,k,m] = 1}
      }
    }
  }
}

# Create an array to store the residual ratios
rsd_ratio = array(0,dim = c(length(beta1),length(beta2),length(beta3)))

# Mark the points whose residual ratios are greater than p-value
Ps = array(0,dim = c(length(beta1),length(beta2),length(beta3)))
for (i in 1:length(beta1)){
  for(j in 1:length(beta2)){
    for (k in 1:length(beta3)){
      for (m in 1:n){
        resn[i,j,k] = resn[i,j,k] + resijk[i,j,k,m]
      }
      rsd_ratio[i,j,k] = 1-resn[i,j,k]/n
      if(rsd_ratio[i,j,k] > 0.44){Ps[i,j,k] = 1}
    }
  }
}

# Find the indices of points to be plotted and classify
w = which(Ps==1,arr.ind = T) 
xax = beta1[w[,1]]
yax = beta2[w[,2]]
zax = beta3[w[,3]]
types=rep(0,length(xax))
types[1]=4
subgroup1=xax[1]
subgroup2=c()
for(i in 2:length(xax)){
  if(xax[i]<1.4){
    subgroup1=c(subgroup1,xax[i])
    types[i]=2
  }
  else {
    subgroup2=c(subgroup2,xax[i])
    types[i]=4
  }
}

# Plot accordingly
df=data.frame(x=xax,y=yax,z=zax)
scatterplot3d(df, pch = 16, angle=70,
              grid=TRUE, box=FALSE,xlab = '',
              ylab = '',
              zlab = '',color=types,bg="#545454")
