# Load the relevant library
library(ggplot2)

n = 1000 # Sample size

# Set the seed for reproducibility
set.seed(1)

#Create data
x1 = rnorm(n,0,1)
x2 = rnorm(n,3,1)
x = rbind(x1,x2)
ep = rnorm(n,0,0.5)

# Create y
# The true values of beta are (1,1)^T and (-1,-1)^T
y = rep(0,n)
for (i in 1:n/2) {
  y[i] = x1[i]+x2[i]+ep[i]
}
for (i in (1+(n/2)):n){
  y[i] = -x1[i]-x2[i]+ep[i]
}


# Define the range of the virtual feature space and step size
beta1 = seq(-2,2,by=0.05)
beta2 = seq(-2,2,by=0.05)

# Create an array to store the residuals
resij = array(0,dim = c(length(beta1),length(beta2),n))

# Create an array to store the number of residuals within a threshold
resn = array(0,dim = c(length(beta1),length(beta2)))

# Calculate residuals and count the number of residuals within a threshold
for (i in 1:length(beta1)) {
  for (j in 1:length(beta2)) {
    for (k in 1:n) {
      resij[i,j,k] = y[k] - beta1[i]*x1[k]-beta2[j]*x2[k]
      if(abs(resij[i,j,k])<0.9) {resij[i,j,k] = 0}
      else {resij[i,j,k] = 1}
    }
  }
}

# Create an array to store the residual ratios
rsd_ratio= array(0,dim = c(length(beta1),length(beta2)))

# Mark the points whose residual ratios are greater than p-value
Ps = array(0,dim = c(length(beta1),length(beta2)))
for (i in 1:length(beta1)) {
  for(j in 1:length(beta2)){
    for (k in 1:n) {
      resn[i,j] = resn[i,j] + resij[i,j,k] # resn中每个位置保存每个格点的残差之和，此值越大，说明不符合的程度高，即该点越不可能成为回归系数的近似值
    }
    rsd_ratio[i,j] = 1-resn[i,j]/n # 这里等号左侧的resn（为一个介于0和1之间的数）越大则原来的resn越小，则说明该点越可能成为回归系数的近似值，模拟中的residual ratio
    if(rsd_ratio[i,j] > 0.4){Ps[i,j] = 1} # 将大于0.4的位置记为1
  }
}

# Find the indices of points to be plotted and classify
w = which(Ps==1,arr.ind = T) 
xax = beta1[w[,1]]
yax = beta2[w[,2]]
types=rep(0,length(xax))
types[1]=2
subgroup1=xax[1]
subgroup2=c()
for(i in 2:length(xax)){
  if(sqrt((xax[i]-xax[1])^2+(yax[i]-yax[1])^2)<1.5){
    subgroup1=c(subgroup1,xax[i])
    types[i]=2
  }
  else {
    subgroup2=c(subgroup2,xax[i])
    types[i]=4
  }
}

# Plot accordingly
df=data.frame(x=xax,y=yax)
p<-ggplot(df, aes(x=xax, y=yax,color=types)) + 
  geom_point(color=types)+
  coord_cartesian(xlim = c(-2, 2), ylim = c(-2, 2))+
  theme(axis.title = element_blank())
plot(p)
