# Load the ggplot2 library
library(ggplot2)

# Set the seed for reproducibility
set.seed(1)

n = 300
x = rnorm(n,0,1)
ep = rnorm(n,0,0.2)

y = rep(0,n)
for (i in 1:n/2) {
  random_number1 <- runif(n/2, 1, 2)

  
  y[i] =random_number1[i]*x[i]+1*x[i]^2+ep[i]
}
for (i in (1+(n/2)):n){
  
  y[i] = 5*x[i]+1*x[i]^2+ep[i]
}

# Define the range of beta1 and beta2 values
beta1 = seq(0,7,by=0.05)
beta2 = seq(-1,2,by=0.05)

# Create an array to store the residuals
resij = array(0,dim = c(length(beta1),length(beta2),n))
# Create an array to store the number of residuals within a threshold
resn = array(0,dim = c(length(beta1),length(beta2)))

# Calculate residuals and count the number of residuals within a threshold
for (i in 1:length(beta1)) {
  for (j in 1:length(beta2)) {
    for (k in 1:n){
      resij[i,j,k] = y[k] - beta1[i]*x[k]-beta2[j]*x[k]^2
      if(abs(resij[i,j,k])<0.4) {resij[i,j,k] = 0}
      else {resij[i,j,k] = 1}
    }
  }
}

# Calculate the proportion of residuals within a threshold and decide whether to plot a point
p = array(0,dim = c(length(beta1),length(beta2)))
for (i in 1:length(beta1)) {
  for(j in 1:length(beta2)){
    for (k in 1:n) {
      resn[i,j] = resn[i,j] + resij[i,j,k]
    }
    resn[i,j] = 1-resn[i,j]/n
    if(resn[i,j] >0.40){p[i,j] = 1}
  }
}

w = which(p==1,arr.ind = T)
xax = beta1[w[,1]]
yax = beta2[w[,2]]

# Plot the scatter plot
types=rep(0,length(xax))
types[1]=2
subgroup1=xax[1]
subgroup2=c()
for(i in 2:length(xax)){
  if(sqrt((xax[i]-xax[1])^2+(yax[i]-yax[1])^2)<3){
    subgroup1=c(subgroup1,xax[i])
    types[i]=2
  }
  else {
    subgroup2=c(subgroup2,xax[i])
    types[i]=4
  }
}

df=data.frame(x=xax,y=yax)

p<-ggplot(df, aes(x,y,color=types)) + 
  geom_point(color=types)+
  coord_cartesian(xlim = c(0, 6.5), ylim = c(-0.5,2))+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
plot(p)

# Create a 3D plot of beta1, beta2, and the proportion of residuals within a threshold
persp(beta1, beta2, resn, phi = 15, theta = 150, xlab = "", ylab = "",zlab = "")


