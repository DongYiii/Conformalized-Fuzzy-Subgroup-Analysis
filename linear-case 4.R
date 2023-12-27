# Load the relevant library
library(ggplot2)

n = 1000 # Sample size

# Set the seed for reproducibility
set.seed(1)

#Create data
x1 = rnorm(n,0,1)
x2 = rnorm(n,3,1)
x = rbind(x1,x2)
ep = rnorm(n,0,0.01)

# Create y under the fuzzy setting
y = rep(0,n)
beta_i1=runif(n/2,0.2,0.4)
beta_i2=runif(n/2,0.6,0.8)
for (i in 1:n/2) {
  y[i] = beta_i1[i]*x1[i]+beta_i2[i]*x2[i]+ep[i]
}
for (i in (1+n/2):n){
  y[i] = 0.7*x1[i]+0.3*x2[i]+ep[i]
}

# Define the range of the virtual feature space and step size
beta1 = seq(0,1,by=0.015)
beta2 = seq(0,1,by=0.015)

# Create an array to store the residuals
resij = array(0,dim = c(length(beta1),length(beta2),n))

# Create an array to store the number of residuals within a threshold
resn = array(0,dim = c(length(beta1),length(beta2)))

# Calculate residuals and count the number of residuals within a threshold
for (i in 1:length(beta1)) {
  for (j in 1:length(beta2)) {
    for (k in 1:n) {
      resij[i,j,k] = y[k] - beta1[i]*x1[k]-beta2[j]*x2[k]
      if(abs(resij[i,j,k])<0.35) {resij[i,j,k] = 0}
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
      resn[i,j] = resn[i,j] + resij[i,j,k]
    }
    rsd_ratio[i,j] = 1-resn[i,j]/n 
    if(rsd_ratio[i,j] > 0.48){Ps[i,j] = 1}
  }
}

# Find the indices of points to be plotted and classify
w = which(Ps==1,arr.ind = T) 
xax = beta1[w[,1]]
yax = beta2[w[,2]]

types=rep(0,length(xax))
for(i in 1:length(xax)){
  if(yax[i]>0.5){
    types[i]=2
  }
  else{
    types[i]=4
  }
}

# Plot accordingly
df=data.frame(x=xax,y=yax)
p1<-ggplot(df, aes(x=xax, y=yax)) +
  geom_point(col=types,size=1)+
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))+
  theme(axis.title = element_blank())
plot(p1)

nrz <- nrow(rsd_ratio)
ncz <- ncol(rsd_ratio)
jet.colors <- colorRampPalette( c("blue", "green") )
nbcol <- 100
color <- jet.colors(nbcol)
zfacet <- rsd_ratio[-1, -1] + rsd_ratio[-1, -ncz] + rsd_ratio[-nrz, -1] + rsd_ratio[-nrz, -ncz]
facetcol <- cut(zfacet, nbcol)
persp(beta1,beta2,rsd_ratio,col = color[facetcol], phi = 25, theta = 74 ,xlab = '', ylab = '', zlab = '', main = NULL, sub = NULL, shade = 0.1)

