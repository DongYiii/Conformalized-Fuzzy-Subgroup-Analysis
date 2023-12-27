# Load the ggplot2 library
library(ggplot2)

n = 900 # Sample size

# Set the seed for reproducibility
set.seed(3)

x = rnorm(n,3,1)
ep = rnorm(n,0,0.3)

# Create y
# The true values of beta are 1, 0, and -1
y = rep(0,n) # y为n个0
for (i in 1:n/3) {
  y[i] = x[i]+ep[i]
}
for (i in (1+(n/3)):(2/3*n)){
  y[i] = ep[i]
}
for (i in (2/3*n+1):n) {
  y[i] = -x[i]+ep[i]
}

# Define the range of the virtual feature space and step size
beta = seq(-2,2,by=0.05)

# Create an array to store the residuals
resij = array(0,dim = c(length(beta),n))

# Create an array to store the number of residuals within a threshold
resn = array(0,dim = c(length(beta)))

# Calculate residuals and count the number of residuals within a threshold
for (i in 1:length(beta)) {
  for (j in 1:n) {
    resij[i,j] = y[j] - beta[i]*x[j]
    if(abs(resij[i,j])<1) {resij[i,j] = 0}
    else {resij[i,j] = 1}
  }
}

# Create an array to store the residual ratios
rsd_ratio = array(0,dim = c(length(beta)))

# Mark the points whose residual ratios are greater than p-value
Ps = array(0,dim = c(length(beta)))
for (i in 1:length(beta)) {
  for(j in 1:n){
    resn[i] = resn[i] + resij[i,j]
  }
  rsd_ratio[i] = 1-resn[i]/n
  if(rsd_ratio[i] > 0.18){Ps[i] = 1} 
}

# Find the indices of points to be plotted and classify
w = which(Ps==1,arr.ind = T) 
xax = beta[w[,1]]
types=rep(0,length(xax))
types[1]=2
for(i in 2:length(xax)){
  if((xax[i]-xax[1])<0.66){
    types[i]=2
  }
  else if((xax[i]-xax[1])>0.66&&(xax[i]-xax[1])<1.7){
    types[i]=4
  }
  else {
    types[i]=3
  }
}

# Plot accordingly
y=rep(-0.05,length(xax))
df=data.frame(x=xax,y=y)
p1<-ggplot(df, aes(x=xax, y=y)) +
  annotate("segment",x=-2.5,xend=2.5, y=-0.05, yend=-0.05, size=1)+
  coord_cartesian(xlim = c(-2.5, 2.5), ylim = c(0, 0))+
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank())+
  geom_point(col=types,size=2)
plot(p1)

p2<-ggplot(df, aes(x=xax, y=-1)) +
  annotate("segment",x=-2.5,xend=2.5, y=-1, yend=-1, size=0.5)+
  geom_point(size =2,col=types) +
  scale_x_continuous(limits = c(-2.5,2.5)) +
  scale_y_continuous(limits = c(-1,0)) + 
  theme(panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank())
plot(p2)

df=data.frame(x=beta,y=rsd_ratio)
p3<-ggplot(df, aes(x=beta, y=rsd_ratio)) +
  geom_line( color="blue", size=1, alpha=0.9)+
  geom_point(color="red", size=1)+
  theme(axis.title = element_blank())
plot(p3)


