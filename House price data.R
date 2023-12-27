#Load the required package
library(readxl)
library(ggplot2)

#Read data from an Excel file
data <- read_excel(".../.../...xlsx")
y <- data[[8]]
n=length(y)

#Extract variable
x1=data[[1]]
x2=data[[2]]
x3=data[[3]]
x4=data[[4]]
x5=data[[5]]

# Define the range of beta1 and beta2 values
beta1 = seq(-5,35,by=0.1)
beta2 = seq(-2,2,by=0.01)

# Create an array to store the residuals
resij = array(0,dim = c(length(beta1),length(beta2),length(y)))
# Create an array to store the number of residuals within a threshold
resn = array(0,dim = c(length(beta1),length(beta2)))

# Calculate residuals and count the number of residuals within a threshold
for (i in 1:length(beta1)) {
  for (j in 1:length(beta2)) {
    for (k in 1:n){
      resij[i,j,k] = y[k] - beta1[i]*x1[k]^2-beta2[j]*x1[k]^3+3017*x2[k]-7774*x3[k]+8000*x4[k]-8000*x5[k]
      if(abs(resij[i,j,k])<8000) {resij[i,j,k] = 0}
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
    if(resn[i,j] >0.19){p[i,j] = 1}
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
for(i in 1:length(xax)){
  if(sqrt((xax[i]-5)^2+(yax[i]-0)^2)<14){
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
  coord_cartesian(xlim = c(-5, 35), ylim = c(-1, 1))+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
plot(p)

# Create a 3D plot of beta1, beta2, and the proportion of residuals within a threshold
persp(beta1, beta2, resn, phi = 11, theta = 100, xlab = "", ylab = "",zlab = "")


