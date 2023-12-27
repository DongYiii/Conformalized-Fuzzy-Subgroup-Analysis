# Load the relevant libraries
library("foreign")
library(ggplot2)

# Read and merge data
mydata1<-read.xport(".../.../DEMO_G.XPT")
mydata2<-read.xport(".../.../BMX_G.XPT")
merged_data<-merge(mydata1,mydata2,by="SEQN",all=TRUE)
n=500

# Deal with NA
for(i in 1:ncol(merged_data)) {
  # Check if the i-th column of the data box is numerical data. If so, replace the missing value (NA) in that column with the average value of that column
  if(is.numeric(merged_data[[i]])) {
    merged_data[[i]][is.na(merged_data[[i]])] <- mean(merged_data[[i]], na.rm = TRUE)
  }
}

lm1<-lm(merged_data$BMXHT~merged_data$RIDAGEYR+merged_data$BMXWT+merged_data$BMXWAIST,data=merged_data)
print(summary(lm1))

BMXHT=t(merged_data$BMXHT)
RIDAGEYR=t(merged_data$RIDAGEYR)
BMXWT=t(merged_data$BMXWT)
BMXWAIST=t(merged_data$BMXWAIST)
RIAGENDR=t(merged_data$RIAGENDR)
BMXBMI=t(merged_data$BMXBMI)

# Define the range of the virtual feature space and step size
beta1 = seq(0,1,by=0.01)
beta2 = seq(0,1,by=0.01)

# Create an array to store the residuals
resij = array(0,dim = c(length(beta1),length(beta2),n))

# Create an array to store the number of residuals within a threshold
resn = array(0,dim = c(length(beta1),length(beta2)))

# Calculate residuals and count the number of residuals within a threshold
for (i in 1:length(beta1)) {
  for (j in 1:length(beta2)) {
    for (k in 1:n) {
      resij[i,j,k] = BMXHT[k] - 0.07132*RIDAGEYR[k]-beta1[i]*BMXWT[k]-beta2[j]*BMXWAIST[k]-108.8
      if(abs(resij[i,j,k])<2.5) {resij[i,j,k] = 1} 
      else {resij[i,j,k] = 0}
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
    rsd_ratio[i,j] = resn[i,j]/n 
    if(rsd_ratio[i,j] > 0.17){Ps[i,j] = 1}
  }
}

# Find the indices of points to be plotted and classify
w = which(Ps==1,arr.ind = T) 
xax = beta1[w[,1]]
yax = beta2[w[,2]]
types=rep(0,length(xax))
types[1]=2
for(i in 2:length(xax)){
  if(yax[i]<0.48){
    types[i]=2
  }
  else {
    types[i]=4
  }
}

# Plot accordingly
df=data.frame(x=xax,y=yax)
p1<-ggplot(df, aes(x=xax, y=yax)) +
  geom_point(size=1,col=types)+
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
persp(beta1,beta2,rsd_ratio,col = color[facetcol], phi = 20, theta = 55 ,xlab = '', ylab = '', zlab = '', main = NULL, sub = NULL)

# Determine which subgroup each individual belongs to
sbgrp=rep(0,n)
sb1_num=0
for(i in 1:n){
  r2=BMXHT[i] - 0.07132*RIDAGEYR[i]-0.04*BMXWT[i]-0.52*BMXWAIST[i]-108.8
  r1=BMXHT[i] - 0.07132*RIDAGEYR[i]-0.41*BMXWT[i]-0.22*BMXWAIST[i]-108.8
  if(r2<r1){
    sbgrp[i]=2
  }
  else{
    sbgrp[i]=1
    sb1_num=sb1_num+1
  }
}

# Calculate the proportion of individuals with BMI indicators above 24 in each subgroup
overweight_ratio=rep(0,2)
for(i in 1:n){
  if(BMXBMI[i]>=24){
    if(sbgrp[i]==1){
      overweight_ratio[1]=overweight_ratio[1]+1
    }
    else{
      overweight_ratio[2]=overweight_ratio[2]+1
    }
  }
}
overweight_ratio[1]=overweight_ratio[1]/sb1_num
overweight_ratio[2]=overweight_ratio[2]/(n-sb1_num)
print(overweight_ratio)