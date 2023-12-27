# Load the ggplot2 library
library(ggplot2)

# Set the seed for reproducibility
set.seed(1)

n = 300
x = rnorm(n, 0, 1)
ep = rnorm(n, 0, 0.2)
y = rep(0, n)
for (i in 1:(n/2)) {
  y[i] = 5 * sin(x[i]) + ep[i]
}
for (i in (1 + (n/2)):n) {
  y[i] = 1 * x[i]^2 + 2 * x[i] + ep[i]
}

# Define the range of beta1 and beta2 values
beta1 = seq(-2, 3, by = 0.05)
beta2 = seq(-2, 8, by = 0.05)

# Create an array to store the residuals
resij = array(0, dim = c(length(beta1), length(beta2), n))

# Create an array to store the number of residuals within a threshold
resn = array(0, dim = c(length(beta1), length(beta2)))

# Calculate residuals and count the number of residuals within a threshold
for (i in 1:length(beta1)) {
  for (j in 1:length(beta2)) {
    for (k in 1:n) {
      resij[i, j, k] = y[k] - beta1[i] * x[k]^2 - beta2[j] * x[k]
      if (abs(resij[i, j, k]) < 0.4) {
        resij[i, j, k] = 0
      } else {
        resij[i, j, k] = 1
      }
    }
  }
}

# Calculate the proportion of residuals within a threshold and decide whether to plot a point
p = array(0, dim = c(length(beta1), length(beta2)))
for (i in 1:length(beta1)) {
  for (j in 1:length(beta2)) {
    for (k in 1:n) {
      resn[i, j] = resn[i, j] + resij[i, j, k]
    }
    resn[i, j] = 1 - resn[i, j] / n
    if (resn[i, j] > 0.41) {
      p[i, j] = 1
    }
  }
}

# Find the indices of points to be plotted
w = which(p == 1, arr.ind = TRUE)
xax = beta1[w[, 1]]
yax = beta2[w[, 2]]

# Plot the scatter plot
types = rep(0, length(xax))
types[1] = 2
subgroup1 = xax[1]
subgroup2 = c()
for (i in 2:length(xax)) {
  if (sqrt((xax[i] - xax[1])^2 + (yax[i] - yax[1])^2) < 2) {
    subgroup1 = c(subgroup1, xax[i])
    types[i] = 2
  } else {
    subgroup2 = c(subgroup2, xax[i])
    types[i] = 4
  }
}

df = data.frame(x = xax, y = yax)

p <- ggplot(df, aes(x, y, color = types)) + 
  geom_point(color = types) +
  coord_cartesian(xlim = c(-1.5, 2), ylim = c(-2, 8)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

plot(p)

# Create a 3D plot of beta1, beta2, and the proportion of residuals within a threshold
persp(beta1, beta2, resn, phi = 15, theta = 115, xlab = "", ylab = "", zlab = "")

