# variables
mu_x = 3
mu_y = 5

sd_x = 1.5
sd_y = 2

N = 30

t = c()
for (i in 1:100000)
{
  x = rnorm(N, mean = mu_x, sd = sd_x)
  y = rnorm(N, mean = mu_y, sd = sd_y)

  t[i] = mean((x-y)^2)
  #t[i] = mean( ((x-y)/(sqrt(sd_x^2 + sd_y^2)))^2 )
}

# simulated sampling distribution
plot(density(t), col=c("red"), lwd = 4)

# normal approximation
mu = (sd_x^2 + mu_x^2) - 2*mu_x*mu_y + (sd_y^2 + mu_y^2)
sd = sqrt(4*(mu_x-mu_y)^2*(sd_x^2 + sd_y^2) / N)
z = rnorm(100000, mean = mu, sd = sd)
lines(density(z), col = c("blue"), lwd = 4)

# chi-squared approximation
ncp = N * ((mu_x-mu_y)^2 / (sd_x^2 + sd_y^2))
s = rchisq(100000, df = N, ncp = ncp)*(sd_x^2 + sd_y^2) / N
lines(density(s), col=c("darkgreen"), lwd = 4)
