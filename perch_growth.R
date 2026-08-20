# Bayesian MCMC to model slopes/intercepts of change-point LAA regression
# against temperature in yellow perch

# required packages
library(tidyverse)
library(R2jags)
library(runjags)
library(tidybayes)
library(modelr)
library(tidyr)
library(broom)
library(brms)
library(broom.mixed)
library(ggplot2)
library(ggdist)
library(ggpubr)
library(ggeffects)



# load in LAA data
laa_temp_data = read.csv("laa_nutrient.csv")
laa_temp_data = laa_temp_data[laa_temp_data$length_mm < 450,]


# create a year index to eventually use within JAGS model
year_index <- 
  laa_temp_data |>
  distinct(year, nutrition) |> # choice of environmental covariate: nutrition or ice_on
  arrange(year) |>
  mutate(year.index = 1:n())


t.hist = matrix(nrow = max(year_index$year.index), ncol = max(laa_temp_data$age)+1)

for(i in 1:length(year_index$year.index)){
  
  t.hist[i,1] = year_index[i,2]
  
  
  for(j in 1:max(laa_temp_data$age)){
    
    if(i <= j){
      
      t.hist[i,j+1] = mean(year_index[1:i,2], na.rm = T)
      
    }else
      
      t.hist[i,j+1] = mean(year_index[i:(i-j),2], na.rm = T)
    
  }  
}


# add in individual specific temp histories
laa_temp_data$t.hist = NA

laa_temp_data = laa_temp_data |>
  left_join(year_index, by = c("year", "nutrition"))


for(i in 1:length(laa_temp_data$year)){
  laa_temp_data$t.hist[i] = t.hist[laa_temp_data[i,"year.index"], laa_temp_data[i,"age"]+1]
}


#################
# scale temp and size data
laa_temp_data$t.hist = scale(laa_temp_data$t.hist)
laa_temp_data$length = laa_temp_data$length_mm/10
laa_temp_data$age = laa_temp_data$age + 0.5


# create a year index to eventually use within JAGS model
age_year_index <- 
  laa_temp_data |>
  distinct(age, year, t.hist, MU) |>
  arrange(age, year, t.hist, MU) |>
  mutate(age.year.index = 1:n()) |>
  left_join(year_index, by = c("year"))


# set up jags data
mod_data <- 
  laa_temp_data |>
  left_join(age_year_index, by = c("age", "year", "t.hist", "year.index", "nutrition", "MU")) |>
  arrange(age.year.index)


mod_laa_data <- 
  mod_data |>
  select(age.year.index, length, age, MU)


mod_temp_data <- 
  mod_data |>
  select(age.year.index, t.hist, MU)





# JAGS data
mod_data_list <- list(
  laa_data = as.matrix(mod_laa_data),
  temp_data = as.matrix(mod_temp_data),
  age_year_index = as.matrix(age_year_index),
  nageyears = length(unique(age_year_index$age.year.index)),
  nobs = nrow(mod_laa_data),
  minage = min(mod_data$age),
  maxage = max(mod_data$age)
)



jags1 <- "model {
  

  pred_alpha <- a1
  pred_cp    <- a4

for (j in 1:nageyears) {
  
  pred_beta1[j] <- a2 + (b1[age_year_index[j, 4]] * age_year_index[j, 3]) 
  pred_beta2[j] <- a3 + (b2[age_year_index[j, 4]] * age_year_index[j, 3])  
  
  alpha[j] ~ dnorm(pred_alpha, 1 / alpha_sig)T(0,10000)
  beta1[j] ~ dnorm(pred_beta1[j], 1 / beta1_sig)T(0, 10000)
  beta2[j] ~ dnorm(pred_beta2[j], 1 / beta2_sig)T(0, beta1[j])
  cp[j] ~ dnorm(pred_cp, 1 / cp_sig)T(minage, maxage)

  

}


# likelihoods

for (i in 1:nobs) {

  mu[i] <- alpha[laa_data[i,1]] + 
    (beta1[laa_data[i,1]] * min(laa_data[i,3], cp[laa_data[i,1]])) + 
    (beta2[laa_data[i,1]] * max(0,laa_data[i,3] - cp[laa_data[i,1]]))
  laa_data[i,2] ~ dnorm(mu[i], 1/nu_laa)
}


# priors on regression coefficients

      a1 ~ dunif(0, 10)
      a2 ~ dunif(0, 10)
      a3 ~ dunif(0, 10)
      a4 ~ dunif(0, 5)

# priors on temperature coefficients
for(m in 1:4){

      b1[m] ~ dnorm(0, 1)
      b2[m] ~ dnorm(0, 1)

}




# priors on process error
alpha_sig ~ dgamma(1, 1)
beta1_sig ~ dgamma(1, 1)
beta2_sig ~ dgamma(1, 1)
cp_sig ~ dgamma(1, 1)
nu_laa ~ dgamma(1, 1)


}"

writeLines(jags1, con="M1.txt")


jags_params <- c(
  paste0("a", 1:4), paste0("b", 1:2),
  "alpha_sig", "beta1_sig", "beta2_sig", "cp_sig", "nu_laa")

inits = function(){list(a1 = runif(1,5,6), a2 = runif(1,5,6), a3 = runif(1,1,2), a4 = runif(1,2,3),
                        b1 = runif(4,0,1), b2 = runif(4,-1,0), 
                        alpha_sig = runif(1, 0.01, 0.1), 
                        beta1_sig = runif(1, 0.01, 0.1), 
                        beta2_sig = runif(1, 0.01, 0.1), 
                        cp_sig = runif(1, 0.01, 0.1),
                        nu_laa = runif(1, 0.01, 0.1))}


m <- jags.parallel(
  data = mod_data_list, 
  inits = inits,
  parameters.to.save = jags_params,
  model.file = "M1.txt",
  n.chains = 3,
  n.iter = 150000,
  n.burnin = 100000,
  n.thin = 100
)


traceplot(m)
save(m, file = "perch_JAGS_nutrition.RData")


#####################
# posterior summaries
#####################

# fixed effects

# posterior draws
mcmc = m$BUGSoutput$sims.list

# scaled temperature range
ts = seq(-2, 2, length.out = 100)

#
# temperature on juvenile growth
#

## Calculate the fitted values
newdata = data.frame(x = ts)
Xmat = model.matrix(~x, newdata)
coefs = cbind(rowMeans(mcmc[["a2"]]), mcmc[["b1"]])
fit = (coefs %*% t(Xmat))
newdata = newdata %>% cbind(tidyMCMC(fit, conf.int = TRUE, conf.method = "quantile"), ts)

g1 = ggplot(newdata, aes(x = ts, y = estimate))+
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3) +
  xlab("Hypoxic Extent") + 
  ggtitle("Juvenile Growth Rate") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(-1.369, -0.251, 0.866, 1.983), labels = c("2", "4", "6", "8"))


#
# temperature on adult growth
#


## Calculate the fitted values
newdata = data.frame(x = ts)
Xmat = model.matrix(~x, newdata)
coefs = cbind(rowMeans(mcmc[["a3"]]), mcmc[["b2"]])
fit = (coefs %*% t(Xmat))
newdata = newdata %>% cbind(tidyMCMC(fit, conf.int = TRUE, conf.method = "quantile"), ts)

g2 = ggplot(newdata, aes(x = ts, y = estimate))+
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3) +
  xlab("Hypoxic Extent") + 
  ggtitle("Adult Growth Rate") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(-1.369, -0.251, 0.866, 1.983), labels = c("2", "4", "6", "8"))


#
# temperature on age at maturity
#

## Calculate the fitted values
newdata = data.frame(x = ts)
Xmat = model.matrix(~x, newdata)
coefs = cbind(rowMeans(mcmc[["a4"]]), mcmc[["b3"]])
fit = (coefs %*% t(Xmat))
newdata = newdata %>% cbind(tidyMCMC(fit, conf.int = TRUE, conf.method = "quantile"), ts)

g3 = ggplot(newdata, aes(x = ts, y = estimate))+
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3) +
  xlab("Hypoxic Extent") + 
  ggtitle("Age at Maturity") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(-1.369, -0.251, 0.866, 1.983), labels = c("2", "4", "6", "8"))


#
# temperature on size at maturity
#

## Calculate the fitted values
fit0 = rep(rowMeans(mcmc[["a1"]]), length(ts))

newdata = data.frame(x = ts)
Xmat = model.matrix(~x, newdata)
coefs = cbind(rowMeans(mcmc[["a4"]]), mcmc[["b3"]])
fit1 = (coefs %*% t(Xmat))

newdata = data.frame(x = ts)
Xmat = model.matrix(~x, newdata)
coefs = cbind(rowMeans(mcmc[["a2"]]), mcmc[["b1"]])
fit2 = (coefs %*% t(Xmat))

fit = fit0 + (fit1 * fit2) 

newdata = newdata %>% cbind(tidyMCMC(fit, conf.int = TRUE, conf.method = "quantile"), ts)


g4 = ggplot(newdata, aes(x = ts, y = estimate))+
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3) +
  xlab("Hypoxic Extent") + 
  ggtitle("Size at Maturity") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(-1.369, -0.251, 0.866, 1.983), labels = c("2", "4", "6", "8"))

gg1 = ggarrange(g1, g2, g3, g4, nrow = 2, ncol = 2)

ggsave("perch_fe_hypoxia.png", gg1, width = 5, height = 5, units = "in", dpi = 600)



# growth curves (assuming same max age)

p1 = m$BUGSoutput$mean

# scaled hypoxia range
ts = c(0.679, -0.408, -1.495)

png("yp_growth_curves_hypoxia.png", width = 4, height = 4, units = "in", res = 600)

par(cex.main = 1.5, mar = c(3.5, 4, 3.5, 0) + 0.1, mgp = c(2.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)

pred.alpha = p1$a1
pred.beta1 = p1$a2+(p1$b1*ts)
pred.beta2 = p1$a3+(p1$b2*ts)
pred.cp = p1$a4+(p1$b3*ts)

pred.length.cold = data.frame(age = seq(from = 0, to = 15, length.out = 100))
pred.length.mean = data.frame(age = seq(from = 0, to = 15, length.out = 100))
pred.length.warm = data.frame(age = seq(from = 0, to = 15, length.out = 100))

for(i in 1:length(pred.length.mean[,1])){
  
  pred.length.cold[i,2] = log(
    pred.alpha + (pred.beta1[1] * pmin(pred.length.cold[i,1], pred.cp[1])) + 
      (pred.beta2[1] * pmax(0, (pred.length.cold[i,1] - pred.cp[1])))
  )    
  
  pred.length.mean[i,2] = log(
    pred.alpha + (pred.beta1[2] * pmin(pred.length.mean[i,1], pred.cp[2])) + 
      (pred.beta2[2] * pmax(0, (pred.length.mean[i,1] - pred.cp[2])))
  )  
  
  pred.length.warm[i,2] = log(
    pred.alpha + (pred.beta1[3] * pmin(pred.length.warm[i,1], pred.cp[3])) + 
      (pred.beta2[3] * pmax(0, (pred.length.warm[i,1] - pred.cp[3])))
  )    
  
  
}


plot(pred.length.mean$age, exp(pred.length.mean[,2]), type = "l", lwd = 2, 
     main = "Lake Erie Yellow Perch", xlab = "Age", ylab = "Length",
     xlim = c(0, 16),
     ylim = c(0, max(c(exp(pred.length.cold[,2]), exp(pred.length.mean[,2]), exp(pred.length.warm[,2]))))+2)
lines(pred.length.cold$age, exp(pred.length.cold[,2]), lwd = 2, col = "blue")
lines(pred.length.warm$age, exp(pred.length.warm[,2]), lwd = 2, col = "red")


dev.off()



load("perch_JAGS_nutrition.RData")

df1 = data.frame(value = m$BUGSoutput$sims.list$nu_laa)

load("perch_JAGS_ice.RData")

df2 = data.frame(value = m$BUGSoutput$sims.list$nu_laa)

df = rbind(df1, df2)
df$group = rep(c("Nutrition", "Ice-on duration"), each = 1500)

g1 = df %>%
  ggplot(aes(x = sqrt(value), y = group)) +
  stat_halfeye() + 
  xlab("Process Error") +
  theme_classic() + ylab("")

ggsave("process_error.pdf", g1, width = 4, height = 4, units = "in", dpi = 600)
