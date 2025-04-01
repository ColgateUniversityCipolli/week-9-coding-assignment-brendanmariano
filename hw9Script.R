library(tidyverse)
################################################################################
# HW 9
################################################################################

###################
# Data Cleaning
###################
#Continuous data
dat = read_csv("agacis.csv") |>
  dplyr::select(-Annual) |> #What does dplyr do as opposed to the regular function?
  pivot_longer(cols = c("Jan", "Feb", "Mar",
               "Apr", "May", "Jun",
               "Jul", "Aug", "Sep",
               "Oct", "Nov", "Dec"),
               names_to = "Month",
               values_to = "Precipitation") |>
  mutate(Precipitation = case_when(Precipitation == "M" ~ NA_character_,
            TRUE ~ Precipitation))  |>
  mutate(Precipitation = as.numeric(Precipitation))
view(dat)
############################
# Calculating Distributions
############################
#Gamma function
mle.gamma = function(data, par, neg = F) {
  alpha = par[1]
  beta =par[2]
  
  dist = dgamma(x = data, shape = alpha, rate = beta)
  log.lik = sum(log(dist), na.rm = T)
  
  return(ifelse(neg, -log.lik, log.lik))
}
vals = optim(fn = mle.gamma, 
      data = dat$Precipitation, 
      par = c(1,1),
      neg = T)
(alpha = vals$par[1])
(beta = vals$par[2])
# Log norm function
mle.lognorm = function(data, par, neg = F){
  mu = par[1]
  sigma = par[2]
  
  log.lik = sum(log(dlnorm(x = data, meanlog = mu, sdlog = sigma)), na.rm = T)
  
  return(ifelse(neg, -log.lik, log.lik))
}
lnorm.vals = optim(fn = mle.lognorm, 
                          data = dat$Precipitation, 
                          par = c(1,1),
                          neg = T)
(mu = lnorm.vals$par[1])
(sigma = lnorm.vals$par[2])
# Computing likelihood ratios

#Weibull and Gamma
#Wrong
weibull.lik = -2166.496
(gamma.lik = sum(log(dgamma(x = dat$Precipitation, shape = alpha, rate = beta)), na.rm = T))
(likelihood.ratio = exp(weibull.lik - gamma.lik))

#Weibull and Log Normal
(lnorm.lik = sum(log(dlnorm(x = dat$Precipitation, meanlog = mu, sdlog = sigma)), na.rm = T))
(likelihood.ratio2 = exp(weibull.lik - lnorm.lik))
#Gamma and Log Normal
(likelihood.ratio3 = exp(gamma.lik - lnorm.lik))

################################################################################
#Coding challenge: Need to finish: Recall how to superimpose several lines
################################################################################
#Winter
winter.dat = dat |>
  filter(Month %in% c("Dec","Jan", "Feb")) |>
  filter(!is.na(Precipitation))
view(winter.dat)
vals.w = optim(fn = mle.gamma, 
             data = dat$Precipitation, 
             par = c(1,1),
             neg = T)
(alpha.w = vals.w$par[1])
(beta.w = vals.w$par[2])
win.dist = dgamma(x = winter.dat$Precipitation, shape = alpha.w, rate = beta.w)
#Spring
spring.dat = dat |>
  filter(Month %in% c("Mar","Apr", "May")) |>
  filter(!is.na(Precipitation))

vals.spr = optim(fn = mle.gamma, 
               data = dat$Precipitation, 
               par = c(1,1),
               neg = T)
(alpha.spr = vals.spr$par[1])
(beta.spr = vals.spr$par[2])
spr.dist = dgamma(x = spring.dat$Precipitation, shape = alpha.spr, rate = beta.spr)
#Summer
summer.dat = dat |>
  filter(Month %in% c("Jun","Jul", "Aug"))
view(summer.dat)
vals.summ = optim(fn = mle.gamma, 
                 data = dat$Precipitation, 
                 par = c(1,1),
                 neg = T)
(alpha.summ = vals.summ$par[1])
(beta.summ = vals.summ$par[2])
summ.dist = dgamma(x = summer.dat$Precipitation, shape = alpha.summ, rate = beta.summ)
#Fall
fall.dat = dat |>
  filter(Month %in% c("Sep","Oct", "Nov"))
vals.fall = optim(fn = mle.gamma, 
                  data = dat$Precipitation, 
                  par = c(1,1),
                  neg = T)
(alpha.fall = vals.summ$par[1])
(beta.fall = vals.summ$par[2])
fall.dist = dgamma(x = fall.dat$Precipitation, shape = alpha.fall, rate = beta.fall)

plots = ggplot() + 
  geom_line(aes(x = winter.dat$Precipitation, y = win.dist), color = "cyan3") +
  geom_line(aes(x = spring.dat$Precipitation, y = spr.dist), color = "chartreuse3") +
  geom_line(aes(x = summer.dat$Precipitation, y = summ.dist), color = "red3") +
  geom_line(aes(x = fall.dat$Precipitation, y = fall.dist), color = "chocolate3") 
  
plots