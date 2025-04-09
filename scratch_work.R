#Hiddink et al., 2017

# If we assume that the recovery of biomass or numbers (hereafter abundance) of biota
# B after trawling is described by the logistic growth equation, then the equilibrium
# solution can be used to estimate B as a fraction of carrying capacity K in an
# environment subject to chronic fishing disturbance (28):

# b is biomass
# k is carrying capacity
# f is frequency of disturbance
# d is depletion rate (proportion biomass removed)
# r is recovery rate

biomass_est <- function(f, d, r){
  b_over_k = (1 - f * (d/r))
  return(b_over_k)
}

#testing
biomass_est(f = 1, d = 0.9, r = 0.75)

#Hiddink et al., produces a negative result and I'm not sure why!!!

#Shaefer Model

#from https://haddonm.github.io/URMQMF/surplus-production-models.html

#bt = biomass at time t -
#rbt = growth rate in biomass at time t - list assumptions, need data
#k = carrying capacity - initial condition is carrying capacity
#ct = catch at time t

biomass_shaf <- function(bt, r, k, ct){
  bt_1 = bt + r * bt * (1 - (bt / k)) - ct
  return(bt_1)
}

#testing
biomass_shaf(bt = 1000, r = .75, k = 1000, ct = 1000)

#testing in a loop

#set initial bt (same as k in this scenario)
bt = 1000

#output list to store biomass per year
output_list <- list()

#100 yr loop
for(i in 1:100) {

  #every 20 years almost everything dies
  if(i %% 20 == 0) {ct = 999} else {ct = 0}

  #run biomass function every year
  bt = biomass_shaf(bt = bt, r = 0.75, k = 1000, ct = ct)

  #store biomass at time t
  output_list[[i]] <- bt
}

#x is years, y is bt at time t
plot(x = 1:100, y = output_list, type = "line")
