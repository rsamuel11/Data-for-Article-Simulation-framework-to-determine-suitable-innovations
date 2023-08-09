################N=8000#####################################################

library(rugarch)
attach(BondDataSA)
BondDataSA<-as.data.frame(BondDataSA)
head(BondDataSA)
spec = ugarchspec(variance.model = list(model = "fGARCH",
                                        garchOrder = c(1,1),submodel = "ALLGARCH"),
                  mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                  distribution.model = "std",fixed.pars=list(shape=4.1))

fit = ugarchfit(data = BondDataSA[,4,drop=FALSE], spec = spec)
fit
coef(fit)

# simulate for N=8000

sim = ugarchsim(fit, n.sim=15000, n.start=1, m.sim=1000, rseed = 12345,
                startMethod="sample")

simGARCH <- fitted(sim)
simGARCH

simGARCH <- as.data.frame(simGARCH)
simGARCH

###Remove the first 7000 for initial values effect

R_simGARCH <- simGARCH[-c(1:7000), ]
R_simGARCH

###Fit ARMA(1,1)-fGARCH(1,1) with "norm" to the simulated dataset R_simGARCH#####
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "norm")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "snorm" to the simulated dataset R_simGARCH#####
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "snorm")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "std" to the simulated dataset R_simGARCH######
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "std")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "sstd" to the simulated dataset R_simGARCH######
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "sstd")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "ged" to the simulated dataset R_simGARCH#####
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "ged")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "sged" to the simulated dataset R_simGARCH#####
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "sged")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "ghyp" to the simulated dataset R_simGARCH#####
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "ghyp")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "nig" to the simulated dataset R_simGARCH#####
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "nig")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "ghst" to the simulated dataset R_simGARCH#####
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "ghst")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "jsu" to the simulated dataset R_simGARCH#####
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "jsu")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

################N=9000#####################################################

library(rugarch)
attach(BondDataSA)
BondDataSA<-as.data.frame(BondDataSA)
head(BondDataSA)
spec = ugarchspec(variance.model = list(model = "fGARCH",
                                        garchOrder = c(1,1),submodel = "ALLGARCH"),
                  mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                  distribution.model = "std",fixed.pars=list(shape=4.1))

fit = ugarchfit(data = BondDataSA[,4,drop=FALSE], spec = spec)
fit
coef(fit)

# simulate for N=9000

sim = ugarchsim(fit, n.sim=15000, n.start=1, m.sim=1000, rseed = 12345,
                startMethod="sample")

simGARCH <- fitted(sim)
simGARCH

simGARCH <- as.data.frame(simGARCH)
simGARCH

###Remove the first 6000 for initial values effect

R_simGARCH <- simGARCH[-c(1:6000), ]
R_simGARCH

###Fit ARMA(1,1)-fGARCH(1,1) with "norm" to the simulated dataset R_simGARCH#####
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "norm")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "snorm" to the simulated dataset R_simGARCH#####
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "snorm")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "std" to the simulated dataset R_simGARCH######
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "std")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "sstd" to the simulated dataset R_simGARCH######
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "sstd")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "ged" to the simulated dataset R_simGARCH#####
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "ged")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "sged" to the simulated dataset R_simGARCH#####
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "sged")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "ghyp" to the simulated dataset R_simGARCH#####
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "ghyp")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "nig" to the simulated dataset R_simGARCH#####
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "nig")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "ghst" to the simulated dataset R_simGARCH#####
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "ghst")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "jsu" to the simulated dataset R_simGARCH#####
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "jsu")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

################N=10000#####################################################

library(rugarch)
attach(BondDataSA)
BondDataSA<-as.data.frame(BondDataSA)
head(BondDataSA)
spec = ugarchspec(variance.model = list(model = "fGARCH",
                                        garchOrder = c(1,1),submodel = "ALLGARCH"),
                  mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                  distribution.model = "std",fixed.pars=list(shape=4.1))

fit = ugarchfit(data = BondDataSA[,4,drop=FALSE], spec = spec)
fit
coef(fit)

# simulate for N=10000

sim = ugarchsim(fit, n.sim=15000, n.start=1, m.sim=1000, rseed = 12345,
                startMethod="sample")

simGARCH <- fitted(sim)
simGARCH

simGARCH <- as.data.frame(simGARCH)
simGARCH

###Remove the first 5000 for initial values effect

R_simGARCH <- simGARCH[-c(1:5000), ]
R_simGARCH

###Fit ARMA(1,1)-fGARCH(1,1) with "norm" to the simulated dataset R_simGARCH#####
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "norm")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "snorm" to the simulated dataset R_simGARCH#####
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "snorm")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "std" to the simulated dataset R_simGARCH######
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "std")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "sstd" to the simulated dataset R_simGARCH######
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "sstd")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "ged" to the simulated dataset R_simGARCH#####
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "ged")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "sged" to the simulated dataset R_simGARCH#####
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "sged")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "ghyp" to the simulated dataset R_simGARCH#####
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "ghyp")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "nig" to the simulated dataset R_simGARCH#####
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "nig")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "ghst" to the simulated dataset R_simGARCH#####
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "ghst")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)

###Fit ARMA(1,1)-fGARCH(1,1) with "jsu" to the simulated dataset R_simGARCH#####
spec <- ugarchspec(variance.model = list(model = "fGARCH",
                                         garchOrder = c(1,1),submodel = "ALLGARCH"),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "jsu")
fit = ugarchfit(data = R_simGARCH, spec = spec)
show(fit)
coef(fit)
