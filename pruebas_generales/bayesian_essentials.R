
####################################
## Remarks on Bayesian Essentials ##
####################################


install.packages("bayess")
library(bayess)
library(mnormt)

# Cargar los datos
data(normaldata)
shift = normaldata[,2]

#######################
## Varianza conocida ##
#######################

# Distribución a posteriori de una variable aleatoriamente continua normalmente distribuida
# (el prior es también una distribución normal y la varianza se supone conocida)
# por hipótesis, el error estándar conocido es 0.75
n=length(shift)

# primer parámetro de la distribución a posteriori (media)
mmu=sum(shift)/(n+1)

# segundo parámetro de la distrbución a posteriori (varianza)
vmu=0.75^2/(n+1)

# Trazar la distribución a posteriori
curve(dnorm(x ,mean=mmu,sd=sqrt(vmu)),
      col="steelblue2", lwd=2, add=TRUE, lty=2,
      xlim = c(-1,1))

##########################
## Varianza desconocida ##
##########################



