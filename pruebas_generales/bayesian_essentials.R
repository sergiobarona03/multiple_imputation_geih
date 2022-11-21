
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

# Distribuci�n a posteriori de una variable aleatoriamente continua normalmente distribuida
# (el prior es tambi�n una distribuci�n normal y la varianza se supone conocida)
# por hip�tesis, el error est�ndar conocido es 0.75
n=length(shift)

# primer par�metro de la distribuci�n a posteriori (media)
mmu=sum(shift)/(n+1)

# segundo par�metro de la distrbuci�n a posteriori (varianza)
vmu=0.75^2/(n+1)

# Trazar la distribuci�n a posteriori
curve(dnorm(x ,mean=mmu,sd=sqrt(vmu)),
      col="steelblue2", lwd=2, add=TRUE, lty=2,
      xlim = c(-1,1))

##########################
## Varianza desconocida ##
##########################


