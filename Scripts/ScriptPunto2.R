###############################################
#            Problem set 1                    #
#               Punto2                        #
# Colaboradores:                              #
# Daniel Raquira 201914059                    #
# Santiago Becerra 201911587                  #
#                                             #
###############################################

##Organizacion de paquetes
install.packages("stargazer")
install.packages("pacman")
install.packages("tidyverse")
require(tidyverse)
require(stargazer)
require(pacman)

## Escogimos para earnings la variable ingtot
summary(geih_limpia2$ingtot)
## Modelo age earning
geih_limpia2<- geih_limpia2 %>% mutate(age2 = age^2)
mod_age_earning<-lm(ingtot~age+age2,geih_limpia2) ##Modelo de regresión 1
summary(mod_age_earning) 
