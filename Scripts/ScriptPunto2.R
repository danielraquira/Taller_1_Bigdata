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
stargazer(mod_age_earning,type="text",title="Regresión Earning Age", out="mod_age-earning.txt")
res <- resid(mod_age_earning)

## Sacamos el plot de ingreso predicho vs age
plot(x=geih_limpia2$age, y=predict(mod_age_earning),
     xlab='Age',
     ylab='ingresos predichos',
     main='Ingreso predicho vs Age')
lines(sort(geih_limpia2$age),             
      fitted(mod_age_earning)[order(geih_limpia2$age)],
      col = "red",
      type = "l")

## Sacar el punto maximo de age respecto a los ingresos predichos

coeficientes<-mod_age_earning$coef
coeficientes
b1<-coeficientes[2]
b2<-coeficientes[3]

Peak=(-b1)/(2*b2) ##Derivada para maximizar
Peak 

##Intervalos de confianza luego del boot
media_age<-mean(geih_limpia2$age) 
e_ingreso_edad<-b1+2*b2*media_age 
e_ingreso_edad 

e_mod_age_earning.fn<-function(data,directorio,
                            mean_age=mean(geih_limpia2$age), 
                            mean_age2=mean(geih_limpia2$age2)){
  mod2_age_earning<-lm(ingtot~age+age2, geih_limpia2, subset = directorio)
  coeficientes<-mod2_age_earning$coefficients
  B1<-coeficientes[2]
  B2<-coeficientes[3]
  elasticidad <- B1+2*B2*mean_age
}

require(boot)
resultados <- boot(data=geih_limpia2, e_mod_age_earning.fn,R=1000)
resultados
elasticidad

desviación_estandar <- 1449.747*1.96
IDC1_lowerbound<-e_ingreso_edad - desviación_estandar
IDC1_upperbound<-e_ingreso_edad + desviación_estandar
print(c(IDC1_lowerbound,IDC1_upperbound))
