###############################################
#            Problem set 1                    #
#               Punto3                        #
# Colaboradores:                              #
# Daniel Raquira 201914059                    #
# Santiago Becerra 201911587                  #
#                                             #
###############################################

#a)
#Hacemos el modelo
geih_limpia3<- geih_limpia2 %>% mutate(mujer = ifelse(geih_limpia2$sex == 1, 1, 0))
geih_limpia3<- subset(geih_limpia3, ingtot!=0) 
geih_limpia3<-geih_limpia3 %>% mutate(log_ingtot=log(geih_limpia3$ingtot))

mod_mujer_earning<-lm(log_ingtot~mujer, geih_limpia3)
stargazer(mod_mujer_earning, type = "text", title = "Regresión log-Earnings Mujer", out="mod_logearnings-mujer.txt")

#b)
geih_limpia3<- geih_limpia3 %>% mutate(age_mujer = age*mujer)
mod_completo<- lm(log_ingtot~age+age2+mujer+age_mujer, geih_limpia3)
stargazer(mod_completo, type = "text", title = "Regresión log-Earnings Edad Mujer", out = "mod_completo.txt")

ggplot(data = geih_limpia3, aes(x = age, y = predict(mod_completo))) +
  geom_point(aes(shape = factor(mujer))) +
  geom_point(aes(color = factor(mujer)))+
  geom_smooth(method = lm(log_ingtot~age+age2+mujer+age_mujer,geih_limpia3), 
              se = FALSE, 
              aes(color = factor(mujer))) +
  labs(title = "Regresión Log-ingreso vs Edad (por genero)",
       x = "Edad",
       y = "Log ingreso")
coefs_completo<-mod_completo$coefficients
coefs_completo
b_1<-coefs_completo[2]
b_2<-coefs_completo[3]
b_3<-coefs_completo[4]
b_4<-coefs_completo[5]

#Hallamos los peak derivando respecto a age e igualando a 0. B1+2B2Age+B4=0
Peak_mujer = (-b_1 - b_4)/(2*b_2) 
Peak_mujer #La edad óptima de las mujeres es 39.69
Peak_hombre = (-b_1)/(2*b_2)
Peak_hombre #La edad óptima de los hombres es 48.2419

#bootstrap para luego construir intervalos de confianza para mujeres

media_edad_mujer<-mean(geih_limpia3$age[geih_limpia3$mujer==1])
mod_completo_mujeres.fn<-function(data,index,
                      media_edad_mujer=mean(geih_limpia3$age [geih_limpia3$mujer==1]), 
                      media_edad2_mujer=mean(geih_limpia3$age2 [geih_limpia3$mujer==1])){
  mod_completo<-lm(log_ingtot~age+age2+mujer+age_mujer, geih_limpia3, subset = index)
  coefs_completo_mujer<-mod_completo$coefficients
  B1<-coefs_completo_mujer[2]
  B2<-coefs_completo_mujer[3]
  B4<-coefs_completo_mujer[5]
  e_edad_mujer<-B1+2*B2*media_edad_mujer+B4
  return(e_edad_mujer)
}
resultados_mujer<-boot(data=geih_limpia3, mod_completo_mujeres.fn,R=1000)
resultados_mujer

boot.ci(boot.out = resultados_mujer, conf = 0.95, type = "norm", index = 1:min(2,length(resultados_mujer$t0)))

#bootstrap para luego construir intervalos de confianza para hombres

media_edad_hombre<-mean(geih_limpia3$age[geih_limpia3$mujer==0])
mod_completo_hombre.fn<-function(data,index,
                                  media_edad_hombre=mean(geih_limpia3$age [geih_limpia3$mujer==0]), 
                                  media_edad2_hombre=mean(geih_limpia3$age2 [geih_limpia3$mujer==0])){
  mod_completo<-lm(log_ingtot~age+age2+mujer+age_mujer, geih_limpia3, subset = index)
  coefs_completo_hombre<-mod_completo$coefficients
  B1<-coefs_completo_hombre[2]
  B2<-coefs_completo_hombre[3]
  B4<-coefs_completo_hombre[5]
  e_edad_hombre<-B1+2*B2*media_edad_hombre+B4
  return(e_edad_hombre)
}
resultados_hombre<-boot(data=geih_limpia3, mod_completo_hombre.fn,R=1000)
resultados_hombre

boot.ci(boot.out = resultados_hombre, conf = 0.95, type = "norm", index = 1:min(2,length(resultados_hombre$t0)))

#c
mod_final_c<-lm(log_ingtot~mujer+oficio, geih_limpia3)
stargazer(mod_final_c, type = "text", title = "Modelo Log-Ingresos Mujer Oficios", out = "mod_final_c.txt" )

#lo gacenos por FWL
geih_limpia4<-geih_limpia3 %>% mutate(r1=lm(log_ingtot~oficio,geih_limpia3)$residuals, #Residuals of logwage~ability (we are "purging" ability)
                  r2=lm(mujer~oficio,geih_limpia3)$residuals, #Residuals of schooling~ability (we are "purging" ability)
)



FWL<- lm(r1~-1 +r2, geih_limpia4)
stargazer(mod_final_c, FWL, type = "text", title = "Comparación Long y FWL", out = "comparacion.txt")

#FWL con bootstrap

FWL.fn<-function(data,index,
                       media_r2=mean(r2)){
  FWL<-lm(r1~-1 + r2, geih_limpia4, subset = index)
  coefs_punto_c<-FWL$coefficients
  B1<-coefs_punto_c[2]
  e<-B1
  return(e)
}
resultados_FWL<-boot(data=geih_limpia4, FWL.fn,R=1000)
resultados_FWL

boot.ci(boot.out = resultados_FWL, conf = 0.95, type = "norm", index = 1:min(2,length(resultados_FWL$t0)))
