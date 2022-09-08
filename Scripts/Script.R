###############################################
#            Problem set 1                    #
#                                             #
# Colaboradores:                              #
# Daniel Raquira 201914059                    #
# Santiago Becerra 201911587                  #
#                                             #
###############################################

## 1 ##
#a.i.
rm(list=ls())
require(pacman)
p_load(tidyverse, rvest)

url<-0
tablaurl<-0

for (i in 1:10) {
  print(paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", i ,".html"))
  url[i]<-paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", i ,".html")
  tablaurl[i]<-read_html(url[i]) %>% html_table
}

#Creamos dataframes para pasar las tablas y luego pegarlas
geih1<-data.frame()
geih1<-rbind(geih1,tablaurl[1])
geih2<-data.frame()
geih2<-rbind(geih2,tablaurl[2])
geih3<-data.frame()
geih3<-rbind(geih3,tablaurl[3])
geih4<-data.frame()
geih4<-rbind(geih4,tablaurl[4])
geih5<-data.frame()
geih5<-rbind(geih5,tablaurl[5])
geih6<-data.frame()
geih6<-rbind(geih6,tablaurl[6])
geih7<-data.frame()
geih7<-rbind(geih7,tablaurl[7])
geih8<-data.frame()
geih8<-rbind(geih8,tablaurl[8])
geih9<-data.frame()
geih9<-rbind(geih9,tablaurl[9])
geih10<-data.frame()
geih10<-rbind(geih10,tablaurl[10])

#dropeamos la columna de identificación (cambia el nombre de esta columna según los data.frames)
geih1$X1.3217 <- NULL
geih2$X1.3218 <- NULL
geih3$X1.3218 <- NULL
geih4$X1.3218 <- NULL
geih5$X1.3218 <- NULL
geih6$X1.3218 <- NULL
geih7$X1.3218 <- NULL
geih8$X1.3218 <- NULL
geih9$X1.3217 <- NULL
geih10$X1.3217 <- NULL

#pegamos los dataframes
geih<-rbind(geih1,geih2,geih3,geih4,geih5,geih6,geih7,geih8,geih9,geih10)

#a.ii.
#volvemos ocu factor
geih$ocu<-factor(geih$ocu)
#dejamos mayores de 18 años y ocupados
geih_limpia<-geih[which(geih$age>=18 & geih$ocu==1),]
#Convertimos en factor las variables categoricas según el diccionario
geih_limpia<-geih_limpia %>% mutate_at(.vars = c("cclasnr11", "cclasnr2", "cclasnr3", "cclasnr4", "cclasnr5",
                       "cclasnr6", "cclasnr7", "cclasnr8", "clase", "college",
                       "cotPension", "cuentaPropia", "depto", "directorio", "dominio",
                       "dsi", "estrato1", "formal", "ina", "inac", "informal",
                       "maxEducLevel","mes", "microEmpresa", "ocu", "oficio", 
                       "orden","p6050", "p6090", "p6100", "p6210", "p6210s1", "p6240", "p6510",
                       "p6510s2", "p6545", "p6545s2", "p6580", "p6580s2", "p6585s1",
                       "p6585s1a2","p6580","p6580s2","p6585s1","p6585s1a2", "p6585s2", "p6585s2a2",
                       "p6585s3","p6585s3a2", "p6585s4", "p6585s4a2",
                       "p6590","p6600", "p6610", "p6620", "p6630s1", "p6630s2", "p6630s3",
                       "p6630s4", "p6630s6", "p6920", "p7040", "p7050", "p7090",
                       "p7110", "p7120", "p7140s1", "p7140s2", "p7150","p7160",
                       "p7310", "p7350", "p7422", "p7472", "p7495", "p7500s1",
                       "p7500s2", "p7500s3", "p7505", "p7510s1", "p7510s2",
                       "p7510s3", "p7510s5", "p7510s6", "p7510s7", "pea", "pet", 
                       "regSalud", "relab", "secuencia_p", "sex", "sizeFirm", "wap"), .funs = factor)
#Verificamos si hay NA en nuestra variable de interes
sum(is.na(geih_limpia$ingtot))
#No hay NA en nuestra variable dependiente

#Dejamos variables de interés eun un nuevo dataframe
geih_limpia2<-as.data.frame(cbind(geih_limpia$directorio,geih_limpia$ingtot,geih_limpia$sex,geih_limpia$age,geih_limpia$clase,geih_limpia$college,geih_limpia$cuentaPropia,geih_limpia$depto,geih_limpia$formal,geih_limpia$maxEducLevel,geih_limpia$oficio,geih_limpia$relab,geih_limpia$totalHoursWorked))
geih_limpia2<-`colnames<-`(geih_limpia2, c("directorio","ingtot","sex","age","clase","college","cuentaPropia","depto","formal","maxEducLevel","oficio","relab","totalHoursWorked"))

#exportamos las estadisticas descriptivas
resumen<-summary.data.frame(object = geih_limpia2)
print(resumen)
write.table(resumen, file = "tabla descriptiva.txt", sep = ",", quote = FALSE, row.names = F)

p_load(ggplot2)

ggplot() %>% 