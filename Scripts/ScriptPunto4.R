###############################################
#            Problem set 1                    #
#               Punto4                        #
# Colaboradores:                              #
# Daniel Raquira 201914059                    #
# Santiago Becerra 201911587                  #
#                                             #
###############################################

set.seed(123456)
geih_limpia2 <- geih_limpia2 %/% mutate( geih_limpia2 , holdout= as.logical (1:nrow(geih_limpia2)%in% 
                                        sample(nrow(geih_limpia2), nrow(geih_limpia2)*.3)))
  
test <- geih_limpia2[geih_limpia2$holdout==T,]
train <- geih_limpia2[geih_limpia2$holdout==F,]

##no se logram el codigo antes del tiempo de entrega del taller