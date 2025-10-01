# EFECTOS DE LA EDUCACIÓN DE LA MUJERES SOBRE LA FERTILIDAD!

# Variables instrumentales
# Las mujeres mÁs educadas tienen menos hijos?
# Numerosos estudios indican que la educación de las mujeres 
# tiene un efecto negativo sobre la fertilidad. 
# Varias son las posibles explicaciones: la escolarización aumenta el 
# coste de oportunidad de tener un hijo, aumenta la eficiencia del 
# control fertilidad o simplemente reduce la preferencia por los hijos.
# En este ejercicio vamos a estudiar este posible efecto negativo

# El siguiente ejercicio se basa en el paper: McCrary, J and Royer, H.
# (2011). "The Effect of Female Education on Fertility and Infant 
# Health: Evidence from School Entry Policies Using Exact Date of Birth". 
# American Economic Review, 101: 158-195.

# Cargamos paquetes que vamos a necesitar
#install.packages('devtools')
#devtools::install_github("beniaminogreen/cragg")
#install.packages('AER')
#install.packages('stargazer')
#install.packages("stargazer")
library(haven); library(dplyr); library(cragg); library(AER)
library(tidyverse)
library(stargazer); library(summarytools)

# Leyendo los datos
setwd(".../IV")

data <- read_dta("GSS2012_2018.DTA") |> 
  dplyr::select(year, age, sex, race, educ, childs, paeduc, maeduc, wrkstat, marital) |> 
  mutate_at(c('age','sex','race','educ','childs', 
              'paeduc','maeduc','wrkstat','marital'), as.numeric) |> 
  filter(sex==2, year>=2014 & year<=2018, age>=35 & age<=55) |> 
  mutate(age2 = age*age, afroa = case_when(race == 1 ~ 1,
                                           race == 2 ~ 0),
         working = case_when(wrkstat >= 1 & wrkstat<= 2 ~ 1,
                             wrkstat >= 3 & wrkstat<= 8 ~ 0),
         casado = case_when(marital == 1 ~ 1,
                            marital != 1 ~ 0)) |>  drop_na()

View(data[,c("race","afroa")])
View(data[,c("wrkstat","working")])
View(data[,c("marital","casado")])








# OLS
ols <- lm(childs ~ educ+age+I(age2)+casado+afroa+working, data=data)
summary(ols)

# IV: se instrumenta la educacion de la mujeres con la educacion de los padres
iv <- ivreg(childs ~ educ+age+I(age2)+casado+afroa+working |
              age+I(age2)+casado+afroa+working+paeduc+maeduc, data=data)

summary(iv)

fstage <- lm(educ ~ age+I(age2)+casado+afroa+working+paeduc+maeduc, data=data)
summary(fstage)
data <- data |> 
  mutate(educpred=predict(fstage))
sstage <- lm(childs ~ educpred+age+I(age2)+casado+afroa+working, data=data)
summary(sstage)

# Diagnósticos de los instrumentos
# Relevancia de los instrumentos
# La idea es probar la significancia conjunta de los instrumentos en la primera etapa
# Se calcula entonces el F estadistico de la primera etapa
# Existen dos formas
fstage <- lm(educ ~ age+I(age2)+casado+afroa+working+paeduc+maeduc, data=data)
summary(fstage)


linearHypothesis(fstage, 
                 c("paeduc = 0", "maeduc = 0"))

summary(iv, diagnostics=TRUE)
# Weak instruments: La H0 es que los instrumentos son débiles, asi que un rechazo
# significa que los instrumento no son debiles, lo cual es bueno

# El F es bastante grande con los cual rehazamos H0 que los instrumentos no tienen 
# efecto sobre la educacion, con lo cual son relevantes

# Wu-Hausman: Es un test de endogeneidad, donde H0: Cov(educ, error) = 0. 
# Rechazando H0
# indicate la existencia de endogeniedad la necesidad por variables instrumentales
# En otras palabras, prueba la consistencia de las estimaciones OLS bajo el 
# supuesto que el IV es consistente. Cuando se rechaza H0, indica que OLS es 
# no cosistente, sugiriendo que la endogeneidad es presente. Si no se rechaza H0, 
# significa que OLS y IV son similares y la endogeneidad no es un problema

# Sargan: sobreidentificacion - sirve para probar la validez de los instrumentos 
# (los instrumentos no estan correlacionados con los errores). Este test solo 
#  puede calcularse si los instrumentos exceden el numero de variables endogenas. 
#  Este test tambien es llamado test de restricciones de sobre-identificacion. 
#  H0: Cov(z,error)=0. Lo bueno seria no rechazar


# Tabla de resultados
stargazer(ols, iv,
          header = FALSE, 
          type = "text",
          covariate.labels = c("Educación", "Edad", "Edad2", "Casado (=1)", 
                               "afroamericano (=1)", "Trabajando (=1)", "Constante"),
          digits = 4, 
          out.header = T,
          model.names = F,
          column.labels = c("OLS", "IV"),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Y: número de hijos",
          omit.stat = c("f", "ser"))
