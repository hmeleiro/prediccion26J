---
title: "Predicción electoral del 26J"
subtitle: "Asignatura: Diseño de encuestas"
author: "Julio Blázquez, Héctor Meleiro, Manuel Mejías y Francisco Mullor"
date: "19/01/2018"
output:
  html_document:
    keep_md: yes
    highlight: tango
    theme: united
    toc: yes
  html_notebook:
    code_folding: hide
    highlight: tango
    theme: united
    toc: yes
  word_document:
    toc: yes
---

# Introducción

En esta práctica realizaremos una predicción electoral empleando las distintas técnicas de cocina habitualmente usadas. Para ello utilizaremos los datos de la encuesta preelectoral del CIS para las elecciones del 26 de julio (estudio 3141). En primer lugar, postestratificaremos por dos variables: situación laboral y nivel de estudios. Los valores target de estas dos variables se han extraido de la Encuesta de Población Activa del INE. En segundo lugar, filtraremos la muestra para separar los/as votantes improbables. En tercer lugar, emplearemos un algoritmo de Machine Learning para asignar un voto probable a los indecisos. Por último, ponderaremos la predicción resultante por el recuerdo de voto para reducir el sesgo político de la muestra.




## Cargamos librerías e importamos los datos


```r
library(tidyverse)
library(dplyr)
library(haven)
library(car)
library(descr)
library(questionr)
library(weights)
library(anesrake)
library(knitr)
library(kableExtra)
library(caret)

setwd("~/Google Drive/DATOS/R/SCRIPTS MASTER/PRACTICAS DISENO ENCUESTAS/PRACTICA 4 PREDICCION/")
dat  <- read_sav("data/CIS3141.sav")
dat <- as.data.frame(dat) 
```

# Recodificamos variables

Recodificamos las variables que usaremos para postestratificar la muestra (situación laboral y nivel de estudios) para que coincidan con las categorías de la Encuesta de Población Activa del INE. También recodificamos las variables de voto+simpatia y recuerdo de voto para agrupar las confluencias de Podemos y simplificar la categoría otros partidos.


```r
# Recodifico Voto+simpatia
dat$votosim <- NA
dat$votosim[dat$VOTOSIMGR == 1] <-"PP"
dat$votosim[dat$VOTOSIMGR == 2] <-"PSOE"
dat$votosim[dat$VOTOSIMGR == 4] <-"Cs"
dat$votosim[dat$VOTOSIMGR %in% c(6, 7, 10, 31)] <-"UP + conflus"
dat$votosim[dat$VOTOSIMGR %in% c(8, 9, 11, 12, 13, 90)] <- "Otros partidos"
dat$votosim[dat$VOTOSIMGR == 95] <- "Nulo"
dat$votosim[dat$VOTOSIMGR == 96] <- "Blanco"
dat$votosim[dat$VOTOSIMGR %in% c(97:99)] <- "Indecisos"
dat$votosim <- factor(dat$votosim, levels = c("PP", "PSOE", "UP + conflus", "Cs", "Otros partidos", "Nulo", "Blanco", "Indecisos"))

# Recodifico recuerdo d voto
dat$recuerdo <- NA
dat$recuerdo[dat$RECUERDO == 1] <- "PP"
dat$recuerdo[dat$RECUERDO == 2] <- "PSOE"
dat$recuerdo[dat$RECUERDO == 4] <- "Cs"
dat$recuerdo[dat$RECUERDO %in% c(3, 6, 7, 10)] <- "Podemos + conflus"
dat$recuerdo[dat$RECUERDO == 5] <- "IU"
dat$recuerdo[dat$RECUERDO %in% c(8, 9, 14)] <- "Otros partidos"
dat$recuerdo[dat$RECUERDO == 15] <- "Blanco"
dat$recuerdo[dat$RECUERDO == 77] <- "Nulo"
dat$recuerdo[dat$RECUERDO %in% c(95, 97, 98, 99)] <- "No votó" 
dat$recuerdo <- factor(dat$recuerdo, levels = c("PP", "PSOE", "Podemos + conflus", "Cs", "IU", "Otros partidos", "Nulo", "Blanco", "No votó"))


# Recodifico situacion laboral
dat$SitLab3<-NA
dat$SitLab3[dat$P24 == 1] <- 1 # Trabaja
dat$SitLab3[dat$P24 %in% c(4,5)] <- 2 # Parados
dat$SitLab3[dat$P24 %in% c(2, 3, 6, 7)] <- 3 # Inactivo 
dat$SitLab3 <- factor(dat$SitLab3, levels = c(1,2,3), 
                      labels = c("Trabaja", "Parado/a", "Inactivo/a"))

# Recodifico nivel de estudios
dat$Estudios_rec <- dat$ESTUDIOS
dat$Estudios_rec[dat$Estudios_rec > 6] <- NA
dat$Estudios_rec <- factor(dat$Estudios_rec, levels = c(1,2,3,4,5,6), 
                           labels = c("Sin estudios", "Primaria", "Secundaria 1", "Secundaria 2", "FP", "Superiores"))
```

# 1. Post estratificación por provincia, situación laboral, y nivel de estudios

### Creo las poblaciones target
Conseguimos de la EPA las poblaciones targets a las que queremos acercar la muestra y creo una lista con ellas.

```r
# Creo un array con las frecuencias target extraídas de la EPA
SitLab3 <- c("Trabaja" = .5011, "Parado/a" = .1253,  "Inactivo/a" = .3736) 
Estudios_rec <- c("Sin estudios" = .081, "Primaria" = .145, "Secundaria 1" = .286, "Secundaria 2" = .135, "FP" = .074, "Superiores" = .278)

targets <- list(SitLab3,Estudios_rec) # Creo una lista con los target
names(targets) <- c("SitLab3", "Estudios_rec") # La doy nombres (esto es importante, deben coincidir con el nombre de la variable en los datos del CIS)
dat$caseid <- 1:length(dat$SitLab3) # Asigno un id a cada individuo
```


### Hacemos el raking y asignamos los pesos a cada encuestado
Aplico el procedimiento de raking especificando que parta de los factores de ponderación provistos por el CIS para corregir por la afijación no proporcional de la muestra. Observo los resultados, y asigno los factores de ponderación resultantes a cada individuo.

```r
outsave <- anesrake(targets, dat, weightvec = dat$PESO, # Aquí especificamos que parta de los pesos para corregir por la afijación no proporcional de la muestra
                    caseid = dat$caseid,
                    verbose= FALSE, cap = 5, choosemethod = "total",
                    type = "pctlim", pctlim = .05 , nlim = 5,
                    iterate = TRUE , force1 = TRUE) # este ultimo argumento especifica que en el caso de que las categorías de las variables no sumen 1 debido al redondeo fuerce los valores para que así sea.
```

[1] "Raking converged in 27 iterations"

```r
dat$weightvec  <- unlist(outsave[1]) # Asigno los pesos a los individuos de la muestra
```

### Resultados del raking

```r
t <- summary(outsave)
t1 <- as.data.frame(t$SitLab3)
t2 <- as.data.frame(t$Estudios_rec)

ktab <- kable(t1, digits = 4, align = "l")
kable_styling(ktab)
```

<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> Target </th>
   <th style="text-align:left;"> Old Weights N </th>
   <th style="text-align:left;"> Old Weights % </th>
   <th style="text-align:left;"> Wtd N </th>
   <th style="text-align:left;"> Wtd % </th>
   <th style="text-align:left;"> Change in % </th>
   <th style="text-align:left;"> Resid. Disc. </th>
   <th style="text-align:left;"> Orig. Disc. </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Trabaja </td>
   <td style="text-align:left;"> 0.5011 </td>
   <td style="text-align:left;"> 7594.497 </td>
   <td style="text-align:left;"> 0.4370 </td>
   <td style="text-align:left;"> 8706.351 </td>
   <td style="text-align:left;"> 0.5011 </td>
   <td style="text-align:left;"> 0.0641 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0.0641 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Parado/a </td>
   <td style="text-align:left;"> 0.1253 </td>
   <td style="text-align:left;"> 3024.156 </td>
   <td style="text-align:left;"> 0.1740 </td>
   <td style="text-align:left;"> 2177.022 </td>
   <td style="text-align:left;"> 0.1253 </td>
   <td style="text-align:left;"> -0.0487 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> -0.0487 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Inactivo/a </td>
   <td style="text-align:left;"> 0.3736 </td>
   <td style="text-align:left;"> 6758.598 </td>
   <td style="text-align:left;"> 0.3889 </td>
   <td style="text-align:left;"> 6491.105 </td>
   <td style="text-align:left;"> 0.3736 </td>
   <td style="text-align:left;"> -0.0153 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> -0.0153 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Total </td>
   <td style="text-align:left;"> 1.0000 </td>
   <td style="text-align:left;"> 17377.250 </td>
   <td style="text-align:left;"> 1.0000 </td>
   <td style="text-align:left;"> 17374.478 </td>
   <td style="text-align:left;"> 1.0000 </td>
   <td style="text-align:left;"> 0.1281 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0.1281 </td>
  </tr>
</tbody>
</table>

```r
ktab <- kable(t2, digits = 4, align = "l")
kable_styling(ktab)
```

<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> Target </th>
   <th style="text-align:left;"> Old Weights N </th>
   <th style="text-align:left;"> Old Weights % </th>
   <th style="text-align:left;"> Wtd N </th>
   <th style="text-align:left;"> Wtd % </th>
   <th style="text-align:left;"> Change in % </th>
   <th style="text-align:left;"> Resid. Disc. </th>
   <th style="text-align:left;"> Orig. Disc. </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Sin estudios </td>
   <td style="text-align:left;"> 0.0811 </td>
   <td style="text-align:left;"> 1140.588 </td>
   <td style="text-align:left;"> 0.0653 </td>
   <td style="text-align:left;"> 1415.050 </td>
   <td style="text-align:left;"> 0.0811 </td>
   <td style="text-align:left;"> 0.0157 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0.0157 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Primaria </td>
   <td style="text-align:left;"> 0.1451 </td>
   <td style="text-align:left;"> 3080.076 </td>
   <td style="text-align:left;"> 0.1765 </td>
   <td style="text-align:left;"> 2533.114 </td>
   <td style="text-align:left;"> 0.1451 </td>
   <td style="text-align:left;"> -0.0313 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> -0.0313 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Secundaria 1 </td>
   <td style="text-align:left;"> 0.2863 </td>
   <td style="text-align:left;"> 4227.678 </td>
   <td style="text-align:left;"> 0.2422 </td>
   <td style="text-align:left;"> 4996.349 </td>
   <td style="text-align:left;"> 0.2863 </td>
   <td style="text-align:left;"> 0.0441 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0.0441 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Secundaria 2 </td>
   <td style="text-align:left;"> 0.1351 </td>
   <td style="text-align:left;"> 2248.649 </td>
   <td style="text-align:left;"> 0.1288 </td>
   <td style="text-align:left;"> 2358.416 </td>
   <td style="text-align:left;"> 0.1351 </td>
   <td style="text-align:left;"> 0.0063 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0.0063 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FP </td>
   <td style="text-align:left;"> 0.0741 </td>
   <td style="text-align:left;"> 3044.604 </td>
   <td style="text-align:left;"> 0.1744 </td>
   <td style="text-align:left;"> 1292.762 </td>
   <td style="text-align:left;"> 0.0741 </td>
   <td style="text-align:left;"> -0.1004 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> -0.1004 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Superiores </td>
   <td style="text-align:left;"> 0.2783 </td>
   <td style="text-align:left;"> 3711.938 </td>
   <td style="text-align:left;"> 0.2127 </td>
   <td style="text-align:left;"> 4856.591 </td>
   <td style="text-align:left;"> 0.2783 </td>
   <td style="text-align:left;"> 0.0656 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0.0656 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Total </td>
   <td style="text-align:left;"> 1.0000 </td>
   <td style="text-align:left;"> 17453.534 </td>
   <td style="text-align:left;"> 1.0000 </td>
   <td style="text-align:left;"> 17452.282 </td>
   <td style="text-align:left;"> 1.0000 </td>
   <td style="text-align:left;"> 0.2634 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0.2634 </td>
  </tr>
</tbody>
</table>

# 2. Filtro de participación

En las anteriores elecciones del 20 de diciembre de 2015 votó el 69.67% del censo. Asumimos que la participación descenderá ligeramente debido a la repetición electoral y a las fechas estivales de la convocatoria electoral. Exploro las dos preguntas sobre probabilidad de ir a votar presentes en el cuestionario. Seremos más exigentes en el caso de quienes declaran que votarán por Unidos Podemos. La justificación de esto parte de tres premisas: 
1) El coste político derivado del "No" a la investidura de Pedro Sánchez y la sensación de que gran parte de la culpa de la repetición electoral recayó sobre Pablo Iglesias.
2) La unión con Izquierda Unida pudo haber desencantado a una parte del electorado de ambos partidos que estaba dispuesta a votar a uno de los dos partidos pero no a la coalición.
3) La presunción por parte de la mayoría de analistas y encuestas de que el sorpasso era un hecho pudo haber confiado al electorado de Unidos Podemos. Esto, junto a la fecha veraniega en la que se convocó a los ciudadanos, pudieron haber sido elementos decisivos para que el sorpasso no se materializase.

El cuestionario del CIS incluye dos preguntas distintas para filtrar a los potenciales votantes. Una que proporciona cuatro categorías de probabilidad y otra que proporciona una escala del 0 al 10. El procedimiento seguido es, por lo tanto, el siguiente: seleccionamos a los individuos que preguntados ante la probabilidad de ir a votar responden "Sí, con toda seguridad" o "Probablemente sí" y, a la vez, declaran una probabilidad de 8 (sobre) o más de asistir a las urnas. Una vez hecho esto, descartamos a quienes declaran que votarán por Unidos Podemos y responden "Probablemente sí" a la pregunta de probabilidad de ir a votar.

Esto nos deja con el 68,2% de la muestra ponderada. Nos parece razonable esta participación así que eliminamos de la muestra al resto de individuos.

```r
dat$prob.voto <- 0
dat$prob.voto[dat$P6A %in% c(1, 2) & dat$P12 >= 8] <- 1
dat$prob.voto[dat$P6A %in% c(1, 2) & dat$P12 < 8] <- 0
dat$prob.voto[dat$votosim == "UP + conflus" & dat$P6A == 2] <- 0  ## aumento el umbral para los que declaran votar a UP, IU, Podemos, etc...

unweighted <-  wpct(dat$prob.voto)
weighted  <-  wpct(dat$prob.voto, dat$weightvec)
tab  <- data.frame(unweighted, weighted)
rownames(tab) <- c("No votará", "Votará")

dat <- dat[dat$prob.voto == 1, ]

ktab <- kable(tab, digits = 4, align = "l", col.names = c("sin pesos", "con pesos"))
kable_styling(ktab)
```

<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> sin pesos </th>
   <th style="text-align:left;"> con pesos </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> No votará </td>
   <td style="text-align:left;"> 0.3266 </td>
   <td style="text-align:left;"> 0.3176 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Votará </td>
   <td style="text-align:left;"> 0.6734 </td>
   <td style="text-align:left;"> 0.6824 </td>
  </tr>
</tbody>
</table>



# 3. Imputación de votos a indecisos/as

En esta parte emplearemos un algoritmo de Machine Learning para clasificar a los/as indecisos/as en base a una serie de variables: sexo, edad, nivel de estudios, percepción de la situación política y económica, valoración de la actuación de los cuatro partidos principales y probabilidad de votar a los cuatro partidos. 

El filtro de participación nos ha dejado con 11.776 casos que consideramos que probablemente irán a votar. De esos, 1.605 son indecisos/as (un 13,6%). El modelo, debido a las no respuestas, solo es capaz de imputarle el voto a 767. Por lo tanto, nos quedamos finalmente con 838 indecisos/as que eliminaremos de la muestra.


```r
#############################
### Limpieza de variables ### 
#############################
dat$IVoto <- as.character(dat$votosim)

dat$sexo <- dat$P21
dat$edad <- as.numeric(dat$P22)
dat$edad[dat$edad == 99] <- NA

dat$int.pol <- dat$P1
dat$int.pol[dat$int.pol > 4] <- NA
dat$int.pol <- factor(dat$int.pol)


dat$sit.pol <- dat$P3
dat$sit.pol[dat$sit.pol > 5] <- NA
dat$sit.pol <- factor(dat$sit.pol)

dat$sit.econ <- dat$P4
dat$sit.econ[dat$sit.econ > 5] <- NA
dat$sit.econ <- factor(dat$sit.econ)

dat$valPP <- dat$P501
dat$valPP[dat$valPP > 5] <- NA

dat$valPSOE <- dat$P502
dat$valPSOE[dat$valPSOE > 5] <- NA

dat$ValPod <- dat$P503
dat$ValPod[dat$ValPod > 5] <- NA

dat$valCs <- dat$P504
dat$valCs[dat$valCs > 5] <- NA

dat$prob.PP <- dat$P1301
dat$prob.PP[dat$prob.PP > 10] <- NA

dat$prob.PSOE <- dat$P1302
dat$prob.PSOE[dat$prob.PSOE > 10] <- NA

dat$prob.Pod <- dat$P1303
dat$prob.Pod[dat$prob.Pod > 10] <- NA

dat$prob.Cs <- dat$P1304
dat$prob.Cs[dat$prob.Cs > 10] <- NA

####################################################################################################################

####################################################################################################################

## Selecciono las variables con las que voy a especificar el modelo
var <- c("IVoto", "sexo", "edad", "int.pol", "sit.pol", "sit.econ", "valPP", "valPSOE", "ValPod", "valCs", "prob.PP", "prob.PSOE", "prob.Pod", "prob.Cs", "Estudios_rec")

########################################
### Separo a los indecisos del resto ### 
########################################

## Separo los indecisos de los que declaran qué van a votar
decisos <- dat[dat$votosim != "Indecisos",]  ### Con estos entrenaré el modelo
indecisos <- dat[dat$votosim == "Indecisos",] 

## Los indecisos los separo entre los casos que no tienen valores perdidos en las variables independientes elegidas y los que sí tienen valores perdidos (a estos últimos no podré imputarles el voto mediante el modelo)
indecisos.com <- indecisos[complete.cases(indecisos[, var]),]  ### A estos les imputaré el voto con el modelo

indecisos.nocom <-  indecisos[!complete.cases(indecisos[, var]),]  ### A estos no puedo imputarles el voto porque no han contestado a alguna de las variables


#############################
### Especifico el modelo  ### 
#############################

# Creo la fórmula de la ecuación del modelo
formula <- IVoto ~ sexo + edad + int.pol + sit.pol + sit.econ + valPP + valPSOE + ValPod + valCs + prob.PP + prob.PSOE + prob.Pod + prob.Cs + Estudios_rec

# Después de probar con varias, nos quedamos con el ELM (Extreme Machine Learning)
set.seed(123)
model <- train(formula, data = decisos, na.action = "na.exclude", method = "elm") 

indecisos.com$IVoto <- predict(model, newdata = indecisos.com) # Usamos el modelo para predecir a los indecisos

dat <- rbind(decisos, indecisos.nocom, indecisos.com) # Vuelvo a unir los casos

# Convierto la variable en una categórica (factor)
dat$IVoto <- factor(dat$IVoto, levels = c("PP", "PSOE", "UP + conflus", "Cs", "Otros partidos", "Nulo", "Blanco", "Indecisos"))


n <- nrow(dat[dat$IVoto == "Indecisos",])
ntot <- nrow(dat)

print(paste("Después de usar el modelo para imputar el voto a los indecisos, nos quedan:", n, "indecisos, en una muestra de", ntot, "individuos que declaran una alta probabilidad de ir a votar."))
```

[1] "Después de usar el modelo para imputar el voto a los indecisos, nos quedan: 838 indecisos, en una muestra de 11776 individuos que declaran una alta probabilidad de ir a votar."


# 4. Corregimos por recuerdo de voto

Para terminar, ponderaremos la predicción resultante por recuerdo de voto para corregir el sesgo político de la encuesta. Para ello, calculamos los factores de ponderación que corrigen el sub y sobre-recuerdo de la muestra. A continuación, creamos una matriz de transferencia de votos a la cual aplicaremos los factores de ponderación resultantes. Finalmente, calculamos el porcentaje sobre voto válido de nuestra variable de intención de voto.

### Creamos los factores de ponderación

factor de ponderación = frecuencia poblacional / frecuencia muestral


```r
# Sobre censo
freq.mues <- as.numeric(wpct(dat$recuerdo, weight = dat$weightvec))
freq.target <- c(0.1982, 0.1519, 0.1427, 0.0963, 0.0254, 0.0705, 0.062, 0.051, 0.3033)
pesos <- freq.target / freq.mues

names(pesos) <- levels(dat$recuerdo)
```


### Los aplico a una matriz de transferencia de votos 


```r
tab <- questionr::wtd.table(dat$recuerdo, dat$IVoto, weights = dat$weightvec)
```



```r
ktab <- kable(tab, digits = 2, align = "l")
kable_styling(ktab)
```

<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> PP </th>
   <th style="text-align:left;"> PSOE </th>
   <th style="text-align:left;"> UP + conflus </th>
   <th style="text-align:left;"> Cs </th>
   <th style="text-align:left;"> Otros partidos </th>
   <th style="text-align:left;"> Nulo </th>
   <th style="text-align:left;"> Blanco </th>
   <th style="text-align:left;"> Indecisos </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> PP </td>
   <td style="text-align:left;"> 2547.18 </td>
   <td style="text-align:left;"> 68.62 </td>
   <td style="text-align:left;"> 15.84 </td>
   <td style="text-align:left;"> 138.48 </td>
   <td style="text-align:left;"> 4.81 </td>
   <td style="text-align:left;"> 2.25 </td>
   <td style="text-align:left;"> 19.82 </td>
   <td style="text-align:left;"> 66.15 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PSOE </td>
   <td style="text-align:left;"> 79.53 </td>
   <td style="text-align:left;"> 1905.96 </td>
   <td style="text-align:left;"> 115.67 </td>
   <td style="text-align:left;"> 96.39 </td>
   <td style="text-align:left;"> 13.70 </td>
   <td style="text-align:left;"> 5.08 </td>
   <td style="text-align:left;"> 40.75 </td>
   <td style="text-align:left;"> 34.31 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Podemos + conflus </td>
   <td style="text-align:left;"> 32.42 </td>
   <td style="text-align:left;"> 158.97 </td>
   <td style="text-align:left;"> 1700.07 </td>
   <td style="text-align:left;"> 52.45 </td>
   <td style="text-align:left;"> 63.62 </td>
   <td style="text-align:left;"> 11.05 </td>
   <td style="text-align:left;"> 24.01 </td>
   <td style="text-align:left;"> 25.14 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cs </td>
   <td style="text-align:left;"> 173.52 </td>
   <td style="text-align:left;"> 65.48 </td>
   <td style="text-align:left;"> 39.76 </td>
   <td style="text-align:left;"> 922.14 </td>
   <td style="text-align:left;"> 11.36 </td>
   <td style="text-align:left;"> 7.09 </td>
   <td style="text-align:left;"> 22.18 </td>
   <td style="text-align:left;"> 26.69 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IU </td>
   <td style="text-align:left;"> 6.93 </td>
   <td style="text-align:left;"> 38.33 </td>
   <td style="text-align:left;"> 385.54 </td>
   <td style="text-align:left;"> 13.56 </td>
   <td style="text-align:left;"> 2.36 </td>
   <td style="text-align:left;"> 2.06 </td>
   <td style="text-align:left;"> 7.27 </td>
   <td style="text-align:left;"> 9.34 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Otros partidos </td>
   <td style="text-align:left;"> 24.81 </td>
   <td style="text-align:left;"> 28.87 </td>
   <td style="text-align:left;"> 63.36 </td>
   <td style="text-align:left;"> 42.01 </td>
   <td style="text-align:left;"> 730.75 </td>
   <td style="text-align:left;"> 8.00 </td>
   <td style="text-align:left;"> 21.75 </td>
   <td style="text-align:left;"> 17.35 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Nulo </td>
   <td style="text-align:left;"> 2.03 </td>
   <td style="text-align:left;"> 2.96 </td>
   <td style="text-align:left;"> 0.63 </td>
   <td style="text-align:left;"> 4.02 </td>
   <td style="text-align:left;"> 1.13 </td>
   <td style="text-align:left;"> 25.97 </td>
   <td style="text-align:left;"> 2.44 </td>
   <td style="text-align:left;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Blanco </td>
   <td style="text-align:left;"> 18.32 </td>
   <td style="text-align:left;"> 11.23 </td>
   <td style="text-align:left;"> 5.69 </td>
   <td style="text-align:left;"> 4.57 </td>
   <td style="text-align:left;"> 2.70 </td>
   <td style="text-align:left;"> 10.66 </td>
   <td style="text-align:left;"> 73.37 </td>
   <td style="text-align:left;"> 8.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> No votó </td>
   <td style="text-align:left;"> 423.26 </td>
   <td style="text-align:left;"> 306.24 </td>
   <td style="text-align:left;"> 229.18 </td>
   <td style="text-align:left;"> 177.97 </td>
   <td style="text-align:left;"> 75.35 </td>
   <td style="text-align:left;"> 8.65 </td>
   <td style="text-align:left;"> 47.98 </td>
   <td style="text-align:left;"> 598.18 </td>
  </tr>
</tbody>
</table>
##### Matriz de transferencia (sin corrección por recuerdo de voto)



```r
# Aplico los pesos a las filas
tab[1,] <- tab[1,] *pesos[1]
tab[2,] <- tab[2,] *pesos[2]
tab[3,] <- tab[3,] *pesos[3]
tab[4,] <- tab[4,] *pesos[4]
tab[5,] <- tab[5,] *pesos[5]
tab[6,] <- tab[6,] *pesos[6]

ktab <- kable(tab, digits = 2, align = "l")
kable_styling(ktab)
```

<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> PP </th>
   <th style="text-align:left;"> PSOE </th>
   <th style="text-align:left;"> UP + conflus </th>
   <th style="text-align:left;"> Cs </th>
   <th style="text-align:left;"> Otros partidos </th>
   <th style="text-align:left;"> Nulo </th>
   <th style="text-align:left;"> Blanco </th>
   <th style="text-align:left;"> Indecisos </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> PP </td>
   <td style="text-align:left;"> 2104.24 </td>
   <td style="text-align:left;"> 56.69 </td>
   <td style="text-align:left;"> 13.08 </td>
   <td style="text-align:left;"> 114.40 </td>
   <td style="text-align:left;"> 3.98 </td>
   <td style="text-align:left;"> 1.86 </td>
   <td style="text-align:left;"> 16.37 </td>
   <td style="text-align:left;"> 54.65 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PSOE </td>
   <td style="text-align:left;"> 62.92 </td>
   <td style="text-align:left;"> 1507.81 </td>
   <td style="text-align:left;"> 91.51 </td>
   <td style="text-align:left;"> 76.25 </td>
   <td style="text-align:left;"> 10.84 </td>
   <td style="text-align:left;"> 4.02 </td>
   <td style="text-align:left;"> 32.23 </td>
   <td style="text-align:left;"> 27.14 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Podemos + conflus </td>
   <td style="text-align:left;"> 26.70 </td>
   <td style="text-align:left;"> 130.93 </td>
   <td style="text-align:left;"> 1400.13 </td>
   <td style="text-align:left;"> 43.19 </td>
   <td style="text-align:left;"> 52.40 </td>
   <td style="text-align:left;"> 9.10 </td>
   <td style="text-align:left;"> 19.77 </td>
   <td style="text-align:left;"> 20.71 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cs </td>
   <td style="text-align:left;"> 157.23 </td>
   <td style="text-align:left;"> 59.34 </td>
   <td style="text-align:left;"> 36.03 </td>
   <td style="text-align:left;"> 835.61 </td>
   <td style="text-align:left;"> 10.29 </td>
   <td style="text-align:left;"> 6.42 </td>
   <td style="text-align:left;"> 20.10 </td>
   <td style="text-align:left;"> 24.18 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IU </td>
   <td style="text-align:left;"> 4.51 </td>
   <td style="text-align:left;"> 24.97 </td>
   <td style="text-align:left;"> 251.11 </td>
   <td style="text-align:left;"> 8.83 </td>
   <td style="text-align:left;"> 1.54 </td>
   <td style="text-align:left;"> 1.34 </td>
   <td style="text-align:left;"> 4.73 </td>
   <td style="text-align:left;"> 6.09 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Otros partidos </td>
   <td style="text-align:left;"> 22.28 </td>
   <td style="text-align:left;"> 25.92 </td>
   <td style="text-align:left;"> 56.89 </td>
   <td style="text-align:left;"> 37.73 </td>
   <td style="text-align:left;"> 656.21 </td>
   <td style="text-align:left;"> 7.18 </td>
   <td style="text-align:left;"> 19.53 </td>
   <td style="text-align:left;"> 15.58 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Nulo </td>
   <td style="text-align:left;"> 2.03 </td>
   <td style="text-align:left;"> 2.96 </td>
   <td style="text-align:left;"> 0.63 </td>
   <td style="text-align:left;"> 4.02 </td>
   <td style="text-align:left;"> 1.13 </td>
   <td style="text-align:left;"> 25.97 </td>
   <td style="text-align:left;"> 2.44 </td>
   <td style="text-align:left;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Blanco </td>
   <td style="text-align:left;"> 18.32 </td>
   <td style="text-align:left;"> 11.23 </td>
   <td style="text-align:left;"> 5.69 </td>
   <td style="text-align:left;"> 4.57 </td>
   <td style="text-align:left;"> 2.70 </td>
   <td style="text-align:left;"> 10.66 </td>
   <td style="text-align:left;"> 73.37 </td>
   <td style="text-align:left;"> 8.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> No votó </td>
   <td style="text-align:left;"> 423.26 </td>
   <td style="text-align:left;"> 306.24 </td>
   <td style="text-align:left;"> 229.18 </td>
   <td style="text-align:left;"> 177.97 </td>
   <td style="text-align:left;"> 75.35 </td>
   <td style="text-align:left;"> 8.65 </td>
   <td style="text-align:left;"> 47.98 </td>
   <td style="text-align:left;"> 598.18 </td>
  </tr>
</tbody>
</table>
##### Matriz de transferencia (con corrección por recuerdo de voto)


### Calculamos nuestra predicción sobre voto válido

```r
votoval <- sum( sum(tab[,1]), sum(tab[,2]), sum(tab[,3]), sum(tab[,4]), sum(tab[,5]),sum(tab[,7]))  # Sumamos los casos que constituyen el voto válido (excluímos a esos 838 indecisos a los que no hemos imputado el voto y los votos nulos)


# Calculo el voto a partido sobre voto válido
results.vv <- c("PP" = sum(tab[,1]) / votoval,
                      "PSOE" = sum(tab[,2]) / votoval,
                      "UP" = sum(tab[,3]) / votoval,
                      "Cs" = sum(tab[,4]) / votoval,
                      "Otros partidos" = sum(tab[,5]) / votoval,
                      "Blanco" = sum(tab[,7]) / votoval)



# Resultados oficiales sobre voto valido 26J
vv26J <- c(0.3301, 0.2263, 0.2115, 0.1306, 0.0937, 0.0074)


# Tabla resumen
results.vv <- data.frame("pred.votoválido" = results.vv,
                        "resultados" = vv26J)

# Calculo la diferencia entre predicción y resultado oficial
results.vv$diff <- results.vv$pred.votoválido - results.vv$resultados

# Multiplico por 100
results.vv$pred.votoválido <- results.vv$pred.votoválido*100
results.vv$resultados <- results.vv$resultados*100
results.vv$diff <- results.vv$diff*100
```


# 5. Resultados

Como se puede comprobar al comparar los resultados de la predicción con los resultantes de las elecciones del 26J, nuestro pronóstico electoral es bastante preciso: 1) acierta el orden de los cuatro partidos principales, 2) también acierta las distancias entre los distintos partidos, y 3) no predice el sorpasso de Unidos Podemos al PSOE.

Aunque es cierto que hemos trabajado conociendo los resultados, ya en campaña existía cierto clima de opinión que consideraba que la culpa de la repetición electoral residió principalmente en Podemos. Por lo tanto, consideramos que la clave principal de este buen pronóstico (ser más estrictos con la probabilidad de ir a votar de quienes declaraban que votarían a UP) podría haber sido prevista.

```r
ktab <- kable(results.vv, digits = 2, align = "l", format = "html", booktabs = T, col.names = c("Predicción",
                                                                                                "Resultados 26J",
                                                                                                "Diferencia"))
kable_styling(ktab)
```

<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> Predicción </th>
   <th style="text-align:left;"> Resultados 26J </th>
   <th style="text-align:left;"> Diferencia </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> PP </td>
   <td style="text-align:left;"> 30.06 </td>
   <td style="text-align:left;"> 33.01 </td>
   <td style="text-align:left;"> -2.95 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PSOE </td>
   <td style="text-align:left;"> 22.65 </td>
   <td style="text-align:left;"> 22.63 </td>
   <td style="text-align:left;"> 0.02 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> UP </td>
   <td style="text-align:left;"> 22.21 </td>
   <td style="text-align:left;"> 21.15 </td>
   <td style="text-align:left;"> 1.06 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cs </td>
   <td style="text-align:left;"> 13.88 </td>
   <td style="text-align:left;"> 13.06 </td>
   <td style="text-align:left;"> 0.82 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Otros partidos </td>
   <td style="text-align:left;"> 8.68 </td>
   <td style="text-align:left;"> 9.37 </td>
   <td style="text-align:left;"> -0.69 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Blanco </td>
   <td style="text-align:left;"> 2.52 </td>
   <td style="text-align:left;"> 0.74 </td>
   <td style="text-align:left;"> 1.78 </td>
  </tr>
</tbody>
</table>






