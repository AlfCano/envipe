## Importar ENOE desde datos abiertos 3t2017

## Instalar paquetes necesarios
install.packages(c("stringr", "rio", "janitor", "tidyr", "tibble", "dplyr", "lookup", "uchardet"))

##Crear la función iconv.recursive
# La función "iconv.recursive" fue elaborada por los contribuidores de RKWard, y se encuentra disponible en su menú de importación genérica "Archivo->Importar->Formato de Importación->Importación genérica (basada en rio)"
# Crea la función auxiliar "iconv.recursive" para convertir todas las cadenas a la codificación actual.
iconv.recursive <- function (x, from) { # Asigna la función subsecuente al objeto llamado "iconv.recursive".
	attribs <- attributes (x); # Accede a la lista de los atributos de un objeto.
	if (is.character (x)) {    # Si es de tipo character...
		x <- iconv (x, from=from, to="", sub="") # aplica la función "iconv()" para convertir vectores de caracteres entre codificaciones.
	} else if (is.list (x)) {    # Si es una lista.
		x <- lapply (x, function (sub) iconv.recursive (sub, from)) # Se aplica para cada elemento de la lista con la función "lapply()".
	}
	attributes (x) <- lapply (attribs, function (sub) iconv.recursive (sub, from)) # Crea los atributos para todos los elementos de la lista.
	x
} # Fin de la declaración de la función. 

## Crear ruta de acceso al directorio extraído desde DA
# se debe indicar la dirección en la que se ha extraído el directorio, desde el archivador:  "> ~2017_trim3_enoe_csv"

local({                           # Inicia el ambiente local.
fp   <- list()                    # Crea la lista para asignar las rutas a los archivos.
dir  <-  "/home/cano/Escritorio/ENOE/ENVIPE_2023" # Se coloca aquí la dirección al directorio que contiene las carpetas "conjunto de datos_...". 
cd   <-  "conjunto_de_datos"      # Cadena constante en los nombres de las tablas.
tip  <- (c("tsdem","tper_vic1","tper_vic2")) # Combina cadenas en una columna (vector) con los nombres de las tablas a importar.
prog <-  "envipe2023"                   # Programa de información que se importará.
#year <-  "2023"                   # Periodo.
#t	 <-  ""                     # Trimestre.
fp$files <- list()                # Crea la lista para asignar el resultado en el objeto "tab" del loop siguiente (CONTENEDOR).
for(i in tip) {                   # Inicia el loop con "for(){}", para cada el elemento en el puesto "i" (i-ésimo) en la columna "tip" (SECUENCIA).
tab <- paste(
		#cd, 
		i, 
		prog, 
		#year, 
		#t,
		sep = "_") # Pega los nombres para cada puesto "i" (OPERACIONES).
r   <- file.path (dir, tab, fsep = .Platform$file.sep) # Crea la ruta de archivos con el separador de la plataforma "/" ó "\".
.GlobalEnv$fp[[i]] <- r           # Asigna el resultado de la ruta de archivo al ambiente global.
.GlobalEnv$fp$files[[i]] <- tab   # Asigna la cadena con el nombre del directorio a la lista "files" en el ambiente global.
.GlobalEnv$fp$dir <- dir          # Asigna el objeto dir al entorno global. Éste aloja la ruta que servirá para guardar el resultado, la tabla con las tres tablas unidas y sus metadatos.
}  # Cierra el loop.
}) # Cierra el ambiente local.


## Dirección donde se alojará la tabla al final del guion  
# Designación del directorio de trabajo con la función `setwd`
local({
setwd(fp$dir) # Ejecuta la función "setwd". 
})

# ### SDEM ###

## Importar la lista "cat" para tabla "sdem"
# Designación del directorio de trabajo con la función `setwd` Catálogo de datos: "catalogos"

local({
## Computar 
setwd(file.path(fp$tsdem,"catalogos", fsep = .Platform$file.sep)) 
}) 

local({
## Computar 
.GlobalEnv$meta.sdem <- list()        # Crea la lista "meta.sdem" en el Ambiente Global (".GlobalEnv").        
}) 

## Importar la lista "cat" para tabla "sdem"

local({
## Preparar
library("rio")       # Carga el paquete "rio".
##Computar
sdem_files <- list.files(pattern = "\\.csv$") # Enlista los archivos en directorio con extensión csv.
.GlobalEnv$meta.sdem$cat <- list()    # Crea la lista "cat" dentro de "meta.sdem" que guardará el resultado del loop.
for (i in sdem_files) {         
data <- import(i)               # Importa los archivos listados en el directorio.
data <- iconv.recursive (data, from= "latin1") # Convierte todas las cadenas de "latin1" a la actual.
##Asignar el resultado
.GlobalEnv$meta.sdem$cat[[i]] <- data # Asigna el resultado a la lista "cat" en el Ambiente Global dentro de la lista "meta.sdem".
}
})


# est_dis.csv tiene un ofendig character
# Para limpiar los nombres usamos de la cadena ".csv" de "stringr".

## Preparar
library("stringr")
##Computar
names(meta.sdem$cat) <- str_replace(names(meta.sdem$cat), pattern = ".csv", replacement = "") # Para eliminar la cadena ".csv", de los nombres se aplica la función"str_replace()" para reemplazar: ".csv" dentro de los nombres ("names()") de la lista ("meta.sdem$cat") que contiene las tablas correspondientes a los archivos importados y reemplazar por: "", es decir, por cadena vacía.


#library(stringr)
names(meta.sdem$cat) <- str_to_upper(names(meta.sdem$cat)) ## Revisar si es necesario cambiar de mayúsculas a minúsculas. 

#library(uchardet)
meta.sdem$cat$NIV <- read.csv('niv.csv', encoding="latin1") 



##  Crear la variable CVE, dentro de cada archivo en la lista "cat"
#   Esta acción permitirá copiar las etiquetas de valor al proveer el mismo nombre para cada variable
#   que contien estas etiquetas.
local({
library("stringr")
##Computar
l <- meta.sdem[["cat"]]             # Se copia la lista "cat" al objeto "l" en el entorno local, para simplificar la notación.
list_names <- names(l)              # Extramemos los nombres de los objetos que contiene la lista "l" en el objeto "list_names."
for (i in list_names){              # Iniciamos el loop para cada observación "i" en la lista "list_names".
f <- meta.sdem[["cat"]][[i]]
# Extraemos un data.frame de la lista que se corresponde con los nombres enlistados en "list_names".
f  <- mutate_if(f, is.character, function(x) str_squish(x))
f$CVE <- f[[i]]                     # Para el marco de datos "f" copiamos la columna de datos homónima del data.frame y le colocamos el nómbre el nombre genérico "CVE".
l[[i]] <- f}                        # Reinsertamos f en la lista l con la nueva variable CVE en el objeto homónimo "i".
## Imprimir el resultado
.GlobalEnv$meta.sdem[["cat"]] <- l #Insertamos la lista con los data.frames modificados en la lista "meta.sdem$cat" dento del entorno global.
})


meta.sdem[["cat"]][["NIV"]][["CVE"]] <- as.numeric(meta.sdem[["cat"]][["NIV"]][["CVE"]])

meta.sdem[["cat"]][["NIV"]][["CVE"]] <- as.character(meta.sdem[["cat"]][["NIV"]][["CVE"]])


## Diccionario de datos "sdem"

#Importar diccionario de datos. Al final se limpian los nombres con la función `clean_names()` (de `janitor`).

local({                
## Preparar
library("rio")
library("janitor")
## Computar
setwd(file.path(fp$tsdem,"diccionario_de_datos", fsep = .Platform$file.sep))
file <- list.files(pattern = "\\.csv$")
data <- import(file) # Importa el archivo al objeto "data".
data <- iconv.recursive (data, from="latin1")
data <- clean_names (data) # Se limpian los nombres del "data.frame".
## Asignar el resultado  
.GlobalEnv$meta.sdem$dic <- data
})

## Creación del data.frame "noms", para seleccionar variables a convertir en factor.

# Se crea un marco de datos para seleccionar las variables cuyos valores deberán ser etiquetados.
 
local({     
## Preparar
library("tidyr")
library("tibble")
library("dplyr")
## Computar
noms <- data.frame(cbind("catalogo"=meta.sdem$dic[["nemonico"]],
"rango_claves"=meta.sdem$dic[["rango_claves"]],
"nombre_campo"=meta.sdem$dic[["nombre_campo"]])) # Crea el data.frame "noms" con las columnas con las variables "catalogo" y "nombre_campo" de la tabla "dic" .
#noms[["rango_claves"]] <- replace(noms[["rango_claves"]], noms[["rango_claves"]]==',,', NA) # Cambia celdas en blanco por NA.
noms <- distinct(noms, catalogo, .keep_all = TRUE)
noms  <- rownames_to_column(noms, var="id")
fctrs <- noms[!grepl(',,', noms[["rango_claves"]] ),]
## Asignar el resultado  
.GlobalEnv$meta.sdem$noms <- noms
.GlobalEnv$meta.sdem$fctrs <- fctrs
})

# Se cambió "catalogo" por "nemonico" y se agregó distict() con dplyr.


# Variables que se removerán se encuentran en la lista "rem": `"r_def","loc","mun","v_sel","n_hog","h_mud","n_ent","n_ren","eda","nac_dia","nac_mes" y "cs_p13_2"`.
local({
## Preparar
library ("dplyr")
## Computar
df <- meta.sdem$fctrs #Copia el data.frame nomms al objeto df dentro del Ambiente Local.
rem <- list("EDAD","CVE_ENT") # Crea la lista "rem" con los nombres de las variables numéricas que no se convertirán en tipo factor.
df <- df %>%
  filter(!catalogo %in% rem) # Elimina ("!") las filas de la variable "catalogo" que contienen elementos en ("%in%") en la lista "rem".
## Asignar el resultado
.GlobalEnv$meta.sdem$fctrs <- df
})

# Por último, se elimina el registro con "l_nac" en la variable "nemonico" de la tabla "dic". La variable "l_nac" no tiene contraparte con una tabla de etiquetas de valor, ni con una variable en el conjunto de datos que se importará en el siguiente apartado. En cambio, **"l_nac_c"** si tiene una variable homónima en el conjunto de datos y etiquetas en la lista "cat".

# local({
# ## Preparar
# library("dplyr")
# ## Computar
# df <- meta.sdem[["dic"]] %>%
#   filter(!nemonico %in% "l_nac")
# ## Asignar el resultado
# .GlobalEnv$meta.sdem[["dic"]] <- df
# })

#Se obtiene un marco de datos sin el registro l_nac con ciento cuatro filas.

## Selección de variables que requieren etiquetas de valor 
 # Crea el objeto etq.val y se descartan los objetos que ya tienen etiquetas de valor.
                                                    # En este caso nombres de entidad (variable 8) y municipio (variable 9) en este data.frame.
local({
## Preparar
library ("dplyr")
## Computar
df <- meta.sdem$fctrs #Copia el data.frame nomms al objeto df dentro del Ambiente Local.
rem <- list("NOM_MUN","NOM_ENT") # Crea la lista "rem" con los nombres de las variables numéricas que no se convertirán en tipo factor.
df <- df %>%
  filter(!catalogo %in% rem) # Elimina ("!") las filas de la variable "catalogo" que contienen elementos en ("%in%") en la lista "rem".
## Asignar el resultado
.GlobalEnv$meta.sdem$etq.val <- df
})

## Importación del conjunto de datos y asignación de metadatos

#Importamos los datos sociodemográficos "sdem"
local({
## Computar
setwd(file.path(fp$tsdem,"conjunto_de_datos", fsep = .Platform$file.sep))
#sdem <- fp$files$tsdem
file <- "conjunto_de_datos_tsdem_envipe2023.csv"
#file <- paste(sdem,"csv",sep = ".")
data <- read.csv (file=file, na.strings = "NA", nrows = -1, skip = 0, check.names = TRUE, strip.white = FALSE, blank.lines.skip = TRUE)
data <- iconv.recursive (data, from="latin1")
## Asignar el resultado
.GlobalEnv$meta.sdem$data <- data
})

library("stringr")
meta.sdem$data  <- mutate_if(meta.sdem$data , is.character, function(x) str_squish(x))

# Debido a que `R` distingue entre mayúsculas y minúsculas, el tipo de letra en las variables "nemonico" y "catalogo" dentro del marco de datos "dic" debe ser igual a los nombres de las tablas dentro de la lista "cat" y al tipo de letrea en los nombres de las variables dentro del conjunto de datos.
# Es posible convertir mayúsculas o minúsculas según sea el caso para que coincidan los nombres que se encuentran en la lista "cat" y los nombres en el . Se puede utilizar las funciones `str_to_upper( )` o ` str_to_lower( )` del paquete `stringr`. En el caso "3t2017" los nombres de los "data.frames" contenidos dentro de `meta.sdem[["cat"]]`se encuentran en minúsculas. Sin embargo, si fuera necesario, un ejemplo de lo anterior sería:  

#library(stringr)
#names(meta.sdem$data) <- str_to_lower(names(meta.sdem$data)) ## Revisar si es necesario cambiar de mayúsculas a minúsculas. 

# Cuando se tiene el conjunto de datos importado, es posible convertir en variables de factor aquellas cuyos nombres se encuentran en la columna "catalogos" del marco de datos "noms".

local({
## Computar
list_names <- meta.sdem$fctrs[["catalogo"]] # Copia la columna con los nombres de las variables deseadas al objeto "list_names".
for (i in list_names)  {
f <- meta.sdem$data # Copia el objeto "data" de la lista "meta.sdem" al objeto "f".
f[[i]]<- as.factor(f[[i]]) # Con la función "as.factor()" del paquete "base".
## Asignar el resultado
.GlobalEnv$meta.sdem$data <- f
}
})

#Ahora se asignan las etiquetas de valor en el formato `RKWard` con la función `rk.set.label()`. Para aplicar la etiqueta correspondiente a cada una de las variables del conjunto de datos, se ha construido un loop con la función `vlookup()` del paquete `lookup`.

local({
## Preparar
library("lookup")
## Computar
d <- meta.sdem$data  # Para simplificar la notación asignamos el objeto "data" de la lista "meta.sdem" al objeto "d".
list_nem <- meta.sdem$noms$catalogo # Para simplificar la notación se copia la variable "nemonico" del data.frame "dic" en la lista "meta.sdem" al objeto "list_nem"
for (i in list_nem) {
rk.set.label(d[[i]], # Asigna las etiquetas de valor en el objeto "d" a cada variable "i".
vlookup(i,        # Busca los valores "i" en
meta.sdem$noms,    # la tabla seleccionada "dic",  
"catalogo",       # en la variable "nemonico" y
"nombre_campo"))  # devuelve la cadena correspondiente en la variable "nombre_campo".
                }
## Asignar el resultado
.GlobalEnv$meta.sdem$data <- d
})






# Finalmente, se colocan las etiquetas de valor a cada nivel. Esto se logra con la aplicación del resultado a los niveles (con la función `levels()`) de cada una de las variables de factor.  

local({
## Preparar
library("lookup")
## Computar
list_names <- meta.sdem$etq.val[["catalogo"]]  # Crea la lista de nombres "list_names" con la variable "catalogo"
d <- meta.sdem$data  
for (i in list_names){
f <- meta.sdem$cat[[i]] # Crea el objeto "f" que contiene las categorías a colocar que itera por cada observación "i" contenida en "list_names".
v <- d[[i]]             # Luego crea el objeto "v" que contiene la variable a la que se asignarán los valores conforme a la siguiente búsqueda.
levels(v) <- vlookup(levels(v),     # Pega las categorías en los niveles de "v" con la función "levels", tras comparar con la función "lookup",los valores en los niveles de "v" también a través de la función "levels",
f,          # en el contenido de cada tabla "f".
"CVE",      # En el data.frame "f", busca por parejas el valor de la clave en la variable "CVE" de cada nivel con
"descrip")  # la etiqueta de valor en la variable "DESCRIP".
## Asignar el resultado
d[[i]] <- v  
}
.GlobalEnv$meta.sdem$data <- d
nrow(meta.sdem$data) # Para obtener el recuento: "[1] 392178"
})   

# Hasta este punto, se han obtenido en la tabla "data" `[1] 392178` registros, ciento cuatro columnas etiquetadas y setenta y tres variables de factor con etiquetas de valor a través de los metadatos proporcionados en linea con el estándar DA. Para obtener el recuento anterior se puede ejecturar:



 # En la ENOEN 3t 2021 no existe archivo de etiquetas para la variable "l_nac_c" ("Pregunta 11 Lugar de nacimiento 30"). Y en las versiones del 2017 al 2020 la variable "cs_p14_c" destacó que sus etiquetas de valor sólo aparecen en mayúsculas, mientras que en la 2021 y 2022 aparecen en altas y bajas con uso de tilde. 
 
# En la importación de la ENOEN 3t 2021 se observó que no existe archivo de etiquetas para la variable "l_nac_c" ("Pregunta 11 Lugar de nacimiento 30"). Por otro lado, en las versiones del 2017 a la 2020 la variable "cs_p14_c" destacó que sus etiquetas de valor sólo aparecen en mayúsculas, mientras que en la 2021 y 2022 aparecen en altas y bajas con uso de tilde. También para esta variable en el caso del 3t 2022 se observó que su archivo de categorías no tiene nombres de columnas "CVE" ni "DESCRIP". Para corregir eso se hizo: 

# {r cs_p14_c, eval=F, echo=T, purl = TRUE} 
# local({
# p <- meta.sdem$cat$cs_p14_c
# p$CVE <- p$V1
# p <- within(p,rm(V1))
# p$DESCRIP <- p$V2
# p <- within(p,rm(V2))
# .GlobalEnv$meta.sdem$cat$cs_p14_c <- p
# })

# Se encuentra en la columna 33 del conjunto de datos. 

## Filtrar casos de la tabla "sdem"

#Para unir las tres tablas ("sdem" + "vic1" + "vic2") con base en las instrucciones en INEGI [-@inegiConociendoBaseDatos2010], se procede al filtrado de la tabla de la tabla "sdem". Primeramente se deben eliminar los registros de la variable "r_def" ("Resultado definitivo de la entrevista") diferentes de 00, las cuales corresponden a entrevistas incompletas o no logradas. También los registros con condición de residencia ausente (c_res="Ausente definitivo"). Así mismo, se debe suprimir a los menores de 12 años `(eda > 11)`, ya que las tablas "vic1" y "vic2" sólo incluyen a personas de 12 años y más. Finalmente, se eliminan los registros que tienen registros iguales a 99 en edad `(eda != 99)`, y que corresponden a "Años no especificados de menores de 12"^[Las personas con `eda == 98` se etiquetan como "Años no especificados de 12 años y más" [@inegiEncuestaNacionalOcupacion2020]]. Por lo anterior, se aplicó la siguiente condición de selección: `(r_def == 0) & (c_res != "Ausente definitivo") & (eda > 11) & (eda != 99)")`. El filtro se aplica a través de la función `filter()` (de `dplyr`) y asigna el resultado a la nueva lista "count.df":
# 
# local({
# library("dplyr")
# ## Computar
# .GlobalEnv$count.df <- list()
# .GlobalEnv$count.df$sdem <- filter(meta.sdem$data, (r_def == 0) & (c_res != "Ausente definitivo") & (eda >11) & (eda!=99))
# nrow(count.df$sdem)
# })
	 
#Se obrienen, `[1] 307117` registros en la tabla "sdem". Este número deberá coincidir con el obtenido de las tablas "vic1" y "vic2". 
# Contar los registros que se tienen en la tabla "sdem".

count.df <- list()
count.df$sdem <- meta.sdem$data
nrow(count.df$sdem)
# Ahora que cuenta con el marco de datos "sdem" en la lista recién creada "count.df " y se ha comprobado el número de registros puede considerar remover la base de datos "data" de a lista "meta.sdem" con:

# meta.sdem <- within(meta.sdem, rm(data))




# ### vic1 ###

## Importar la lista "cat" para tabla "COE1"

local({
## Computar
setwd(file.path(fp[["tper_vic1"]],"catalogos", fsep = .Platform$file.sep))
})

## Importar catálogos

local({
## Preparar
#library("rio")
## Computar
cat_files <- list.files(pattern = "\\.csv$")
.GlobalEnv$meta.vic1 <- list()
.GlobalEnv$meta.vic1$cat <- list()
for (i in cat_files) {
data <- read.csv(i)
data <- iconv.recursive (data, from="latin1") # Se asume que se ha guardado la función "icon.recursive()", en el espacio de trabajo.
## Asignar el resultado
.GlobalEnv$meta.vic1$cat[[i]] <- data
}
})

# Para limpiar los nombres usamos de la cadena ".csv" de "stringr".

library("stringr")
names(meta.vic1$cat) <- str_replace(names(meta.vic1$cat), pattern = ".csv", replacement = "")


#library(stringr)
names(meta.vic1$cat) <- str_to_upper(names(meta.vic1$cat)) ## Revisar si es necesario cambiar de mayúsculas a minúsculas. 

##  Crear la variable CVE, dentro de cada archivo en la lista "cat"
#   Esta acción permitirá copiar las etiquetas de valor al proveer el mismo nombre para cada variable
#   que contien estas etiquetas.
local({
library("stringr")
library("dplyr")
##Computar
l <- meta.vic1[["cat"]]             # Se copia la lista "cat" al objeto "l" en el entorno local, para simplificar la notación.
list_names <- names(l)              # Extramemos los nombres de los objetos que contiene la lista "l" en el objeto "list_names."
for (i in list_names){              # Iniciamos el loop para cada observación "i" en la lista "list_names".
f <- meta.vic1[["cat"]][[i]]
# Extraemos un data.frame de la lista que se corresponde con los nombres enlistados en "list_names".
f  <- mutate_if(f, is.character, function(x) str_squish(x))
f  <- mutate_if(f, is.numeric, function(x) as.character(x))
f$CVE <- f[[i]]                     # Para el marco de datos "f" copiamos la columna de datos homónima del data.frame y le colocamos el nómbre el nombre genérico "CVE".
l[[i]] <- f}                        # Reinsertamos f en la lista l con la nueva variable CVE en el objeto homónimo "i".
## Imprimir el resultado
.GlobalEnv$meta.vic1[["cat"]] <- l #Insertamos la lista con los data.frames modificados en la lista "meta.vic1$$cat" dento del entorno global.
})

# Las etiquetas en la tabla "p4d2" en la lista "cat" presenta dos valores en el mismo campo para cada registro. Para asignar las etiquetas de manera correcta, estos deben ser limpiados y separados en dos columnas, recodificados condicionalmente y el marco de datos debe ser reformado a formato "longer" construir las variables de índice y asignación necesarias. La solución para dividir, limpiar las etiquetas y reconstruir el marco de datos de la tabla "p4d2" se presenta en las líneas 296-348 del guion en extenso. Por otro lado en las líneas 431-435 para realizar la recodificación correspondiente en el marco de datos en la variable homónima. 

# Luego de la importación de los catálogos ("cat") en la lista "meta.vic1" se realizan las siguientes operaciones en la tabla "p4d2". Primero se eliminan las cadenas "P4d=1 " y ": " con la función `sub()` combinado con el operador `%>%` :  

# local({
# ## Preparar
# library("dplyr")
# ## Computar
# d <- meta.vic1[["cat"]][["p4d2"]][["DESCRIP"]]
# d1 <- d %>%
#           sub("P4d=1: ","", .  # Usamos la función "sub()" para sustituir "P4d=1: " por cadena vacía '""'. Y se usa el comodín para colocar el resultado.
#           ) %>% sub(": ", "", .)    # Nuevamente se encadena para substituir la cadena ": " por cadena vacía '""'. E igualmente se coloca "." para indicar la posición del resultado anterior.
# ## Asignar el resultado
# .GlobalEnv$meta.vic1[["cat"]][["p4d2"]][["DESCRIP1"]] <- d1
# })

# Después, se emplea la función `separate()` del paquete `tidyr` para dividir texto en las columnas "p4d.eq1" y "p4d.eq2" separadas por "P4d=2". 

# local({
# ## Preparar
# library("tidyr")
# ## Computar
# p  <- meta.vic1[["cat"]][["p4d2"]]
# vars <- c("p4d.eq1","p4d.eq2") 
# p <-   p %>%
#           separate("DESCRIP1", vars, sep = "P4d=2", remove = FALSE)
# ## Asignar el resultado  
# .GlobalEnv$meta.vic1[["cat"]][["p4d2"]] <- p
# })

# Ahora se eliminan espacios con la función `str_squish()` del paquete `stringr` de esas columnas. 

# local({
# ## Preparar
# library("stringr")
# ## Computar
# vars <- c("p4d.eq1","p4d.eq2")
# for (i in vars) {
# eq <- meta.vic1[["cat"]][["p4d2"]][[i]]
# eq <- str_squish(eq)
# ## Asignar el resultado
# .GlobalEnv$meta.vic1[["cat"]][["p4d2"]][[i]]<-eq}
# })
# 
# # Finalmente, se usa la función `gather()` (también de `tidyr`), para transformar en formato "longer" y posteriormente se utiliza la función `ifelse()` para realizar la codificación condicionada.
# 
# local({
# ## Preparar
# library("tidyr")
# ## Computar
# dt_l <- meta.vic1$cat$p4d2 %>% gather(var, etqt, p4d.eq1:p4d.eq2, factor_key=TRUE)
# dt_l$CVR <- ifelse(dt_l$var=="p4d.eq1", dt_l$CVE+10, dt_l$CVE+20) # La función "ifelse()", crea la variable "CVR" con valores únicos si cumple la condición de que "var" sea exactamente igual a "p4d.eq1". Si cumple suma 10 al valor en CVE, sino le suma 20 a esa misma columna.
# dt_l$DESCRIP <- dt_l$etqt # Copia la columna "etqt" en "DESCRIP".
# dt_l$CVE <- dt_l$CVR  # Copia la columna "CVR" en "CVE".
# ## Asignar el resultado
# .GlobalEnv$meta.vic1$cat$p4d2 <- dt_l
# })

## Diccionario de datos

#Importar diccionario de datos. Al final se limpian los nombres con la función `clean_names()` (de `janitor`).

local({
## Preparar
library("rio") 
library("janitor")
## Computar
setwd(file.path(fp$tper_vic1,"diccionario_de_datos", fsep = .Platform$file.sep))
file <- list.files(pattern = "\\.csv$")
#data <- import(file)
data <- iconv.recursive (data, from="latin1")
data <- clean_names (data)
## Asignar el resultado
.GlobalEnv$meta.vic1$dic <- data
})

##Creación del data.frame "noms", para seleccionar variables a convertir en factor.

#Creamos un marco de datos para seleccionar las variables cuyos valores deberán ser etiquetados.

local({     
## Preparar
library("tidyr")
library("tibble")
library("dplyr")
## Computar
noms <- data.frame(cbind("catalogo"=meta.vic1$dic[["nemonico"]],
"rango_claves"=meta.vic1$dic[["rango_claves"]],
"nombre_campo"=meta.vic1$dic[["nombre_campo"]]))
#noms[["catalogo"]] <- replace(noms[["catalogo"]], noms[["catalogo"]]=='', NA)
#noms <- drop_na(noms)
noms <- distinct(noms, catalogo, .keep_all = TRUE)
noms <- rownames_to_column(noms,var="id")
fctrs <- noms[!grepl(',,', noms[["rango_claves"]] ),]
## Asignar el resultado  
.GlobalEnv$meta.vic1$noms <- noms
.GlobalEnv$meta.vic1$fctrs <- fctrs
})

# Variables que se removerán se encuentran en la lista "rem": "r_def","v_sel","n_hog","h_mud","n_ent","n_ren","eda","n_inf","p3f2","p4d3"

# Con especial atención en las variable "p3f2" y "p4d3". Con las etiquetas "Pregunta 3f ¿Más de uno?, ¿Cuántos?" y Pregunta 4d Campo exclusivo del sistema. Que no tienen etiquetas de valor y deben ser filtradas en el data.frame "noms" de la lista "meta.vic1".

local({
library (dplyr)
df <- meta.vic1$fctrs
rem <- list("CVE_ENT", "AP4_12")
df <- df %>%
  filter(!catalogo %in% rem)
.GlobalEnv$meta.vic1$fctrs <- df
})

# 1     r_def	Resultado definitivo de la entrevista
# 4     v_sel	Vivienda Seleccionada
# 5     n_hog	Número de hogar
# 6     h_mud	Hogar mudado
# 7     n_ent	Número de entrevista o visita
# 8     n_ren	Número de renglón
# 9     eda	    Edad
# 10    n_inf	Renglón del Informante
# 68    p3f2    Pregunta 3f ¿Más de uno?, ¿Cuántos?
# 91    p4d3    Exclusivo del sistema

## Importar el conjunto de datos
local({
setwd(file.path(fp$tper_vic1,"conjunto_de_datos", fsep = .Platform$file.sep))
## Computar
#vic1 <- fp$files$vic1
file <- "conjunto_de_datos_tper_vic1_envipe2023.csv"
#file <- paste(vic1,"csv",sep = ".")
data <- read.csv (file=file,  na.strings = "NA", nrows = -1, skip = 0, check.names = TRUE, strip.white = FALSE, blank.lines.skip = TRUE)
# copiar del entorno local a globalenv()
data <- iconv.recursive (data, from="latin1")
.GlobalEnv$meta.vic1[["data"]] <- data
})

library("stringr")
meta.vic1$data  <- mutate_if(meta.vic1$data , is.character, function(x) str_squish(x))

## Selección de variables que requieren etiquetas de valor 
 # Crea el objeto etq.val y se descartan los objetos que ya tienen etiquetas de valor.
                                                    # En este caso nombres de entidad (variable 8) y municipio (variable 9) en este data.frame.
local({
## Preparar
library ("dplyr")
## Computar
df <- meta.vic1$fctrs #Copia el data.frame nomms al objeto df dentro del Ambiente Local.
rem <- list("NOM_MUN","NOM_ENT") # Crea la lista "rem" con los nombres de las variables numéricas que no se convertirán en tipo factor.
df <- df %>%
  filter(!catalogo %in% rem) # Elimina ("!") las filas de la variable "catalogo" que contienen elementos en ("%in%") en la lista "rem".
## Asignar el resultado
.GlobalEnv$meta.vic1$etq.val <- df
})

# Debido a que `R` distingue entre mayúsculas y minúsculas, el tipo de letra en las variables "nemonico" y "catalogo" dentro del marco de datos "dic" debe ser igual a los nombres de las tablas dentro de la lista "cat" y al tipo de letrea en los nombres de las variables dentro del conjunto de datos.
# Es posible convertir mayúsculas o minúsculas según sea el caso para que coincidan los nombres que se encuentran en la lista "cat" y los nombres en el . Se puede utilizar las funciones `str_to_upper( )` o ` str_to_lower( )` del paquete `stringr`. En el caso "3t2017" los nombres de los "data.frames" contenidos dentro de `meta.sdem[["cat"]]`se encuentran en minúsculas. Sin embargo, si fuera necesario, un ejemplo de lo anterior sería:

# library(stringr)
# names(meta.vic1$data) <- str_to_lower(names(meta.vic1$data))

# # Recodificación en la variable "p4d2"
# local({
# p <- with (meta.vic1$data, ((p4d1*10) + p4d2))
# .GlobalEnv$meta.vic1$data$p4d2 <- p
# })


# Transformar las variables que se encuentran en tipo numérico a factores
local({
list_names <- meta.vic1$fctrs[["catalogo"]]
for (i in list_names)  {
f <- meta.vic1$data
f[[i]]<- as.factor(f[[i]])
.GlobalEnv$meta.vic1$data <-f
}
})

# Para etiquetar la variable "p5d_thrs" se requiere corregir la cadena en el diccionario de datos. La cadena que se encuentra dentro de `meta.vic1$dic$nemonico` es "p5d_thr" pero la variable en "data" se llama "p5d_thrs". Para corregirlo sustituimos con la función `str_replace()` (de `stringr`).

# library("stringr")
# meta.vic1$dic$nemonico <- str_replace(meta.vic1$dic$nemonico, "p5d_thr", "p5d_thrs")

# Colocar etiquetas de variable
local({
library("lookup")
d <- meta.vic1$data
list_nem <- meta.vic1$noms$catalogo
for (i in list_nem) 
{
rk.set.label(d[[i]],
vlookup(i, 
meta.vic1$noms,
"catalogo",
"nombre_campo",
nomatch = NA)
)
}
.GlobalEnv$meta.vic1$data <- d
})

# Colocar niveles a las variables de tipo factor.
local({
#library(tidyverse)
library("lookup")
list_names <- meta.vic1[["etq.val"]][["catalogo"]]
d <- meta.vic1$data
for (i in list_names){
f <- meta.vic1$cat[[i]]
v <- d[[i]]
levels(v) <- vlookup(levels(v),
f,
"CVE",
"descrip",
nomatch = NA)
.GlobalEnv$meta.vic1$data[[i]] <- v
}
.GlobalEnv$count.df$vic1  <- .GlobalEnv$meta.vic1$data
})

lapply(count.df, nrow)

# Considerar remover:
# meta.vic1 <- within(meta.vic1, rm(data))

# ### VIC2 ###

## Importar la lista "cat" para tabla "vic2"

local({
## Computar
setwd(file.path(fp$tper_vic2,"catalogos", fsep = .Platform$file.sep))
})


local({
## Preparar
library(rio)
cat_files <- list.files(pattern = "\\.csv$")
.GlobalEnv$meta.vic2 <- list()
.GlobalEnv$meta.vic2$cat <- list()
for (i in cat_files) {
data <- read.csv(i, fileEncoding="latin1")
#data <- iconv.recursive (data, from="latin1")
.GlobalEnv$meta.vic2$cat[[i]] <- data
}
})

# ap6_15_2.csv Error in type.convert.default(data[[i]], as.is = as.is[i], dec = dec,  :   invalid multibyte string at '<85>30
# ap6_20_2.csv,  invalid multibyte string at '<85>30
# ap7_4_08.csv invalid multibyte string at '<bf>cu<e1>nt<61>s veces?'

# En ENOEN, los siguientes archivos están en otra codificación: p6e, p6f, p6g, p6h, p6i. El siguiente loop se ha creado para detectar la codificación de caracteres. No están en ENOE 2017.
#local({
#library("uchardet")
#p6 <- c("p6e.csv", "p6f.csv", "p6g.csv", "p6h.csv", "p6i.csv")
#for(i in p6) {
#v <- read.csv(i, encoding=detect_file_enc(i))
#.GlobalEnv$meta.vic2$cat[[i]] <- v 
#}
#})

# Para limpiar los nombres usamos de la cadena ".csv" de "stringr".

## Preparar
library("stringr")
##Computar
names(meta.vic2$cat) <- str_replace(names(meta.vic2$cat), pattern = ".csv", replacement = "")

#library(stringr)
names(meta.vic2$cat) <- str_to_upper(names(meta.vic2$cat)) ## Revisar si es necesario cambiar de mayúsculas a minúsculas. 


local({
library("stringr")
library("dplyr")
##Computar
l <- meta.vic2[["cat"]]             # Se copia la lista "cat" al objeto "l" en el entorno local, para simplificar la notación.
list_names <- names(l)              # Extramemos los nombres de los objetos que contiene la lista "l" en el objeto "list_names."
for (i in list_names){              # Iniciamos el loop para cada observación "i" en la lista "list_names".
f <- meta.vic2[["cat"]][[i]]
# Extraemos un data.frame de la lista que se corresponde con los nombres enlistados en "list_names".
f  <- mutate_if(f, is.character, function(x) str_squish(x))
f  <- mutate_if(f, is.numeric, function(x) as.character(x))
f$CVE <- f[[i]]                     # Para el marco de datos "f" copiamos la columna de datos homónima del data.frame y le colocamos el nómbre el nombre genérico "CVE".
l[[i]] <- f}                        # Reinsertamos f en la lista l con la nueva variable CVE en el objeto homónimo "i".
## Imprimir el resultado
.GlobalEnv$meta.vic2[["cat"]] <- l #Insertamos la lista con los data.frames modificados en la lista "meta.vic2$$cat" dento del entorno global.
})


## Diccionario de datos "vic2"

local({
## Preparar
library("rio")
library("janitor")
## Computar
setwd(file.path(fp$tper_vic2,"diccionario_de_datos", fsep = .Platform$file.sep))
file <- list.files(pattern = "\\.csv$")
data <- read.csv(file, fileEncoding="latin1")
#data <- import(file)
#data <- iconv.recursive (data, from="latin1")
data <- clean_names (data)
## Asignar el resultado  
.GlobalEnv$meta.vic2$dic <- data
})

## Creación del data.frame "noms", para seleccionar variables a convertir en factor.

# Se crea un marco de datos para seleccionar las variables cuyos valores deberán ser etiquetados.

local({     
## Preparar
library("tidyr")
library("tibble")
library("dplyr")
## Computar
noms <- data.frame(cbind("catalogo"=meta.vic2$dic[["nemonico"]],
"rango_claves"=meta.vic1$dic[["rango_claves"]],
"nombre_campo"=meta.vic2$dic[["nombre_campo"]]))
#noms[["catalogo"]] <- replace(noms[["catalogo"]], noms[["catalogo"]]=='', NA)
#noms <- drop_na(noms)
noms <- distinct(noms, catalogo, .keep_all = TRUE)
noms <- rownames_to_column(noms,var="id")
fctrs <- noms[!grepl(',,', noms[["rango_claves"]] ),]
## Asignar el resultado  
.GlobalEnv$meta.vic2$noms <- noms
.GlobalEnv$meta.vic2$fctrs <- fctrs
})

# Variables que se removerán se encuentran en la lista "rem": "v_sel","n_hog","h_mud","n_ent","n_ren","eda","n_inf","p8_9": 1:8,10,12,14,17,22,25,27,29, 34,36,42,48,54,66,72,79,103,121,123,                                         125,127,129,131,133,135,137,139, 141:145,147:149

local({
## Preparar
library ("dplyr")
## Computar
df <- meta.vic2$fctrs
rem <- list("CVE_ENT","AP6_1_2","AP6_6_01","AP6_6_02","AP6_6_03","AP6_6_04","AP6_10_2","AP6_12_1","AP6_12_2","AP6_12_3","AP6_12_4","AP6_12_6","AP6_12_7","AP6_15_2","AP6_20_2","AP7_4_05","AP7_4_06","AP7_4_07","AP7_4_08","AP7_4_09","AP7_4_10","AP7_4_11","AP7_4_12","AP7_4_13","AP7_4_14","AP7_4_15","FAC_HOG","FAC_ELE","FAC_HOG_AM","FAC_ELE_AM","EST_DIS","UPM_DIS")
df <- df %>%
  filter(!catalogo %in% rem)
## Asignar el resultado  
.GlobalEnv$meta.vic2$fctrs <- df
})




# Se usan los nombres. La variable "p8_9" (col 42) no tiene categorías en los csv y no está en el 2018.
#3  v_sel	Vivienda Seleccionada
#4  n_hog	Número de hogar
#5  h_mud	Hogar mudado
#6  n_ent	Numero de entrevista o visita
#7  n_ren	Número de renglón
#8  eda    Edad
#9  n_inf	Renglón del Informante
#37	p8_9	Pregunta 8 Opción 9: no sabe ###No tiene archivo de categorías

## Importación del conjunto de datos y asignación de metadatos

#Importamos los datos sociodemográficos "vic2"
local({
## Computar
setwd(file.path(fp$tper_vic2,"conjunto_de_datos", fsep = .Platform$file.sep))
vic2 <- fp$files$vic2
file <- "conjunto_de_datos_tper_vic2_envipe2023.csv"
#file <- paste(vic2,"csv",sep = ".")
data <- read.csv (file=file,  na.strings = "NA", nrows = -1, skip = 0, check.names = TRUE, strip.white = FALSE, blank.lines.skip = TRUE, fileEncoding="latin1") #cuidado con la N y el número de año
## Asignar el resultado  
.GlobalEnv$meta.vic2$data <- data
})

library("stringr")
meta.vic2$data  <- mutate_if(meta.vic2$data , is.character, function(x) str_squish(x))

## Selección de variables que requieren etiquetas de valor 
 # Crea el objeto etq.val y se descartan los objetos que ya tienen etiquetas de valor.
                                                    # En este caso nombres de entidad (variable 8) y municipio (variable 9) en este data.frame.
local({
## Preparar
library ("dplyr")
## Computar
df <- meta.vic2$fctrs #Copia el data.frame nomms al objeto df dentro del Ambiente Local.
rem <- list("NOM_MUN","NOM_ENT") # Crea la lista "rem" con los nombres de las variables numéricas que no se convertirán en tipo factor.
df <- df %>%
  filter(!catalogo %in% rem) # Elimina ("!") las filas de la variable "catalogo" que contienen elementos en ("%in%") en la lista "rem".
## Asignar el resultado
.GlobalEnv$meta.vic2$etq.val <- df
})

# Debido a que `R` distingue entre mayúsculas y minúsculas, el tipo de letra en las variables "nemonico" y "catalogo" dentro del marco de datos "dic" debe ser igual a los nombres de las tablas dentro de la lista "cat" y al tipo de letrea en los nombres de las variables dentro del conjunto de datos.
# Es posible convertir mayúsculas o minúsculas según sea el caso para que coincidan los nombres que se encuentran en la lista "cat" y los nombres en el . Se puede utilizar las funciones `str_to_upper( )` o ` str_to_lower( )` del paquete `stringr`. En el caso "3t2017" los nombres de los "data.frames" contenidos dentro de `meta.sdem[["cat"]]`se encuentran en minúsculas. Sin embargo, si fuera necesario, un ejemplo de lo anterior sería:  

# library(stringr)
# names(meta.vic2$data) <- str_to_lower(names(meta.vic2$data))

local({
## Computar
list_names <- meta.vic2$fctrs[["catalogo"]]
for (i in list_names)  {
f <- meta.vic2$data
f[[i]]<- as.factor(f[[i]])
## Asignar el resultado  
.GlobalEnv$meta.vic2$data <-f
}
})

#Ahora se asignan las etiquetas de valor en el formato `RKWard` con la función `rk.set.label()`. Para aplicar la etiqueta correspondiente a cada una de las variables del conjunto de datos, se ha construido un loop con la función `vlookup()` del paquete `lookup`.

local({
## Preparar
library("lookup")
## Computar
d <- meta.vic2$data
for (i in meta.vic2$noms$catalogo) 
{
rk.set.label(d[[i]],
vlookup(i, 
meta.vic2$noms,
"catalogo",
"nombre_campo",
nomatch = NA)
)
}
## Asignar el resultado
.GlobalEnv$meta.vic2$data <- d
})

# Finalmente, se colocan las etiquetas de valor a cada nivel. Esto se logra con la aplicación del resultado a los niveles (con la función `levels()`) de cada una de las variables de factor. 

local({
## Preparar
library("lookup")
## Computar
list_names <- meta.vic2[["etq.val"]][["catalogo"]] 
d <- meta.vic2$data
for (i in list_names){
f <- meta.vic2$cat[[i]]
v <- d[[i]]
levels(v) <- vlookup(levels(v),
f,
"CVE",
"descrip",
nomatch = NA)
## Asignar el resultado
.GlobalEnv$meta.vic2$data[[i]] <- v
}
.GlobalEnv$count.df$vic2  <- .GlobalEnv$meta.vic2$data
})


lapply(count.df, nrow)

###Copiar de vic1 a vic2 r_def
#  Observar que en la tabla "vic2" no existe variable "r_def", si se utiliza como índice para la unión con las otras tablas.

nrow(meta.vic2[["data"]])
count.df$vic2 <- meta.vic2[["data"]]

count.df$sdem[["fac_tri"]] <- count.df$sdem[["fac"]]

### Revisar `[1] 307117` para las tres tablas

lapply(count.df, nrow)

#meta.vic1 <- within(meta.vic1, rm(data))
#meta.vic2 <- within(meta.vic2, rm(data))

#Nota: El campo R_SEL en la tabla TSDEM se identifica
#con la variable N_REN.

count.df$sdem$R_SEL <- count.df$sdem[["N_REN"]]

## Filtrar 
##Recodificar sdem en sdem.ss con la variable "CODIGO %in% 'Informante seleccionado'"

local({
## Computar
.GlobalEnv$count.df[["sdem.ss"]] <- subset(count.df[["sdem"]], subset=CODIGO %in% 'Informante seleccionado')
for(i in 1:length(names(count.df[["sdem.ss"]]))){
	 attr(.GlobalEnv$count.df[["sdem.ss"]][[names(count.df[["sdem.ss"]])[i]]],".rk.meta") = attr(count.df[["sdem"]][[names(count.df[["sdem.ss"]])[i]]],".rk.meta")
}
## Imprimir el resultado
rk.header ("Filtrar datos", parameters=list("Conjunto de datos a filtrar"="count.df[[\"sdem\"]]",
	"Condición de selección"="CODIGO %in% 'Informante seleccionado'"))
})


##Crear una sola tabla con SDEM_PEA, COE1 y COE2 y guardar el resultado.

# Se usa unión "NATURAL" con el paquete `dplyr`.
local({
##Preparar
library("dplyr")
library("lookup")
## Computar
attach(count.df) #Añadir la lista al directorio de búsqueda de `R`.
df <-right_join(sdem, vic1, by=NULL, copy=FALSE) %>% 
      right_join(vic2, by=NULL, copy=FALSE)
 etq <- data.frame(cbind("nemonico"=c("RESUL_H","NOM_ENT","NOM_MUN","SEXO","DOMINIO","AREAM","ESTRATO"),
 "descrip"=c("Resultado de la visita al hogar","Nombre de la Entidad","Nombre del municipio","Sexo",
 "Dominio","Área metropolitana","Estrato sociodemográfico")))
 for (i in etq$nemonico) {
 rk.set.label(df[[i]], vlookup(i, etq, "nemonico", "descrip",nomatch = NA))}
## Asignar el resultado
.GlobalEnv[["sdem_vics.df"]] <- df
detach(count.df)
nrow(sdem_vics.df) # Recuento [1] 307117
})

# Joining with `by = join_by(r_def, cd_a, ent, con, upm, d_sem, n_pro_viv, v_sel,
# n_hog, h_mud, n_ent, per, n_ren, eda, ur, fac)`
# Joining with `by = join_by(cd_a, ent, con, upm, d_sem, n_pro_viv, v_sel, n_hog,
# h_mud, n_ent, per, n_ren, eda, ur, fac, n_inf)`
# Filtrar menores de 15 años

# local({
# ##Preparar
# library("dplyr")
# ## Computar
# .GlobalEnv$sdem_vics_14.df <- filter(sdem_vics.df, EDAD>14) #filtrar menores de 15 años
# nrow(sdem_vics_14.df)
# })

##Revisar `[1] 286294`

# Remover
# rm(sdem_coes.df)

#Ahora se puede guardar el archivo si se obtiene `[1] 286294` del conteo en el marco de datos "sdem_coes_14.df", tras filtrar a los menores de catorce años al directorio en el que se descomprimieron los datos con:

## Guardar
local({
## Computar
save(sdem_vics_14.df,
	file=file.path(fp$dir,"sdem_vics_14.RData",fsep = .Platform$file.sep))
## Imprimir el resultado
rk.header ("Guardar objetos R", parameters=list("Nombre de archivo"=file.path(fp$dir,"sdem_vics_14.RData",fsep = .Platform$file.sep),
	"Objeto"="sdem_vics_14.df"))
})




