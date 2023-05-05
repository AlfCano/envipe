## Vic2
## ENVIPE
#Paquetes requeridos:
# Los paquetes requeridos son: "rio", "stringr", "janitor", "tibble", "lookup".
# Si no los tenemos instalado ejecutamos:
# install.packages(c("rio", "stringr", "janitor", "tibble", "lookup"))
# Este código supone que se usa en la GUI RKward.

##Crear la función auxiliar de importación "iconv.recursive"
iconv.recursive <- function (x, from) { # Creamos la función auxiliar "iconv.recursive" para convertir todas las cadenas a la codificación actual.
	attribs <- attributes (x);
	if (is.character (x)) {
		x <- iconv (x, from=from, to="", sub="")
	} else if (is.list (x)) {
		x <- lapply (x, function (sub) iconv.recursive (sub, from))
	} # Convierte niveles de factor y todos los demás atributos.
	attributes (x) <- lapply (attribs, function (sub) iconv.recursive (sub, from)) #Crea los atributos para todos los elementos de la lista.
	x
} # La función "iconv.recursive" fue elaborada por los contribuidores de RKWard, 
  # y se encuentra disponible en su menú de importación genérica:
  # "Archivo -> Importar -> Formato de Importación -> Importación genérica (basada en rio)"

##Importar los archivo de categorías de datos a la lista "cat"
#Definir el directorio local ".../catalogos"
local({     # Inicia el entorno local para realizar el cómputo.
## Computar 
setwd(      # Selecciona el "Directorio de trabajo".
"") #Establece el "path" absoluto al directorio catalogo entre comillas.
})          # Cierra el entorno local.

local({ # Inicia el entorno local para realizar el cómputo. 
        # Para "{", presenta el resultado de la última expresión evaluada. 
        # Esto tiene la visibilidad de la última evaluación. Como se explica en la ayuda de R, es útil
        # "para crear funciones recursivas anónimas y como una especie de función de espacio de nombres limitado,
        # ya que las variables definidas en el entorno no son visibles desde el exterior" ([Package base version 4.3.0 Index])
library(rio) #Carga el paquete "rio, A Swiss-Army Knife for Data I/O", para realizar la importación.
 # El siguiente es un loop para importar todos los archivos con extensión "csv" del directorio local a una lista.
##Computar
arch_enlist <- list.files(pattern = "\\.csv$") # Enlista los archivos en el directorio con extensión ".csv".
.GlobalEnv$meta.vic2 <- list()     # Crea la lista "meta.vic2" en blanco. Si una lista con el mismo nombre ya existe sus contenidos serán borrados.
.GlobalEnv$meta.vic2$cat <- list() # Crea una nueva la lista en el Ambiente Global (".GlobalEnv") para guardar el resultado del loop subsecuente.
for (i in arch_enlist) {           # Iniciamos el loop para importar los archivos de cada observación "i" en la lista "arch_enlist" .
data <- import(i)                  # Importa la observación "i" en el objeto "data" .
data <- iconv.recursive (data, from= "latin1") # Convierte todas las cadenas de "latin1" a la codificación actual,
                                               # en caso de notar problemas se puede usar algun otro como "UTF-8".
## Asignar el resltado en el entorno global
.GlobalEnv$meta.vic2$cat[[i]] <- data # Asigna el resultado del loop a lista "cat" en el Ambiente Global 
                                      # de cada observación "i" enlistada en  "arch_enlist"  adentro de la lista "meta.vic2".
}                                     # Cierra el loop.
})                                    # Cierra el entorno local.

# Si se detectó que algún archivo no está en latin1 o UTF-8 encoding, 
# podemos usar la función "detect_file_enc" del paquete "uchardet".
# Esta función, permite detectar automáticamente la codificación necesaria.
# library(uchardet)
# meta.vic2$cat$nombredelarchivo.csv <- read.csv('nombredelarchivo.csv',
                                                #encoding=detect_file_enc("nombredelarchivo.csv"))

## Limpiar nombres con stringr
#Para eliminar las extensiones ".csv" de los nombres dentro de la lista "cat" .
library(stringr)        # Cargamos la librería "stringr" para manipular cadenas de caracteres.
names(                  # El resultado se guarda en el vector de los nombres dentro del objeto designado, en este caso será una lista.
meta.vic2$cat           # Especifica el objeto, en este caso la lista "cat" dentro del objeto "mera.vic2".
) <- str_replace(         # Se usa la función "str_replace", con tres argumentos:
     names(               # 1. Accedemos a los nombres que contien el objeto con la función "names".
     meta.vic2$cat),      #    1.1 El objeto cuyos nomres se van a reemplazar.
     pattern = ".csv",    # 2. Especifica el patron a reemplazar dentro de la Cadena (".csv").
     replacement = "")    # 3. Coloca el reemplazo, en este caso está vacío ("").
                                                                                              
# Si es necesario, podemos convertir a mayúsculas (es el caso de los archivos DBF,
# para los csv cuyo continido se encuentra en minúsculas)
# names(cat) <- str_to_upper(names(cat))

##  Limpiar espacios de etiquetas de valor y crear la variable CVE, dentro de cada archivo en la lista "cat"
#   Esta acción permitirá limpiar las etiquetas de valor de espacios en blanco y proveer el mismo nombre para cada variable
#   que contie el índice de búsqueda desde la columna con nombre fijo "CVE", que ahora estará en cada data.frame
#   contenido dentro de la lista "cat".
local({
library(stringr)
##Computar
ct <- meta.vic2[["cat"]]            # Se copia la lista "cat" al objeto "ct" en el entorno local, para simplificar la notación.
lst_nom <- names(ct)                # Extramemos los nombres de los objetos que contiene la lista "ct" en el objeto "lst_nom."
for (i in lst_nom){                 # Iniciamos el loop para cada observación "i" en la lista "lst_nom".
f <- ct[[i]]                        # Extraemos un data.frame de la lista "ct" que se corresponde con los nombres enlistados en "lst_nom".
f$descrip <- str_squish(f$descrip)  # Limpiamos las cadenas en la columna descrip dentro de la lista "cat".
f$CVE <- f[[i]]                     # Para el marco de datos "f" copiamos la columna de datos homónima del data.frame y le colocamos el nómbre el nombre genérico "CVE".
ct[[i]] <- f}                       # Reinsertamos f en la lista l con la nueva variable CVE en el objeto homónimo "i".
## Imprimir el resultado
.GlobalEnv$meta.vic2[["cat"]] <- ct #Insertamos la lista con los data.frames modificados en la lista "meta.vic2$cat" dento del entorno global.
})

##Importación del archivo "diccionario de datos"
# El Diccionario de datos, contiene el data frame para seleccionar las variables con  
# etiquetas de variable y de valor para ser asignadas a los datos.
# Seleccionamos el directorio ".../dicionario_de_datos".)
local({
## Computar
setwd("")
})

##Importar diccionario de datos
local({
## Preparar
library(rio)
library(janitor)                                            # Carga la librería "janitor" para limpiar los nombres con su función "clean_names".
## Computar
data <- import("") # Importa el contenido del archivo "diccionario_de_datos": "diccionario_de_datos_TPer_Vic2_ENVIPE_2022.csv"
data <- iconv.recursive (data, from="latin1")               # Convert todas las cadenas a la codificación "latin1".
data <- clean_names (data)                                  # Aplicamos la función "clean_names" del paquete "janitor" al objeto "data". 
                                                            # Para que convierta los nombres de las de las variables a minúsculas y coincidan 
                                                            # con sus contrapartes en los otros objetos. 
## Imprimir el resultado
.GlobalEnv$meta.vic2$dic <- data                            # Asigna el resultado "data" al entorno ".GlobalEnv" adentro de la lista "meta.vic2".
})


## Con base en el diccionario de datos se crea la lista de nombres de catálogos
#Creamos un marco de datos para seleccionar las variables cuyos valores deberán ser etiquetados.
meta.vic2$nombres <- data.frame(                       # Se usa la función data.frame para crear el marco de datos.
                     cbind(                            # Para colocar las columnas como se encuentran se usa la función.
                     "catalogo"=                       # Colocamos la cadena del nómbre que llevará el primer vector.
                     meta.vic2$dic[["nemonico"]],      # Seleccionamos el vector en este caso "nemonico" en la lista "meta.vic2" y del data.frame "dic".
                     "nombre_campo"=                   # Colocamos la cadena del nómbre que llevará el segundo vector.
                     meta.vic2$dic[["nombre_campo"]])) # Seleccionamos en este caso "nombre_campo" en la lista "meta.vic2" y del data.frame "dic".

# Si el contenido contiene celdas en blanco en el campo catálogo, y sustituirlas por NA con la función "replace"..
#meta.vic2$nombres[["catalogo"]] <- replace(meta.vic2$nombres[["catalogo"]],     # Define el nómbre del vector de búsqueda.
#                                           meta.vic2$nombres[["catalogo"]]=='', # El patrón de búsqueda son todos los valores en el vector de búsqueda está en blanco ('').
#                                           NA)                                  # El resultado de la buscqueda se reemplaza por NA.
# Finalmente, es posible eliminar los NA con la función "drop_na".
#library(tidyr)                                  # Se carga la librería "tidyr".
#meta.vic2$nombres <- drop_na(meta.vic2$nombres) # Se eliminan filas con NA con la función "drop_na".

# Los metadatos ENVIPE contiene valores duplicados en la variable catálogo, para eliminarlo usamos la función "duplicated" y el poerador "!" (not).
meta.vic2$nombres <-meta.vic2$nombres[!duplicated(meta.vic2$nombres$catalogo), ] # Para filtran los valores duplicados por filas se usa la notación
                                                                                 # con corchetes simples de la forma "[FILAS,COLUMNAS]",
                                                                                 # en esta notación se pueden usar números o nombres para acceder a los índices.
library(tibble) #Para crear una viariable de id argamos el paquete tibble.
meta.vic2$nombres  <- rownames_to_column(meta.vic2$nombres ,var="id") #usamos la función "rownames_to_column" para crear la variable "id".



## Crear el objeto factrs
#Este objeto establece la lista de varibles que se convertirán en factores.
meta.vic2$factrs <- meta.vic2$nombres # En primer lugar copiamos el objeto nombres al objeto f.
meta.vic2$factrs <- meta.vic2$factrs[-(c(1:6,8,10,12,14,17,22,25,27,29,
                                        34,36,42,48,54,66,72,79,103,121,123,
                                        125,127,129,131,133,135,137,139,
                                        142:145,147:149)),] #  elimina variables que no llevan etiquetas. El 22 es la clave de carrera cuyo catálogo CSV no tiene nombres

##Importar el archivo csv con el conjunto de datos

# Seleccionamos el directorio ".../conjunto_de_datos".

local({
## Computar
setwd("")
})

# Importamos el archivo csv que contiene el directorio "conjunto_de_datos/"
local({
## Preparar
require(rio)
## Computar
data <- import("") # Importa el contenido del archivo "diccionario_de_datos": "conjunto_de_datos_TPer_Vic2_ENVIPE_2022.csv"
data <- iconv.recursive (data, from="latin1") # Convierte todas las cadenas a la codificación actual "latin1".
                                              # Vic2 está en "latin1". Si se observa que no se 
                                              # respetan los acentos considere que puede estar en en "UTF-8",
                                              # por los nombres de estados y municipios.
## Imprimir el resultado
.GlobalEnv$meta.vic2$data <- data # Asigna el resultado al entorrno global (".GlobalEnv").
})

## Conviertir variables de cadena a factores
local({                                      # Abre el entorno local.
## Computar
lst_nom <- meta.vic2$factrs[["catalogo"]] # Enlista las variables deseadas que se encuentran en la columna catálogo del objeto fctrs.
f <- meta.vic2$data                          # Para simplificar la notación copamos el objeto data al objeto "f" dentro del entorno local.
for (e in lst_nom)  {                        # Para cada observación "e" en la lista "lst_nom", iniciamos el loop.
f[[e]]<- as.factor(f[[e]])                   # Se usa la función "as.factor" cara coercionar cadénas a variables de factor.
}                                            # Cierra el loop.
## Imprimir el resultado
.GlobalEnv$meta.vic2$data <-f                # Asigna el resultado al entorno global (".GlobalEnv").
})  


## Coerciona variables selecccionadas a tipo numérico

meta.vic2[["data"]][,c(10,17,22,25,27,29,                                                            # La función "c" envía el resultado a las variables seleccionadas.
                       34,36,42,48,54,66,72,79,103,121,123,
                       125,127,129,131,133,135,137,139,
                       142:145)] <- sapply(meta.vic2[["data"]][,c(10,17,22,25,27,29,                 # La función sapply aplica la función a cada vactor seleccionado dentro del marco de datos.
                                                                34,36,42,48,54,66,72,79,103,121,123, # La función "c" selecciona las variables a coercionarse a numéricas.
                                                                125,127,129,131,133,135,137,139,     # Lista de variables deseadas, entre ellas edad y factores de expnsión.
                                                                142:145)], as.numeric)               # Función para coercionar un variable a tipo "numric".

## Asignación de etiquetas de variable en RKWard con rk.set.label
# Las etiquetas de variable se asignan después de haber coercionado los objetos a factores y números.
local({
## Preparar
library(lookup)                             # Cargamos la librería lookup.                         
## Computar
df  <- meta.vic2$data                       # Para simplificar la notación copiamos el objeto "data" del entorno global al local en el objeto "df".
l   <- meta.vic2[["nombres"]][["catalogo"]] # Creamos la lista de nombres a asignarles etiquetas, presentes en el catalogo.
etq <- meta.vic2[["nombres"]]               # Copiamos el marco de datos que contiene las etiquetas al entorno local en el objeto "etq".
for (i in l) {                              # Iniciamos el loop para cada observación "i" en el objeto "l"
rk.set.label(df[[i]],                       # Asignamos la etiqueta para cada observación adentro del objeto "df".
vlookup(i,                                  # Usamos la función vlookup para cada observación "i", 
etq,                                        # en el marco de datos "etq"
"catalogo",                                 # busca el objeto en la columna "catalogo",
"nombre_campo"))}                           # y devuelve el valor correspondiente de la columna "nombre_campo".
##Imprimir el resultado
.GlobalEnv$meta.vic2$data <- df             # Finalmente asigna el marco de datos resultante al objeto den el entorno global.
})

                                                                
## Selección de variables que requieren etiquetas de valor    
meta.vic2$etq.val <- meta.vic2$factrs[-(c(4:5)),]  # Crea el objeto etq.val y se descartan los objetos que ya tienen etiquetas de valor.
                                                    # En este caso nombres de entidad (variable 5) y municipio (variable 6) en este data.frame.
## Asignar etiqueta de nivel a cada valor.
local({
## Preparar
library(lookup)
## Computar
l <- meta.vic2$etq.val[["catalogo"]]  # Creamos la lista de nombres a asignarles etiquetas, presentes en el catalogo.
df <- meta.vic2$data                  # Para simplificar la notación copiamos el objeto "data" del entorno global al local en el objeto "df".
for (i in l){
f <- meta.vic2$cat[[i]]               # Copia el objeto "i" en el objeto "f". De este objeto se extraerán las etiquetas de valor.
v <- df[[i]]                          # Toma el vector a etiquetar "i" del objeto "df".
levels(v                              # Coloca el resultado en el vector que corresponde a los niveles (etiquetas de valor) dentro de la variable v.
) <- vlookup(                         # Usamos la función vlookup para cada observación, 
levels(v),                            # en la columna de factor "v" se buca dentro de los niveles asignados,
f,                                    # en el marco de datos f para extraer las etiquetas de valor.
"CVE",                                # busca el objeto en la columna "CVE",
"descrip")                            # y devuelve el valor correspondiente de la columna "descrip".
df[[i]] <- v                          # Posteriormente, copia la variable etiquetada en el marco de datos df con el nombre corresponiente a la observación "i".
}
## Imprimir el resultado
.GlobalEnv$meta.vic2$data <- df       # Finalmente asigna el marco de datos resultante al objeto den el entorno global.
})
