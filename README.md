# Import and merge envipe into R with labels
Scripts to import "Datos abiertos" (Open data) and merge tables of the Encuesta Nacional de Victimización y Percepción sobre Seguridad Pública (ENVIPE) del Instituto Nacional de Estadística y Geografía (INEGI) to the R native environment. These scripts where written within the RKWard GUI, and expect functions from the rkward R package.
## Introduction
Even though it is possible to download INEGI data frames with the importinegi R package or to download the "*.Rdata*" package, none of these options provides labels nor level labels for factor type objects. A way to get those is to import SPSS ("*.sav*") or STATA ("*.dta*") files, but those are not native R formats this leads to miss labeled data and duplicated labels when strings of the labels are not clean.

These scripts aim to provide a way to import and format those datasets with proper clean labels for variables and levels for factor type objects. This is achieved through the “Datos abiertos” (Open data) data set, which includes complete files of each category and keys for each variable. It also provides a dictionary to specify each variable and establish variable labels.

The datasets can be downloaded form:
https://www.inegi.org.mx/programas/envipe/

The 2022 set is available through:
https://www.inegi.org.mx/programas/envipe/2022/#Datos_abiertos
