library(tidyverse)

liste_liens <-  c("https://www.insee.fr/fr/statistiques/fichier/4190491/Deces_2020.zip",
                  "https://www.insee.fr/fr/statistiques/fichier/4190491/Deces_2019.zip",
                  "https://www.insee.fr/fr/statistiques/fichier/4190491/Deces_2018.zip",
                  "https://www.insee.fr/fr/statistiques/fichier/4769950/deces-2010-2018-csv.zip",
                  "https://www.insee.fr/fr/statistiques/fichier/4769950/deces-2000-2009-csv.zip",
                  "https://www.insee.fr/fr/statistiques/fichier/4769950/deces-1990-1999-csv.zip",
                  "https://www.insee.fr/fr/statistiques/fichier/4769950/deces-1980-1989-csv.zip",
                  "https://www.insee.fr/fr/statistiques/fichier/4769950/deces-1970-1979-csv.zip")

telechargement_insee <- function(lien){
  download.file(lien,"donnees/site_insee/temp.zip")
  
  fichier_zip <- "donnees/site_insee/temp.zip"
  outDir<-"donnees/site_insee"
  unzip(fichier_zip,exdir=outDir)  
}

walk(liste_liens,telechargement_insee)

noms_initiaux <- list.files(path="donnees/site_insee/",
                             pattern=".csv")
noms_ok <- paste0(str_replace(noms_initiaux,"-","_"))
  
file.rename(paste0("donnees/site_insee/",noms_initiaux),
            paste0("donnees/site_insee/",noms_ok))

