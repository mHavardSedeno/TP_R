#TP - Distantiel - Date limite 9 octobre
#1. 
texte <- scan(file = "cyrano.txt", what = "character", n=1000)
#a. Écrire le code permettant d’obtenir la liste des mots d’un fichier texte, dans leur ordre d’apparition.

freq <- table(texte);freq
#PlusFreq <- order(freq, decreasing = TRUE)
PlusFreq <- sort(freq, decreasing = TRUE); PlusFreq
length(PlusFreq)
#b.Écrire le code permettant d’obtenir les 10 mots les plus fréquents du texte.

DixPlusFreq = PlusFreq[1:10]; DixPlusFreq

#c.1 Tracer un (ou plusieurs) graphique montrant que peu de mots accaparent l’essentiel du texte.
#plot(DixPlusFreq, main="Fréquence d'apparition des mots dans un extrait de Cyrano", xlab="Mots", ylab="Ocurence", col = rainbow(5))
#hist(DixPlusFreq, breaks=2, main="Fréquence d'apparition des mots dans un extrait de Cyrano", xlab="Mots", ylab="Ocurence", col = rainbow(5))

#c.2 Écrire le code permettant d’obtenir la matrice FREQ

motAvant <- texte
motApres <- c(motAvant[2:length(texte)],"NA");motApres


FREQ_tbl <- matrix(0, nrow = length(freq), ncol = length(freq));FREQ_tbl

#FreqMotSuivant <- function(motAv,motApr){
#  compt = 0
#  for(i in 1:(length(texte)-1)) {
#    if (texte[i] == motAv & texte[i+1] == motApr){
#      compt = compt + 1
#    }
#  }
#  return(compt)
#}

FreqMotSuivant1 <- function(motAv,motApr){
  compt = 0
  for(i in 1:length(texte)) {
    if (motAvant[i] == motAv & motApres[i] == motApr){
      compt = compt + 1
    }
  }
  return(compt)
}

for (i in 1:length(freq)){
  for (j in 1:length(freq)){
    if (i != j){
      #FREQ[i,j] = FreqMotSuivant(names(freq[i]),names(freq[j]))
      FREQ_tbl[i,j] = FreqMotSuivant1(names(freq[i]),names(freq[j]))
    }
  }
};
FREQ_tbl

#Puis écrire une fonction qui, à partir de FREQ, donne une fonction fournissant le mot le plus vraisemblable
#après un mot donné (ou une chaîne vide si aucune proposition n’est possible).

prochainMot <- function(mot){
  
  for(i in 1:length(freq)) {
    if (names(freq[i]) == mot){
      ligne = i
    }
  }
  

  MotProchain = 0
  
  for(j in 1:length(freq)) {
    if (MotProchain < FREQ_tbl[ligne,j]){
      MotProchain = j
    }
  }
  return(names(freq[MotProchain]))
}

start.time <- Sys.time()
mp <- prochainMot("de");mp
end.time <- Sys.time() - start.time
