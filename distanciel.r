# TP + distanciel 
# Apprentissage 

library(ggplot2)

# Écrire le code permettant d’obtenir la liste des mots d’un fichier texte, dans leur ordre d’apparition

# n = 10 pour fixer le nombre de mots lus
scan_text = scan(file="cyrano.txt", what="character",n=50)
n = length(scan_text)
scan_text[18]

#word_index = data.frame(1:n, scan_text)

scan_text = table(scan_text) 

#text_tb = data.frame(table(scan_text))
text_ord = sort(scan_text, decreasing = TRUE, index.return = TRUE)

premiers = head(text_ord, n=1000)

plot(premiers, main="Fréquence d'apparition des mots dans un extrait de Cyrano", xlab="Mots", ylab="Ocurence", col = rainbow(5))
hist(premiers, breaks=50, main="Fréquence d'apparition des mots dans un extrait de Cyrano", xlab="Mots", ylab="Ocurence", col = rainbow(5))


####### FREQ #########

motav = scan_text
motapr = scan_text
motapr <- c(motapr[2:length(text_ord)], NA)




############ DOC ORDER ##############################################
(ii <- order(x <- c(1,1,3:1,1:4,3), y <- c(9,9:1), z <- c(2,1:9)))
dd <- transform(data.frame(x, y, z),z = factor(z, labels = LETTERS[9:1]))
## Either as above {for factor 'z' : using internal coding}:
dd[ order(x, -y, z), ]
## or along 1st column, ties along 2nd, ... *arbitrary* no.{columns}:
dd[ do.call(order, dd), ]
