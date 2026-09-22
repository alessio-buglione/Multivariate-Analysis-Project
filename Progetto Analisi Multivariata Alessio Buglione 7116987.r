# ESERCIZIO 1 

# CARICAMENTO DATI E ELIMINAZIONE VARIABILE SCORE

hept <- read.table("C:/Desktop/heptathlon.csv", head = TRUE, sep = ",", dec = ".")
str(hept)

X <- hept[, -which(names(hept) == "score")]
str(X)


# SEPARAZIONE DATI QUANTITATIVI

# Etichette delle osservazioni (atlete)
labeloss <- X$athlete

# Matrice dei dati quantitativi
Xnum <- X[, -1]
Xnum <- as.data.frame(Xnum)

str(Xnum)

# Dimensioni
n <- nrow(Xnum)
p <- ncol(Xnum)


# a) ANALISI DESCRITTIVE


# Numeri di sintesi
summary(Xnum)

# Vettore delle medie
colMeans(Xnum)

# Matrice di varianze-covarianze
cov(Xnum)

# Matrice di correlazione
cor(Xnum)

# RAPPRESENTAZIONI GRAFICHE

# Scatterplot matrix
pairs(Xnum)

# STANDARDIZZAZIONE DEI DATI

# Standardizzazione (media 0, varianza 1)
Z <- scale(Xnum)
Z <- as.matrix(Z)


rownames(Z) <- labeloss

# Boxplot accostati post standardizzazione

boxplot(Z, main= "Boxplot delle discipline")

# Controlli
round(colMeans(Z), 6)
cov(Z)          # coincide con la matrice di correlazione



# PCA – AUTOVALORI E AUTOVETTORI


# PCA sulla matrice di correlazione
e <- eigen(cov(Z))

# Autovalori
eval <- e$values
eval

# Autovettori
evec <- e$vectors

# Proporzione di varianza spiegata
eval / sum(eval)

# Varianza cumulata
cumsum(eval / sum(eval))

# Scree plot
plot(eval, 
     type = "b", main = "Scree plot – PCA heptathlon", 
     xlab="Numero componenti principali", ylab="Varianze")



# COMPONENTI PRINCIPALI CAMPIONARIE


# Componenti principali: Y = Z * A
PC <- Z %*% evec
head(PC)

# Correlazione tra variabili originarie e CP
cor(Xnum, PC[,1:2])

# CERCHIO DELLE CORRELAZIONI

# Proiezione delle variabili (relazione di dualità)
provar <- evec %*% sqrt(diag(eval))

# Cerchio unitario
ucircle <- cbind(cos(0:360/180*pi),
                 sin(0:360/180*pi))

plot(ucircle, type = "l", col = "blue", asp=1,
     xlab = "Componente principale 1", ylab = "Componente principale 2", 
     xlim = c(-1, 1),
     ylim = c(-1, 1),
     main = "Cerchio delle correlazioni")
text(provar[,1:2], labels = colnames(Xnum))
abline(h = 0, v = 0)

# PROIEZIONE DELLE ATLETE

plot(PC[,1:2],
     xlab = "CP1", ylab = "CP2",
     main = "Proiezione delle atlete sulle prime 2 CP")
text(PC[,1:2], labels = labeloss, cex = 0.6)
abline(h = 0, v = 0)

# c) CLUSTERING GERARCHICO

# Distanza euclidea sui dati standardizzati
d <- dist(Z)

# Metodi di aggregazione
hcSingle   <- hclust(d, method = "single")
hcComplete <- hclust(d, method = "complete")
hcCentroid <- hclust(d, method = "centroid")
hcWard     <- hclust(d, method = "ward")

# Dendrogrammi
plot(hcSingle,   main = "Legame singolo")
plot(hcComplete,main = "Legame completo")
plot(hcCentroid,main = "Metodo del centroide")
plot(hcWard,    main = "Metodo di Ward")


# d) CRITERIO DEL MASSIMO SALTO – METODO DI WARD


# Altezze di fusione
#Hs <- hcWard$height
Hs

# Differenze successive
#HHs <- c(0, Hs[-length(Hs)])
#diffs <- Hs - HHs
#diffs

# Individuazione massimo salto
#index <- which.max(diffs)
#index

# Taglio del dendrogramma
#plot(hcWard,
#     main = "Metodo di Ward – massimo salto")
#rect.hclust(hcWard, h = Hs[index])

# d) CRITERIO DEL MASSIMO SALTO – TUTTI I METODI

criterio_massimo_salto <- function(hc, titolo = "") {
  Hs <- hc$height
  diffs <- diff(c(0, Hs))              # differenze successive
  index <- which.max(diffs)            # posizione massimo salto
  h_cut <- Hs[index]                   # altezza di taglio
  
  plot(hc, main = paste0(titolo, " – massimo salto"), cex = 0.6)
  rect.hclust(hc, h = h_cut)
  
  # Ritorno info utili (se vuoi stamparle)
  return(list(Hs = Hs, diffs = diffs, index = index, h_cut = h_cut))
}

# Applico il criterio ai 4 dendrogrammi
res_single   <- criterio_massimo_salto(hcSingle,   "Legame singolo")
res_complete <- criterio_massimo_salto(hcComplete, "Legame completo")
res_centroid <- criterio_massimo_salto(hcCentroid, "Metodo del centroide")
res_ward     <- criterio_massimo_salto(hcWard,     "Metodo di Ward")


-------------------------------------------------------------------------------


# ESERCIZIO 2 

# CARICAMENTO DATI


wine <- read.table("C:/Desktop/wine.csv", head = TRUE, sep = ",", dec=".")
str(wine)


# SEPARAZIONE DATI QUANTITATIVI

# g = variabile qualitativa di gruppo (3 tipologie di vino)
# labeloss = etichette delle osservazioni (se non esiste una colonna-nome, la costruisco)

g <- wine[, 1]                 # prima colonna = gruppo
g <- as.factor(g)              # la tratto come fattore

labeloss <- paste0("obs", 1:nrow(wine), "_", g)   # etichette osservazioni
labeloss[1:10]


# MATRICE DATI QUANTITATIVA

# X = matrice n x p con sole variabili quantitative

X <- wine[, -1]
X <- as.data.frame(X)

# Controllo: tutte numeriche
str(X)

# Dimensioni
n <- nrow(X)
p <- ncol(X)
G <- length(levels(g))


# a) ANALISI DESCRITTIVE + GRAFICI

summary(X)
colMeans(X)
cov(X)
cor(X)


# STANDARDIZZAZIONE (consigliata: scale diverse)

Z <- scale(X)
Z <- as.matrix(Z)

round(colMeans(Z), 6)
cov(Z)     # ~ matrice di correlazione dei dati originali

boxplot(Z, main="Boxplot variabili wine")
pairs(Z)

# b) MATRICI DI DEVIANZA: T, W, B

# Notazioni prof:
# T = devianza totale = (n-1) * cov(Z)
# W = devianza within = somma_k (n_k - 1) * cov(Z_k)
# B = devianza between = T - W

# Devianza totale
T <- (n - 1) * cov(Z)
T

# Devianza within
W <- matrix(0, p, p)
for (k in levels(g)) {
  Zk <- Z[g == k, , drop = FALSE]
  nk <- nrow(Zk)
  W <- W + (nk - 1) * cov(Zk)
}
W

# Devianza between
B <- T - W
B

# Verifica: T = W + B
round(T - (W + B), 6)


# c) REGOLA DISCRIMINANTE LINEARE DI FISHER

# Fisher: massimizza (a' B a) / (a' W a)
# => autovalori/autovettori di W^{-1} B

WB <- solve(W) %*% B
eig <- eigen(WB)

eig$values        # potere discriminante (ordinati decrescenti)
A <- eig$vectors  # direzioni discriminanti

# Funzioni discriminanti (proiezioni)
Y <- Z %*% A[, 1:2]
head(Y)

# Grafico sul piano delle prime due funzioni discriminanti
plot(Y,
     col = as.integer(g), pch = 19,
     xlab = "Funzione discriminante 1",
     ylab = "Funzione discriminante 2",
     main = "Fisher – proiezione sulle prime 2 funzioni")
text(Y, labels = labeloss, cex = 0.55, pos = 3)
legend("topright", legend = levels(g), col = 1:G, pch = 19)


# c) CLASSIFICAZIONE: LDA (regola lineare) + errori

# Confronto dell’errore di classificazione su:
# - errore apparente (training)
# - errore LOOCV (leave-one-out), se richiesto/utile

library(MASS)

# Fit LDA sui dati standardizzati
dat <- data.frame(g = g, Z)
lda_fit <- lda(g ~ ., data = dat)
lda_fit

# Predizione (errore apparente)
pred <- predict(lda_fit)$class
tab <- table(Osservato = g, Predetto = pred)
tab

err_app <- 1 - sum(diag(tab)) / sum(tab)
err_app
1 - err_app   # accuracy

# LOOCV (stima più realistica dell’errore)
lda_cv <- lda(g ~ ., data = dat, CV = TRUE)
pred_cv <- lda_cv$class
tab_cv <- table(Osservato = g, Predetto = pred_cv)
tab_cv

err_cv <- 1 - sum(diag(tab_cv)) / sum(tab_cv)
err_cv
1 - err_cv

# Y è la matrice delle funzioni discriminanti: Y[,1] = LD1
ld1 <- as.numeric(Y[,1])

oldpar <- par(no.readonly = TRUE)
par(mfrow = c(length(levels(g)), 1), mar = c(2.5, 4, 2, 1), oma = c(3, 0, 0, 0))

xlim_all <- range(ld1)

for (k in levels(g)) {
  hist(ld1[g == k],
       breaks = 12,
       freq = FALSE,            # densità (come in figura)
       xlim = xlim_all,
       main = paste("Gruppo", k),
       xlab = "",
       ylab = "densità",
       border = "white")
  rug(ld1[g == k])              # tacchettine sotto (facoltative)
}

mtext("LD1 (funzione discriminante 1)", side = 1, outer = TRUE, line = 1)
par(oldpar)

boxplot(ld1 ~ g,
        col = c("grey30", "grey60", "grey80"),
        xlab = "Tipo di vino",
        ylab = "LD1 (funzione discriminante 1)",
        main = "Distribuzione dei gruppi sulla prima funzione discriminante")





















