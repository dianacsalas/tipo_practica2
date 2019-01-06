# Carga de librerías
library(ggplot2)
library(stats)
library(cluster)
library(plyr)
library(car)
require(graphics)
library(C50)
library(corrplot)
library(devtools)
library(factoextra)
library(class)

# Carga y visualización del fichero
setwd('C:/RData')
wine <- read.csv('winequality-red.csv', header = TRUE, encoding = 'UTF-8')

# Visualización del fichero y métricas básicas
View(wine)
summary(wine)
str(wine)

# Ejemplo de limpieza de valores nulos, sustituyendolos por el valor de la mediana
wine$fixed.acidity[is.na(wine$fixed.acidity)]<-median(wine$fixed.acidity,na.rm=TRUE)

# Representación de valores extremos y métricas básicas: boxplots
par( mfrow=c(1,3))
b1<-boxplot(wine$fixed.acidity, main="Fixed Acitidity", col="gray")
b2<-boxplot(wine$volatile.acidity, main="Volatile Acitidity", col="gray")
b3<-boxplot(wine$citric.acid, main="Citric Acid", col="gray")
b4<-boxplot(wine$residual.sugar, main="Residual Sugar", col="gray")
b5<-boxplot(wine$chlorides, main="Chlorides", col="gray")
b6<-boxplot(wine$free.sulfur.dioxide, main="Free Sulfur Dioxide", col="gray")
b7<-boxplot(wine$total.sulfur.dioxide, main="Total Sulfur Dioxide", col="gray")
b8<-boxplot(wine$density, main="Density", col="gray")
b9<-boxplot(wine$pH, main="Free Sulfur Dioxide", col="gray")
b10<-boxplot(wine$sulphates, main="Sulphates", col="gray")
b11<-boxplot(wine$alcohol, main="Alcohol", col="gray")

# Función que escribe la desviación estándar y el valor donde empiezan los outliers
stdesv_y_outliers <- function(atributo) {
  stdesv = sd(atributo)
  out = stdesv * 3.5 + median(atributo)
  desvio = out/median(atributo)
  print(paste0('Desviación estándar: ', stdesv))
  print(paste0('Outlier más pequeño: ', out))
  print(paste0('Número de veces de desvío out/mediana: ', desvio))
}

# Analizamos los outliers más a fondo
stdesv_y_outliers(wine$fixed.acidity)
stdesv_y_outliers(wine$volatile.acidity)
stdesv_y_outliers(wine$citric.acid)
stdesv_y_outliers(wine$residual.sugar)
stdesv_y_outliers(wine$chlorides)
stdesv_y_outliers(wine$free.sulfur.dioxide)
stdesv_y_outliers(wine$total.sulfur.dioxide)
stdesv_y_outliers(wine$density)
stdesv_y_outliers(wine$pH)
stdesv_y_outliers(wine$sulphates)
stdesv_y_outliers(wine$alcohol)

par( mfrow=c(1,1))
hist(wine$total.sulfur.dioxide,main='Total sulfur dioxide', col = 'blue')
hist(wine$free.sulfur.dioxide,main='Free sulfur dioxide', col = 'blue')
hist(wine$chlorides,main='Chlorides', col = 'blue')
hist(wine$residual.sugar,main='Residual sugar', col = 'blue')
hist(wine$sulphates,main='Sulphates', col = 'blue')

# Eliminando outliers del dataset
outliers_rs<-b4$out
outliers_chl<-b5$out
outliers_fsd<-b6$out
outliers_tsd<-b7$out
outliers_sul<-b10$out

wine_clean <- wine
wine_clean <- wine_clean[-which(wine_clean$residual.sugar %in% outliers_rs),]
wine_clean <- wine_clean[-which(wine_clean$chlorides %in% outliers_chl),]
wine_clean <- wine_clean[-which(wine_clean$free.sulfur.dioxide %in% outliers_fsd),]
wine_clean <- wine_clean[-which(wine_clean$total.sulfur.dioxide %in% outliers_tsd),]
wine_clean <- wine_clean[-which(wine_clean$sulphates %in% outliers_sul),]

hist(wine_clean$total.sulfur.dioxide,main='Total sulfur dioxide', col = 'green')
hist(wine_clean$free.sulfur.dioxide,main='Free sulfur dioxide', col = 'green')
hist(wine_clean$chlorides,main='Chlorides', col = 'green')
hist(wine_clean$residual.sugar,main='Residual sugar', col = 'green')
hist(wine_clean$sulphates,main='Sulphates', col = 'green')

# Separamos en conjunto de entrada y de salida
X <- wine_clean[,1:11]
y <- wine_clean[,12]

XX <- wine[,1:11]
yy <- wine[,12]

#Comprobación de la normalidad
shap_test_wine_clean <- lapply(wine_clean, shapiro.test)
shap_test_wine_clean[[1]]
shap_test_wine_clean[[2]]
shap_test_wine_clean[[3]]
shap_test_wine_clean[[4]]
shap_test_wine_clean[[5]]
shap_test_wine_clean[[6]]
shap_test_wine_clean[[7]]
shap_test_wine_clean[[8]]
shap_test_wine_clean[[9]]
shap_test_wine_clean[[10]]
shap_test_wine_clean[[11]]
shap_test_wine_clean[[12]]

shap_test_wine <- lapply(wine, shapiro.test)
shap_test_wine[[1]]
shap_test_wine[[2]]
shap_test_wine[[3]]
shap_test_wine[[4]]
shap_test_wine[[5]]
shap_test_wine[[6]]
shap_test_wine[[7]]
shap_test_wine[[8]]
shap_test_wine[[9]]
shap_test_wine[[10]]
shap_test_wine[[11]]
shap_test_wine[[12]]

#Comprobación de la homogeneidad de la varianza

bartlett.test(wine_clean$fixed.acidity~wine_clean$quality, wine_clean)
bartlett.test(wine_clean$volatile.acidity~wine_clean$quality, wine_clean)
bartlett.test(wine_clean$citric.acid~wine_clean$quality, wine_clean)
bartlett.test(wine_clean$residual.sugar~wine_clean$quality, wine_clean)
bartlett.test(wine_clean$chlorides~wine_clean$quality, wine_clean)
bartlett.test(wine_clean$free.sulfur.dioxide~wine_clean$quality, wine_clean)
bartlett.test(wine_clean$total.sulfur.dioxide~wine_clean$quality, wine_clean)
bartlett.test(wine_clean$density~wine_clean$quality, wine_clean)
bartlett.test(wine_clean$pH~wine_clean$quality, wine_clean)
bartlett.test(wine_clean$sulphates~wine_clean$quality, wine_clean)
bartlett.test(wine_clean$alcohol~wine_clean$quality, wine_clean)

bartlett.test(wine$fixed.acidity~wine$quality, wine)
bartlett.test(wine$volatile.acidity~wine$quality, wine)
bartlett.test(wine$citric.acid~wine$quality, wine)
bartlett.test(wine$residual.sugar~wine$quality, wine)
bartlett.test(wine$chlorides~wine$quality, wine)
bartlett.test(wine$free.sulfur.dioxide~wine$quality, wine)
bartlett.test(wine$total.sulfur.dioxide~wine$quality, wine)
bartlett.test(wine$density~wine$quality, wine)
bartlett.test(wine$pH~wine$quality, wine)
bartlett.test(wine$sulphates~wine$quality, wine)
bartlett.test(wine$alcohol~wine$quality, wine)

# Vemos la matriz de correlaciones
cor_matrix_X <- cor(X)
corrplot(cor_matrix_X, type = "upper", order = "alphabet", tl.col = "black", tl.srt = 45)

cor_matrix_XX <- cor(XX)
corrplot(cor_matrix_XX, type = "upper", order = "alphabet", tl.col = "black", tl.srt = 45)

# Aplicamos PCA escalando datos
pca <- prcomp(X,center = TRUE,scale. = TRUE) 
print(pca)
summary(pca)
biplot(pca)
fviz_pca_var(pca,col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
X_pca <- pca$x[,1:6]

# Conjuntos de train (2/3 de filas) y de test (1/3 de filas)
total_rows <- nrow(X_pca)
train_rows <- round(total_rows*(2/3),0)
X_pca_train <- X_pca[1:train_rows,]
X_pca_test <- X_pca[(train_rows+1):total_rows,]
y_train <- y[1:train_rows]
y_test <- y[(train_rows+1):total_rows]

total_rows <- nrow(XX)
train_rows <- round(total_rows*(2/3),0)
XX_train <- XX[1:train_rows,]
XX_test <- XX[(train_rows+1):total_rows,]
yy_train <- yy[1:train_rows]
yy_test <- yy[(train_rows+1):total_rows]

# Representación del vector de calidad
hist(y,main='quality - datos tratados', col = 'yellow')
hist(yy,main='quality - datos originales', col = 'yellow')

# Escalamos los datos
X_pca_train <- scale(X_pca_train)
X_pca_test <- scale(X_pca_test)
XX_train <- scale(XX_train)
XX_test <- scale(XX_test)

# Generamos los modelos K-nn
## X_pca_train, y_train, X_pca_test, y_test: encontrar el mejor k-valor
i = 1                     
k_optimo = 1                    
for (i in 1:100){ 
  knn_mod <-  knn(train = X_pca_train, test = X_pca_test, cl = y_train, k = i)
  k_optimo[i] <- 100 * sum(y_test == knn_mod)/length(y_test)
  k=i
}
plot(k_optimo, type="b", xlab='k-valor',ylab='accuracy')

## Generamos el modelo y comprobamos precision
knn <-  knn(train = X_pca_train, test = X_pca_test, cl = y_train, k = 36)
acc <- 100 * sum(y_test == knn)/length(y_test)
acc

## Graficamos resultados
y_test_f<-as.factor(y_test)
summary(knn)
summary(y_test_f)
barplot(summary(knn),main='Clasificaciones knn')
barplot(summary(y_test_f),main='Clasificaciones originales')

## XX_train, yy_train, XX_test, yy_test
i = 1                     
k_optimo = 1                    
for (i in 1:100){ 
  knn_mod <-  knn(train = XX_train, test = XX_test, cl = yy_train, k = i)
  k_optimo[i] <- 100 * sum(yy_test == knn_mod)/length(yy_test)
  k=i
}
plot(k_optimo, type="b", xlab='k-valor',ylab='accuracy')

## Generamos el modelo y comprobamos precision
knn_orig <-  knn(train = XX_train, test = XX_test, cl = yy_train, k = 18)
acc_orig <- 100 * sum(yy_test == knn_orig)/length(yy_test)
acc_orig

## Graficamos resultados
yy_test_f<-as.factor(yy_test)
summary(knn_orig)
summary(yy_test_f)
barplot(summary(knn_orig),main='Clasificaciones knn dataset original')
barplot(summary(yy_test_f),main='Clasificaciones originales')

# Matriz de correlación para calidades 5 y 6
q<-c(5,6)
wine_q <- wine_clean[which(wine_clean$quality %in% q),]
cor_matrix_q <- cor(wine_q)
corrplot(cor_matrix_q, type = "upper", order = "alphabet", tl.col = "black", tl.srt = 45)

# Exportamos los ficheros
wine_clean_exp<-cbind(X_pca,quality=y)
wine_clean_out<-cbind(X_pca_test,quality = y_test,pred = knn)
wine_out<-cbind(XX_test,quality = yy_test,pred = knn_orig)

write.csv(wine_out, file = "winequality-red-out.csv",row.names=FALSE)
write.csv(wine_clean_out, file = "winequality-red-clean-out.csv",row.names=FALSE)
write.csv(wine_clean_exp, file = "winequality-red-clean.csv",row.names=FALSE)