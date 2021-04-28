library(readr)
loan <- read_csv("~/Loan_Home.csv")
View(loan)

y_original = loan$Loan_Status
x1_original = loan$Gender
x2_original = loan$Married
x3_original = loan$Dependents
x4_original = loan$Education
x5_original = loan$Self_Employed
x6_original = loan$ApplicantIncome
x7_original = loan$CoapplicantIncome
x8_original = loan$LoanAmount
x9_original = loan$Loan_Amount_Term
x10_original = loan$Credit_History
x11p_original = loan$Property_Area

####TRANSFORMACION VARIABLES CUALITATIVAS############


#1 para Prestamo aprobado 0 para pr?stamo no aprobado

y_original = (y_original=='Y')*1

#0 para mujeres y 1 para hombres.

x1_original = (x1_original=='Male')*1

#1 para Casados 0 para no casados.

x2_original = (x2_original=='Yes')*1

#Intercambio de variables de x3 (Dependientes) a un número aleatorio entre 3 y 5
set.seed(1)# semilla establecida para que los datos aleatorios no cambien.
nuevox3 = sample(3:5,length(x3_original),replace = T) #datos aleatorios de las observaciones

for(l in 1:nrow(loan)){
  
  if(is.na(loan$Dependents[l])){
    loan$Dependents[l] = 6
  }
  if(loan$Dependents[l] == '3+'){
    loan$Dependents[l] = nuevox3[l]
  }
}
loan$Dependents = as.numeric(loan$Dependents)
x3_original = loan$Dependents



#1 para Graduados 0 para no Graduados.

x4_original = (x4_original=='Graduate')*1

#1 es trabajador independiente 0 NO es trabajador independiente. 

x5_original = (x5_original=='Yes')*1


#Area de la propiedad
#Urbano         x11=  0 x12 = 0
#Semi-Urbano    x11 = 1 x12 = 0
#Rural          x11 = 0 x12 = 1

x11_original = (x11p_original=='Semiurban')*1
x12_original = (x11p_original=='Rural')*1



#Eliminacion de los NA
loan1 =  loan[! is.na(loan$Gender),] # Sacar media 
loan2 =  loan1[! is.na(loan1$Married),] # Sacar media 
loan3 =  loan2[! loan2$Dependents==6,]  # Sacar media por correlacion muy baja.
loan4 =  loan3[! is.na(loan3$Self_Employed),] # Sacar media 
loan5 =  loan4[! is.na(loan4$LoanAmount),] # ecuacion de regresión con x6
loan6 =  loan5[! is.na(loan5$Loan_Amount_Term),] # Sacar media por correlacion muy baja con otras variables
loan7 =  loan6[! is.na(loan6$Credit_History),] # Sacar media 

y = loan7$Loan_Status
x1 = loan7$Gender
x2 = loan7$Married
x3d = loan7$Dependents
x4 = loan7$Education
x5 = loan7$Self_Employed
x6 = loan7$ApplicantIncome
x7 = loan7$CoapplicantIncome
x8 = loan7$LoanAmount
x9 = loan7$Loan_Amount_Term
x10 = loan7$Credit_History
x11p = loan7$Property_Area


####TRANSFORMACION VARIABLES CUALITATIVAS############


#1 para Prestamo aprobado 0 para prestamo no aprobado

y = (y=='Y')*1

#0 para mujeres y 1 para hombres.

x1 = (x1=='Male')*1

#1 para Casados 0 para no casados.

x2 = (x2=='Yes')*1

#1 para Graduados 0 para no Graduados.

x4 = (x4=='Graduate')*1

#1 es trabajador independiente 0 NO es trabajador independiente. 

x5 = (x5=='Yes')*1


#Area de la propiedad
#Urbano         x11=  0 x12 = 0
#Semi-Urbano    x11 = 1 x12 = 0
#Rural          x11 = 0 x12 = 1

x11 = (x11p=='Semiurban')*1
x12 = (x11p=='Rural')*1


#Matriz de correlación de las variables cualitativas
mm = cbind(x3d,x6,x7,x8,x9)
correlac = cor(mm)

library("corrplot")
corrplot(correlac)

#Creacion de nuevo dataSET Limpio

loanfinal = loan

##
y_original = loanfinal$Loan_Status
x1_original = loanfinal$Gender
x2_original = loanfinal$Married
x3_original = loanfinal$Dependents
x4_original = loanfinal$Education
x5_original = loanfinal$Self_Employed
x6_original = loanfinal$ApplicantIncome
x7_original = loanfinal$CoapplicantIncome
x8_original = loanfinal$LoanAmount
x9_original = loanfinal$Loan_Amount_Term
x10_original = loanfinal$Credit_History
x11p_original = loanfinal$Property_Area
##

y_original = (y_original=='Y')*1

#0 para mujeres y 1 para hombres.

x1_original = (x1_original=='Male')*1

#1 para Casados 0 para no casados.

x2_original = (x2_original=='Yes')*1

#1 para Graduados 0 para no Graduados.

x4_original = (x4_original=='Graduate')*1

#1 es trabajador independiente 0 NO es trabajador independiente. 

x5_original = (x5_original=='Yes')*1


#Area de la propiedad
#Urbano         x11=  0 x12 = 0
#Semi-Urbano    x11 = 1 x12 = 0
#Rural          x11 = 0 x12 = 1

x11_original = (x11p_original=='Semiurban')*1
x12_original = (x11p_original=='Rural')*1

###Reemplazo valores NA de la variable x10 (Historial Crediticio).

# Credit_History
for(i in 1:nrow(loanfinal))
  if(is.na(loanfinal$Credit_History[i])){
    loanfinal$Credit_History[i] = round(mean(x10))
    x10_original[i] = round(mean(x10))
  }

# Gender
for(i in 1:nrow(loanfinal))
  if(is.na(loanfinal$Gender[i])){
    loanfinal$Gender[i] = round(mean(x1))
    if(loanfinal$Gender[i]=='1'){
      loanfinal$Gender[i]='Male'
    }else{
      loanfinal$Gender[i]='Female'
    }
    x1_original[i] = round(mean(x1)) 
    
  }

#Married

for(i in 1:nrow(loanfinal))
  if(is.na(loanfinal$Married[i])){
    loanfinal$Married[i] = round(mean(x2))
    if(loanfinal$Married[i]=='1'){
      loanfinal$Married[i]='Yes'
    }else{
      loanfinal$Gender[i]='No'
    }
    x2_original[i] = round(mean(x2)) 
    
  }

#Dependents
for(i in 1:nrow(loanfinal))
  if(loanfinal$Dependents[i]==6){
    loanfinal$Dependents[i] = round(mean(x3d))
    x3_original[i] = round(mean(x3d))
  }


# Self_Employed

for(i in 1:nrow(loanfinal))
  if(is.na(loanfinal$Self_Employed[i])){
    loanfinal$Self_Employed[i] = round(mean(x5))
    if(loanfinal$Self_Employed[i]=='1'){
      loanfinal$Self_Employed[i]='Yes'
    }else{
      loanfinal$Self_Employed[i]='No'
    }
    x5_original[i] = round(mean(x5))
    
  }

# loan_Amount_Term

for(i in 1:nrow(loanfinal))
  if(is.na(loanfinal$Loan_Amount_Term[i])){
    loanfinal$Loan_Amount_Term[i] = round(round(mean(x9))/12)*12
    x9_original[i] = round(round(mean(x9))/12)*12
  }

# loan_amount


###Reemplazo valores NA de la variable x8 (Monto del prestamo).
reg2 = lm(x8~x6)
summary(reg2)
#yest2 = reg2$coefficients[1]+reg2$coefficients[2]*x6_original

for(i in 1:nrow(loanfinal))
  if(is.na(loanfinal$LoanAmount[i])){
    loanfinal$LoanAmount[i] = round(reg2$coefficients[1]+reg2$coefficients[2]*x6_original[i])
    
    x8_original[i]= loanfinal$LoanAmount[i]
  }

View(loanfinal)


##########################REGLA DE ORO###################################

set.seed(2)# semilla establecida para que los datos aleatorios no cambien.
aleatorio = sample(614,replace = F) #datos aleatorios de las observaciones
#Se obtiene la matriz aleatoria de los datos. 
trainrandom = loanfinal[c(aleatorio),] #Se coloca la matriz con números de filas aleatorizadas.

##########################TRAINING#######################################
training =trainrandom[1:430,]

#Creamos las variables de entrenamiento para el modelo de regresion logistica
yE = training$Loan_Status
x1E = training$Gender
x2E = training$Married
x3E = training$Dependents
x4E = training$Education
x5E = training$Self_Employed
x6E = training$ApplicantIncome
x7E = training$CoapplicantIncome
x8E = training$LoanAmount
x9E = training$Loan_Amount_Term
x10E = training$Credit_History
x11_E = training$Property_Area

#TRANSFORMACIÓN DE VARIABLES

#1 para Préstamo aprobado 0 para préstamo no aprobado

yE = (yE=='Y')*1

#0 para mujeres y 1 para hombres.

x1E = (x1E=='Male')*1

#1 para Casados 0 para no casados.

x2E = (x2E=='Yes')*1

#1 para Graduados 0 para no Graduados.

x4E = (x4E=='Graduate')*1

#1 es trabajador independiente 0 NO es trabajador independiente. 

x5E = (x5E=='Yes')*1


#Area de la propiedad
#Urbano         x11=  0 x12 = 0
#Semi-Urbano    x11 = 1 x12 = 0
#Rural          x11 = 0 x12 = 1

x11E = (x11_E=='Semiurban')*1
x12E = (x11_E=='Rural')*1

#Creamos el modelo de regresion logistica con las variables de entrenamiento
reg3 = glm(yE~x1E+x2E+x3E+x4E+x5E+x6E+x7E+x8E+x9E+x10E+x11E+x12E,family=binomial())
summary(reg3)

mmtest = cbind(yE,x3E,x6E,x7E,x8E,x9E,x10E)
correlactest = cor(mmtest)

##Gráficas de correlación
library("corrplot")
corrplot(correlactest)

pairs(mmtest)

library("car")
scatterplotMatrix(mmtest)

##########################TEST#######################################
test = trainrandom[431:length(x1_original),]
yP = test$Loan_Status
x1P = test$Gender
x2P = test$Married
x3P = test$Dependents
x4P = test$Education
x5P = test$Self_Employed
x6P = test$ApplicantIncome
x7P = test$CoapplicantIncome
x8P = test$LoanAmount
x9P = test$Loan_Amount_Term
x10P = test$Credit_History
x11_P = test$Property_Area

#TRANSFORMACIÓN DE VARIABLES

#1 para Préstamo aprobado 0 para préstamo no aprobado

yP = (yP=='Y')*1

#0 para mujeres y 1 para hombres.

x1P = (x1P=='Male')*1

#1 para Casados 0 para no casados.

x2P = (x2P=='Yes')*1

#1 para Graduados 0 para no Graduados.

x4P = (x4P=='Graduate')*1

#1 es trabajador independiente 0 NO es trabajador independiente. 

x5P = (x5P=='Yes')*1


#Area de la propiedad
#Urbano         x11=  0 x12 = 0
#Semi-Urbano    x11 = 1 x12 = 0
#Rural          x11 = 0 x12 = 1

x11P = (x11_P=='Semiurban')*1
x12P = (x11_P=='Rural')*1



# Formamos la ecuacion de regresion y le probamos con las variables de test
yest = exp(reg3$coefficients[1]+reg3$coefficients[2]*x1P+reg3$coefficients[3]*x2P+
             reg3$coefficients[4]*x3P+reg3$coefficients[5]*x4P+reg3$coefficients[6]*x5P+
             reg3$coefficients[7]*x6P+reg3$coefficients[8]*x7P+reg3$coefficients[9]*x8P+
             reg3$coefficients[10]*x9P+reg3$coefficients[11]*x10P+reg3$coefficients[12]*x11P+reg3$coefficients[13]*x12P)/(1+exp(reg3$coefficients[1]+reg3$coefficients[2]*x1P+reg3$coefficients[3]*x2P+
                                                                                                                                  reg3$coefficients[4]*x3P+reg3$coefficients[5]*x4P+reg3$coefficients[6]*x5P+
                                                                                                                                  reg3$coefficients[7]*x6P+reg3$coefficients[8]*x7P+reg3$coefficients[9]*x8P+
                                                                                                                                  reg3$coefficients[10]*x9P+reg3$coefficients[11]*x10P+reg3$coefficients[12]*x11P+reg3$coefficients[13]*x12P))

yest1 = round(yest) # se redondea los valores de yest para trasnformarlos en boolenos.

error = (yP==yest1)*1
# Que tan bueno es mi modelo de regresión
#accuracy = sum(error)/length(y_original) 
accuracy = sum(error)/length(yP) 




###############OUTLIERS #################

g_caja1<-boxplot(loanfinal$Dependents, col="skyblue", frame.plot=T)
d1=g_caja1$out
g_caja2<-boxplot(loanfinal$ApplicantIncome, col="skyblue", frame.plot=T)
d2=g_caja2$out
g_caja3<-boxplot(loanfinal$CoapplicantIncome, col="skyblue", frame.plot=T)
d3=g_caja3$out
g_caja4<-boxplot(loanfinal$LoanAmount, col="skyblue", frame.plot=T)
d4=g_caja4$out
g_caja5<-boxplot(loanfinal$Loan_Amount_Term, col="skyblue", frame.plot=T)
d5=g_caja5$out

g_caja1$stats
g_caja2$stats
g_caja3$stats
g_caja4$stats
g_caja5$stats

posicion_caja1 = which(d1>2)
posicion_caja2 = which(d2>10139)
posicion_caja3 = which(d3>5701)
posicion_caja4 = which(d4>265)
posicion_caja5 = which(d5>360)
posicion_caja5_2 = which(d5<360)

#Eliminaci�n de outliers
loanf<-loanfinal[!(loanfinal$Dependents %in% g_caja1$out),]
View(loanf)

#########################NUEVO MODELO ###########################
yN = loanf$Loan_Status
x1N = loanf$Gender
x2N = loanf$Married
x3N = loanf$Dependents
x4N = loanf$Education
x5N= loanf$Self_Employed
x6N = loanf$ApplicantIncome
x7N= loanf$CoapplicantIncome
x8N = loanf$LoanAmount
x9N = loanf$Loan_Amount_Term
x10N= loanf$Credit_History
x11N = loanf$Property_Area

####TRANSFORMACI?N VARIABLES CUALITATIVAS############


#1 para Pr?stamo aprobado 0 para pr?stamo no aprobado

yN = (yN=='Y')*1

#0 para mujeres y 1 para hombres.

x1N = (x1N=='Male')*1

#1 para Casados 0 para no casados.

x2N = (x2N=='Yes')*1

#1 para Graduados 0 para no Graduados.

x4N = (x4N=='Graduate')*1

#1 es trabajador independiente 0 NO es trabajador independiente. 

x5N = (x5N=='Yes')*1


#Area de la propiedad
#Urbano         x11=  0 x12 = 0
#Semi-Urbano    x11 = 1 x12 = 0
#Rural          x11 = 0 x12 = 1

x11N = (x11N=='Semiurban')*1
x12N = (x11N=='Rural')*1

#######################

set.seed(1)# semilla establecida para que los datos aleatorios no cambien.
aleatorio1 = sample(563,replace = F) #datos aleatorios de las observaciones
#Se obtiene la matriz aleatoria de los datos. 
trainrandom1 = loanf[c(aleatorio1),] #Se coloca la matriz con números de filas aleatorizadas.


##########################TRAINING#######################################
training1 =trainrandom1[1:394,]

#Creamos las variables de entrenamiento para el modelo de regresion logistica
ye = training1$Loan_Status
x1e = training1$Gender
x2e = training1$Married
x3e = training1$Dependents
x4e = training1$Education
x5e = training1$Self_Employed
x6e = training1$ApplicantIncome
x7e = training1$CoapplicantIncome
x8e = training1$LoanAmount
x9e = training1$Loan_Amount_Term
x10e = training1$Credit_History
x11_e = training1$Property_Area

#TRANSFORMACIÓN DE VARIABLES

#1 para Préstamo aprobado 0 para préstamo no aprobado

ye = (ye=='Y')*1

#0 para mujeres y 1 para hombres.

x1e = (x1e=='Male')*1

#1 para Casados 0 para no casados.

x2e = (x2e=='Yes')*1

#1 para Graduados 0 para no Graduados.

x4e = (x4e=='Graduate')*1

#1 es trabajador independiente 0 NO es trabajador independiente. 

x5e = (x5e=='Yes')*1


#Area de la propiedad
#Urbano         x11=  0 x12 = 0
#Semi-Urbano    x11 = 1 x12 = 0
#Rural          x11 = 0 x12 = 1

x11e = (x11_e=='Semiurban')*1
x12e = (x11_e=='Rural')*1

######MATRIZ DE CORRELACION

mm2= cbind(ye,x3e,x6e,x7e,x8e,x9e)
correlac2 = cor(mm2)

##Gráficas de correlación
library("corrplot")
corrplot(correlac2)


#Creamos el modelo de regresion logistica con las variables de entrenamiento sin la variable x3
#debido a su baja correlaci�n. 
reg5 = glm(ye~x1e+x2e+x4e+x5e+x6e+x7e+x8e+x9e+x10e+x11e+x12e,family=binomial())
summary(reg5)

mm4 = cbind(ye,x1e,x2e,x4e,x5e,x6e,x7e,x8e,x9e,x10e,x11e,x12e)
correlac4 = cor(mm4)

##Gráficas de correlación
library("corrplot")
corrplot(correlac4)

#######ANALISIS FACTORIAL #########
# Test de Barlett
vectorpca = c(4,7:11)
bt = bartlett.test(loanf[, vectorpca]) #Mientas mas alto mejor.

co = cor(loanf[,vectorpca])

library("car")
scatterplotMatrix(loanf[,vectorpca ])
library("corrplot")
corrplot(co)
########################
# Kaiser, Meyer, Olkin - KMO
library("REdaS")
kmo = KMOS(trainrandom1[, vectorpca]) #Criterio 0.40 -- no adecuado

#Matriz de Variables Independientes de training
matriz = cbind(x3e,x6e,x7e,x8e,x9e,x10e)
pca = prcomp(matriz, center = TRUE, scale. = TRUE) 
print(pca)
plot(pca, type = "l")
summary(pca)

# Componentes Principales
cp = predict(pca, newdata=tail(matriz, length(ye)))
cp = as.data.frame(cp)

#Regresi�n solo con variables cuantitativas

#Con variables cuantitativas utilizadas en el sistema Experto
regcuant = glm(ye ~x6e+x8e+x9e,family=binomial())
summary(regcuant)

#Regresi�n con PCA
#Con 3 PC usadas en el sistema Experto
regpca3 = glm(ye ~ cp$PC1+cp$PC3+cp$PC4,family=binomial())
summary(regpca3)

#Con 4 PC
regpca = glm(ye ~ x1e+x2e+cp$PC1+x4e+x5e+cp$PC2+cp$PC3+cp$PC4+x11e+x12e,family=binomial())
summary(regpca)
#Con 5 PC
regpca2 = glm(ye ~ x1e+x2e+cp$PC1+x4e+x5e+cp$PC2+cp$PC3+cp$PC4+cp$PC5+x11e+x12e,family=binomial())
summary(regpca2)



##########################TEST#######################################

test1 = trainrandom1[395:length(yN),]

yp = test1$Loan_Status
x1p = test1$Gender
x2p = test1$Married
x3p = test1$Dependents
x4p = test1$Education
x5p = test1$Self_Employed
x6p = test1$ApplicantIncome
x7p = test1$CoapplicantIncome
x8p = test1$LoanAmount
x9p = test1$Loan_Amount_Term
x10p = test1$Credit_History
x11_p = test1$Property_Area

#TRANSFORMACIÓN DE VARIABLES

#1 para Préstamo aprobado 0 para préstamo no aprobado

yp = (yp=='Y')*1

#0 para mujeres y 1 para hombres.

x1p = (x1p=='Male')*1

#1 para Casados 0 para no casados.

x2p = (x2p=='Yes')*1

#1 para Graduados 0 para no Graduados.

x4p = (x4p=='Graduate')*1

#1 es trabajador independiente 0 NO es trabajador independiente. 

x5p = (x5p=='Yes')*1


#Area de la propiedad
#Urbano         x11=  0 x12 = 0
#Semi-Urbano    x11 = 1 x12 = 0
#Rural          x11 = 0 x12 = 1

x11p = (x11_p=='Semiurban')*1
x12p = (x11_p=='Rural')*1


# Formamos la ecuacion de regresión y le probamos con las variables de test, quitando la variable 3 por baja correlacion
yest5 = exp(reg5$coefficients[1]+reg5$coefficients[2]*x1p+reg5$coefficients[3]*x2p+reg5$coefficients[4]*x4p+
              reg5$coefficients[5]*x5p+reg5$coefficients[6]*x6p+reg5$coefficients[7]*x7p+
              reg5$coefficients[8]*x8p+reg5$coefficients[9]*x9p+reg5$coefficients[10]*x10p+
              reg5$coefficients[11]*x11p+reg5$coefficients[12]*x12p)/(1+exp(reg5$coefficients[1]+reg5$coefficients[2]*x1p+reg5$coefficients[3]*x2p+reg5$coefficients[4]*x4p+
                                                                              reg5$coefficients[5]*x5p+reg5$coefficients[6]*x6p+reg5$coefficients[7]*x7p+
                                                                              reg5$coefficients[8]*x8p+reg5$coefficients[9]*x9p+reg5$coefficients[10]*x10p+
                                                                              reg5$coefficients[11]*x11p+reg5$coefficients[12]*x12p))



yest6 = round(yest5) # se redondea los valores de yest para trasnformarlos en boolenos.

error3 = (yp==yest6)*1

# Que tan bueno es mi modelo de regresion
accuracy3 = sum(error3)/length(yp)

#Matriz de Variables Independientes de training
matriztest = cbind(x3p,x6p,x7p,x8p,x9p,x10p)
pca2 = prcomp(matriztest, center = TRUE, scale. = TRUE) 
print(pca2)
plot(pca2, type = "l")
summary(pca2)

# Componentes Principales
cpt = predict(pca2, newdata=tail(matriztest, length(yp)))
cpt = as.data.frame(cpt)

#Ecuaci�n solo tomando 4 valores PCA
yestpca4 = exp(regpca$coefficients[1]+regpca$coefficients[2]*x1p+regpca$coefficients[3]*x2p+
                 regpca$coefficients[4]*cpt$PC1+regpca$coefficients[5]*x4p+regpca$coefficients[6]*x5p+
                 regpca$coefficients[7]*cpt$PC2+regpca$coefficients[8]*cpt$PC3+regpca$coefficients[9]*cpt$PC4+regpca$coefficients[10]*x11p+regpca$coefficients[11]*x12p)/(1+exp(regpca$coefficients[1]+regpca$coefficients[2]*x1p+regpca$coefficients[3]*x2p+
                                                                                                                                                                                  regpca$coefficients[4]*cpt$PC1+regpca$coefficients[5]*x4p+regpca$coefficients[6]*x5p+
                                                                                                                                                                                  regpca$coefficients[7]*cpt$PC2+regpca$coefficients[8]*cpt$PC3+regpca$coefficients[9]*cpt$PC4+regpca$coefficients[10]*x11p+regpca$coefficients[11]*x12p))



#Ecuaci�n solo tomando 5 valores PCA
yestpca5 = exp(regpca2$coefficients[1]+regpca2$coefficients[2]*x1p+regpca2$coefficients[3]*x2p+
                 regpca2$coefficients[4]*cpt$PC1+regpca2$coefficients[5]*x4p+regpca2$coefficients[6]*x5p+
                 regpca2$coefficients[7]*cpt$PC2+regpca2$coefficients[8]*cpt$PC3+regpca2$coefficients[9]*cpt$PC4+regpca2$coefficients[10]*cpt$PC5+regpca2$coefficients[11]*x11p+regpca2$coefficients[12]*x12p)/(1+exp(regpca2$coefficients[1]+regpca2$coefficients[2]*x1p+regpca2$coefficients[3]*x2p+
                                                                                                                                                                                                                         regpca2$coefficients[4]*cpt$PC1+regpca2$coefficients[5]*x4p+regpca2$coefficients[6]*x5p+
                                                                                                                                                                                                                         regpca2$coefficients[7]*cpt$PC2+regpca2$coefficients[8]*cpt$PC3+regpca2$coefficients[9]*cpt$PC4+regpca2$coefficients[10]*cpt$PC5+regpca2$coefficients[11]*x11p+regpca2$coefficients[12]*x12p))
#Ecuacion con  3 variables independientes
yestind = exp(regcuant$coefficients[1]+regcuant$coefficients[2]*x3p
              +regcuant$coefficients[3]*x8p+regcuant$coefficients[4]*x9p)/(1+exp(regcuant$coefficients[1]+regcuant$coefficients[2]*x3p
                                                                                 +regcuant$coefficients[3]*x8p+regcuant$coefficients[4]*x9p))

#Ecuacion con  3 Componentes Principales
yestindpca = exp(regcuant$coefficients[1]+regcuant$coefficients[2]*cpt$PC1
                 +regcuant$coefficients[3]*cpt$PC3+regcuant$coefficients[4]*cpt$PC4)/(1+exp(regcuant$coefficients[1]+regcuant$coefficients[2]*cpt$PC1
                                                                                            +regcuant$coefficients[3]*cpt$PC3+regcuant$coefficients[4]*cpt$PC4))


yestpca41 = round(yestpca4) # se redondea los valores de yest para trasnformarlos en boolenos.

errorpca1 = (yp==yestpca41)*1

yestpca51 = round(yestpca5) # se redondea los valores de yest para trasnformarlos en boolenos.

errorpca2 = (yp==yestpca51)*1

yestind1 = round(yestind)
errorind = (yp==yestind1)*1

yestindpca1 = round(yestindpca)
errorindpca = (yp==yestindpca1)*1



# Que tan bueno es mi modelo de regresion con 6 PC
accuracypca4 = sum(errorpca1)/length(yp)

# Que tan bueno es mi modelo de regresion con 5 PC
accuracypca5 = sum(errorpca2)/length(yp)


# Que tan bueno es mi modelo de regresion solo con 3 variables independientes
accuracyind = sum(errorind)/length(yp)

# Que tan bueno es mi modelo de regresion solo con 3 PC
accuracyindpca = sum(errorindpca)/length(yp)

#############Escalamiento Multidimensional

d = dist(loanf[,4,7:11 ], method = "euclidean")
fit = cmdscale(d,eig=TRUE, k=3) # k es el numero de dimensiones
x = fit$points[,1] 
y = fit$points[,2]
z=fit$points[,3]

clase = as.factor(loanf$Loan_Status)

library(rgl)
open3d() # Abre el sistema gráfico donde mostrar las gráficas

plot3d(x, y, z , c=as.integer(clase))







