#--------------------------------------#
# ACTIVAVATION OF LIRAIRIES #
#--------------------------------------#
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("C50")
library(C50)
install.packages("randomForest")
library(randomForest)
install.packages("e1071")
library(e1071)
install.packages("naivebayes")
library(naivebayes)
install.packages("nnet")
library(nnet)
install.packages("kknn")
library(kknn)
install.packages("ROCR")
library(ROCR)
install.packages("rvest")
library(rvest)
install.packages("ggplot2")
library(ggplot2)
install.packages("plyr")
library(plyr)
install.packages("dplyr")
library(dplyr)
install.packages("scales")
library(scales)
install.packages("maps")
library(maps)
install.packages("mapproj")
library(mapproj)
install.packages("plotly")
library(plotly)
install.packages("digest")
library(digest)
install.packages("stringr")
library(stringr)
install.packages("sqldf")
library(sqldf)
install.packages("tree")
library(tree)
install.packages("RJDBC")
library("RJDBC")
#--------------------------------#
  # PREPARATION DES DONNEES #
#Analyse exploratoire des donn?es#
#--------------------------------#


#charger les fichiers
#set the directory
setwd("C:\\vagrant-projects-staging\\OracleDatabase\\21.3.0")

write.csv(customer,"Customersv.csv")
#loading data from CSV R
catalogue <- read.csv("Catalogue.csv" , header = TRUE, sep = ",", dec = ".")
immatriculations <- read.csv("Immatriculations.csv" , header = TRUE, sep = ",", dec = ".")
marketing <- read.csv("Marketing.csv" , header = TRUE, sep = ",", dec = ".")
client <- read.csv("Clients_11.csv" , header = TRUE, sep = ",", dec = ".")

#write.csv(catalogue, "catalogue2.csv", row.names = FALSE)
#Visualisation
str(catalogue)
str(immatriculations)
str(marketing)
str(client)

#-----------------#
#Catalogue trier#
#-----------------#

names(catalogue)
attach(catalogue)
summary(catalogue)
with(catalogue, mean(puissance))

t<-catalogue[occasion=="true",]
f<-catalogue[occasion=="false",]

#filtres


#catalogue<- filter( catalogue, marque!="?" & marque!="N/D" & marque!=" ")
catalogue <- subset(catalogue, marque != "?" & marque != "N/D" & marque != " ")
catalogue<- subset( catalogue, nom!="?" & nom!="N/D" & nom!=" ")
catalogue<- subset( catalogue, puissance!="?" & puissance!="N/D" & puissance!=" ")
catalogue<- subset( catalogue, longueur!="?" & longueur!="N/D" & longueur!=" ")
catalogue<- subset( catalogue, nbPlaces!="?" & nbPlaces!="N/D" & nbPlaces!=" ")
catalogue<- subset( catalogue, nbPortes!="?" & nbPortes!="N/D" & nbPortes!=" ")
catalogue<- subset( catalogue, couleur!="?" & couleur!="N/D" & couleur!=" ")
catalogue<- subset( catalogue, occasion!="?" & occasion!="N/D" & occasion!=" ")
catalogue<- subset( catalogue, prix!="?" & prix!="N/D" & prix!=" ")

#clean file 


#conversions of types
class(catalogue$puissance)
names(catalogue)

catalogue$puissance <- as.numeric(catalogue$puissance)
catalogue$longueur <- as.numeric(catalogue$longueur)
catalogue$nbPlaces <- as.numeric(catalogue$nbPlaces)
catalogue$nbPortes <- as.numeric(catalogue$nbPortes)
catalogue$couleur <- as.factor(catalogue$couleur)
catalogue$occasion<- as.logical.factor(catalogue$occasion)
catalogue$prix<- as.numeric(catalogue$prix)
summary(catalogue)
#--------------#
#Client#
#--------------#

attach(client)

#filtre and replace 
names(client)

client<- subset( client, sexe!="?" & sexe!="N/D" & sexe!=" ")
client<- subset( client, age!="?" & age!="N/D" & age!=" ")
client<- subset( client, taux!="?" & taux!="N/D" & taux!=" ")
client<- subset( client, situationFamiliale!="?" & situationFamiliale!="N/D" & situationFamiliale!=" ")
client<- subset( client, nbEnfantsAcharge!="?" & nbEnfantsAcharge!="N/D" & nbEnfantsAcharge!=" ")
client<- subset( client, X2eme.voiture!="?" & X2eme.voiture!="N/D" & X2eme.voiture!=" ")
client<- subset( client, immatriculation!="?" & immatriculation!="N/D" & immatriculation!=" ")

#test display
client[client$sexe!="Masculin" & client$sexe!="M" &  client$sexe!="F?minin" &  client$sexe!="F" &  client$sexe!="Femme" &  client$sexe!="Homme" &  client$sexe!="?" &  client$sexe!="N/D",]
client[sexe=="?" &  sexe=="N/D" & sexe==" ",]
client[age=="?" &  age=="N/D" & age==" ",]

#replace the client sexe typing 

client$sexe <- str_replace(client$sexe, "Homme", "M")
customers_ext$sexe <- str_replace(customers_ext$sexe, "Masculin", "M")
customers_ext$sexe <- str_replace(customers_ext$sexe, "F?minin", "F")
customers_ext$sexe <- str_replace(customers_ext$sexe, "Femme", "F")
customers_ext$sexe <- str_replace(customers_ext$sexe, "F�F", "F")

client
# catgories exists : Seul, Seule, Celibataire, Mari?(e), En couple, Divorcee. 
#replacing  

client$situationFamiliale <- str_replace(client$situationFamiliale, "Seul", "Celibataire")
client$situationFamiliale <- str_replace(client$situationFamiliale, "Seule", "Celibataire")
client$situationFamiliale <- str_replace(client$situationFamiliale, "C?libatairee", "Celibataire")
client$situationFamiliale <- str_replace(client$situationFamiliale, "C?libataire", "Celibataire")
client$situationFamiliale <- str_replace(client$situationFamiliale, "C?Celibataire", "Celibataire")
client$situationFamiliale <- str_replace(client$situationFamiliale, "C?C?Celibataire", "Celibataire")
client$situationFamiliale <- str_replace(client$situationFamiliale, "C?CeCelibataire", "Celibataire")
client$situationFamiliale <- str_replace(client$situationFamiliale, "C?C?CeCelibataire", "Celibataire")
client$situationFamiliale <- str_replace(client$situationFamiliale, "C�Celibataire", "Celibataire")
client$situationFamiliale <- str_replace(client$situationFamiliale, "Divorc\xe9e", "Celibataire")
client$situationFamiliale <- str_replace(client$situationFamiliale, "C\xe9libataire", "Celibataire")

client$situationFamiliale <- gsub("Mari\\�\\(e\\)", "En Couple", client$situationFamiliale)

client$situationFamiliale <- str_replace(client$situationFamiliale, "Mari�(e)", "En Couple")
#checking
client[client$situationFamiliale!="En Couple" & client$situationFamiliale!="Celibataire" & client$situationFamiliale!="Mari�(e)" & client$situationFamiliale!="Divorc�e",]
client[client$situationFamiliale!="En Couple" & client$situationFamiliale!="Celibataire" & client$situationFamiliale!="Seule" & client$situationFamiliale!="Mari?(e)" & client$situationFamiliale!="Seul" & client$situationFamiliale!="Divorc?e",]
client
 # nb d'enfants

client[client$nbEnfantsAcharge!="0" & client$nbEnfantsAcharge!="1" & client$nbEnfantsAcharge!="2" & client$nbEnfantsAcharge!="3" & client$nbEnfantsAcharge!="4",]

#nbEnfants equals -1 
client<- subset( client, nbEnfantsAcharge!="-1")

#checking
client[client$nbEnfantsAcharge<"0",]

#checking taux :
client[client$taux<"0" ,]

#il y a pas mal de client qui ont -1 taux donc on enl?ve ces lignes
client<- subset( client, taux!="-1")
#conversions of types
client$age <- as.numeric(client$age)
client$sexe <- as.factor(client$sexe)
client$taux <- as.numeric(client$taux)
client$situationFamiliale <- as.factor(client$situationFamiliale)
client$nbEnfantsAcharge <- as.numeric(client$nbEnfantsAcharge)
client$X2eme.voiture <- as.logical(client$X2eme.voiture)



summary(client)
#check for age :
client[client$age <"18" ,]
client[client$age >"84" ,]

#age -1 
client<- subset( client, age!="-1")

#v?rifions 2eme voiture :
client[client$X2eme.voiture!="true" & client$X2eme.voiture!="false",]

#immatriculations :

#NA

x<-na.omit(client)
sum(is.na(x))
#no NA

#------------------------#
#marketing
#------------------------#
marketing$situationFamiliale <- str_replace(marketing$situationFamiliale, "C\xe9libataire", "Celibataire")
marketing$age <- as.numeric(marketing$age)
marketing$sexe <- as.factor(marketing$sexe)
marketing$taux <- as.numeric(marketing$taux)
marketing$situationFamiliale <- as.factor(marketing$situationFamiliale)
marketing$nbEnfantsAcharge <- as.numeric(marketing$nbEnfantsAcharge)
marketing$X2eme.voiture <- as.logical(marketing$X2eme.voiture)
summary(marketing)

#------------------------#
#Immatriculations
#------------------------#


immatriculations$nbPlaces <- as.numeric(immatriculations$nbPlaces)
immatriculations$nbPortes <- as.numeric(immatriculations$nbPortes)
immatriculations$couleur <- as.factor(immatriculations$couleur)
immatriculations$occasion<- as.logical(immatriculations$occasion)
immatriculations$longueur<- as.factor(immatriculations$longueur)
immatriculations$prix<- as.numeric(immatriculations$prix)
summary(immatriculations)
#ATTENTION au format des immatriculations !
#A FAIRE
#no NA
x<-na.omit(immatriculations)
sum(is.na(x))



#----------------------------------------------#
#join immatriculations and client#
#----------------------------------------------#

customer <-merge(client, immatriculations, by="immatriculation")
#d'accord j'ai cr?? mon client Complet mais j'ai plus de ligne que de client 
#regardons le nombre de client qui ont 2 voitures:
customer
client2voitures <- client[client$X2eme.voiture!="FALSE",]
client2voitures
print(customer)



#DOUBLONS

#pour clients dans les immatriculations
doublons <- client[duplicated(client$immatriculation),]
doublons

sum(duplicated(customer$immatriculation))
#12 doubles
client_unique<-unique(client)
client_unique


#same immat owned by different persons
#voyons si dans immatriculations elle est double
immatriculations[immatriculations$immatriculation=="1557 AB 48",]

#its correspond to 2 different cars
#la liaison dans cutomers : 
customer[customer$immatriculation=="1557 AB 48",]
customer
# 1 immat creer 4 lines in customers

#we having 12 doublons immat in clients 12*4 lignes =72 line to delete into customers

#delete the doubles in client 
client <- client[duplicated(client$immatriculation) =="FALSE",]
client_sans_doublons <- client[duplicated(client$immatriculation) =="FALSE",]
client_sans_doublons
#doublons dans immatriculations:
doublons_immat <- immatriculations[duplicated(immatriculations$immatriculation)=="TRUE",]
doublons_immat

#on les enl?ve du coup :
immatriculations
immatriculations <- immatriculations[duplicated(immatriculations$immatriculation)=="FALSE",]
immatriculations_sans_doublons <- immatriculations[duplicated(immatriculations$immatriculation) =="FALSE",]
immatriculations_sans_doublons
#on relie les 2 tables 
customer <-merge(client, immatriculations, by="immatriculation")
summary(customer)
#doubles in customers
customer[duplicated(customer$immatriculation)=="TRUE",]
#NO DOUBLE IN CUSTOMER

#----------#
#CATEGORIES#
#----------#
install.packages("ggplot2")
library(ggplot2)
immatriculations$longueur <- as.factor(immatriculations$longueur)
immatriculations$longueur <- str_replace(immatriculations$longueur,"tr\xe8s longue", "treslongue")
summary(immatriculations)
catalogue$longueur <- str_replace(catalogue$longueur, "tr�s longue", "treslongue")
#nuage de point categories de voiture
names(catalogue)
sum(is.na(catalogue))
# Check for non-numeric values
any(!is.numeric(df))
names(df)[!sapply(df, is.numeric)]
catalogue <- catalogue[complete.cases(catalogue),]

str(catalogue)
# Créer une boîte à moustaches pour l'âge
boxplot(client$age, main = "Boîte à moustaches pour l'âge", ylab = "Âge")
# Créer une boîte à moustaches pour taux
boxplot(client$taux, main = "Boîte à moustaches pour taux", ylab = "Taux")
library(ggplot2)
ggplot(catalogue, aes(x = longueur, y = puissance, color = nbPlaces)) +
  geom_point() +
  labs(title = "Répartition des voitures",
       x = "Longueur", y = "Puissance", color = "Nombre de places") +
  theme_bw()


ggplot(catalogue, aes(x = longueur, y = puissance)) + geom_point()

qplot(longueur, puissance, data= catalogue)

qplot(puissance,nbPortes , data=customer, color=longueur)

qplot(longueur, prix, data=customer)

qplot(nbPlaces, prix, data=catalogue)

qplot(nbPlaces, nbPortes, data=catalogue, color=longueur)

qplot(longueur, nbPortes, data=catalogue) 

qplot(puissance, prix, data=catalogue, color=longueur)

qplot(nbPlaces, puissance, data=catalogue, color=longueur)

qplot(nom, prix, data=catalogue, color=longueur)


#je pense ? 3 ou 4 cat?gories : citadines, familiale/simple, grande famille, sport

#doubles catalogue
catalogue[duplicated(catalogue)=="TRUE",]
#none


citadines <- catalogue[catalogue$longueur=="courte",]
grandeFamille <- catalogue[catalogue$nbPlaces==7,]
sport <- catalogue[catalogue$puissance >300,]
berlineconfort <- catalogue[catalogue$longueur=="treslongue" & catalogue$puissance> 190 & catalogue$puissance <300,]
berlinecompact <- catalogue[catalogue$longueur=="moyenne",]
berline <- catalogue[catalogue$longueur=="longue" & catalogue$nbPlaces!=7,]
 
summary(client)
#faire des criteres tres precis pour pouvoir creer immatriculations$categorie

##ATTENTION il y a des incoherences --> par exemple : new beetle c'est pas 5 places mais 4 donc pas dans la bonne categorie car on ne peut pas la conseiller ? des familles (trop serrees à l'arriere), donc irait plus dans la categorie citadine


#attribuer les cat?gories aux don?nes de Immatriculations

#first TEST :
immatriculations$categorie <- ifelse(immatriculations$longueur =="courte", immatriculations$categorie <- "citadine", 
                                     ifelse(immatriculations$nbPlaces ==7, immatriculation$categorie <- "monospace", 
                                            ifelse(immatriculations$puissance >= 200 & immatriculations$longueur=="treslongue", immatriculations$categorie <- "sport", immatriculations$categorie <- "normale_familiale")))

#second TEST :
immatriculations$categorie <- ifelse(immatriculations$longueur =="courte", immatriculations$categorie <- "citadine", 
                                     ifelse(immatriculations$nbPlaces ==7, immatriculation$categorie <- "monospace", 
                                            ifelse(immatriculations$puissance > 300, immatriculations$categorie <- "sport", 
                                                   ifelse(immatriculations$longueur =="treslongue" & immatriculations$puissance> 190 & immatriculations$puissance <300, immatriculations$categorie <- "berlineconfort", 
                                                          ifelse(immatriculations$longueur=="moyenne", immatriculations$categorie <- "berlinecompact", immatriculations$categorie<- "berline")))))





immatriculations[immatriculations$nbPlaces == 7]
#no nb place==7
summary(immatriculations)
immatriculations$categorie<- as.factor(immatriculations$categorie)
#join between imma and client 
customer <-merge(client, immatriculations, by="immatriculation")
summary(customer)
customer$situationFamiliale<-as.factor(customer$situationFamiliale)
customer$situationFamiliale <- gsub("Mari\\�\\(e\\)", "En Couple", customer$situationFamiliale)
str(customer)
#del column not usefull 


customer <- subset(customer, select= -immatriculation)
customer <- subset(customer, select= -nbPlaces)

names(customer)
customer
#write.csv(catalogue, file = "catalogue_vf.csv", row.names = FALSE)


#-----------------#
#Marketing #
#-----------------#

marketing$age <- as.numeric(marketing$age)
marketing$sexe <- as.factor(marketing$sexe)
marketing$taux <- as.numeric(marketing$taux)
marketing$situationFamiliale <- as.factor(marketing$situationFamiliale)
marketing$nbEnfantsAcharge <- as.numeric(marketing$nbEnfantsAcharge)
marketing$X2eme.voiture <- as.logical(marketing$X2eme.voiture)

str(marketing)

set.seed(123) # for reproducibility
categories <- levels(training_data$categorie)
marketing$categorie <- factor(sample(categories, nrow(marketing), replace = TRUE))
# Remove missing values
marketing <- na.omit(marketing)

# Remove outliers using IQR method
cols_to_remove_outliers <- c("age", "taux", "nbEnfantsAcharge")

for (col in cols_to_remove_outliers) {
  q1 <- quantile(marketing[, col], 0.25)
  q3 <- quantile(marketing[, col], 0.75)
  iqr <- q3 - q1
  marketing <- marketing[!(marketing[, col] < (q1 - 1.5 * iqr) | marketing[, col] > (q3 + 1.5 * iqr)), ]
}

str(testing_data)
str(training_data)
str(marketing)
marketing
# Convert categorical variables to factors
marketing$categorie <- factor(marketing$categorie, levels = levels(training_data$categorie))
# Find factor columns in the data
factor_cols <- sapply(training_data, is.factor)

# Compare factor levels across data
mapply(function(x, y) all(levels(x) %in% levels(y)), marketing[, factor_cols], training_data[, factor_cols])

#........................#
#training part 


# Load the caret package

install.packages("caret")
library(caret)

# Set the seed for reproducibility
set.seed(123)
str(customer)
# Split the dataset into 70% training and 30% testing
train_index <- createDataPartition(customer$categorie, p = 0.7, list = FALSE)
training_data <- customer[train_index, ]
testing_data <- customer[-train_index, ]
testing_data <- testing_data[, -which(names(testing_data) == "categorie")]
names(training_data)
names(testing_data)



#------------#
#CLASSIFIEURS#
#------------#

#Suppression des variables inutiles

training_data <- subset(training_data, select = -nbPortes)
training_data <- subset(training_data, select = -longueur)
training_data <- subset(training_data, select = -puissance)
training_data <- subset(training_data, select = -marque)
training_data <- subset(training_data, select = -nom)
training_data <- subset(training_data, select = -couleur)
training_data <- subset(training_data, select = -occasion)
training_data <- subset(training_data, select = -prix)


testing_data <- subset(testing_data, select = -nbPortes)
testing_data <- subset(testing_data, select = -longueur)
testing_data <- subset(testing_data, select = -puissance)
testing_data <- subset(testing_data, select = -marque)
testing_data <- subset(testing_data, select = -nom)
testing_data <- subset(testing_data, select = -couleur)
testing_data <- subset(testing_data, select = -occasion)
testing_data <- subset(testing_data, select = -prix)


names(testing_data)
names(training_data)

#-------------#
# NAIVE BAYES #
#-------------#
install.packages("naivebayes")
library(naivebayes)
install.packages("caret")
library(caret)
install.packages("pryr")
library(pryr)

# Apprentissage du classifeur de type naive bayes
#se the laplace = 1 argument in the naive_bayes() function to 
#apply Laplace smoothing with a smoothing factor of 1. For example:
#to avoid having zero probabilities for a particular feature value given a certain class

#no missing values 

sum(is.na(testing_data))
sum(is.na(training_data))
testing_data

nb <- naive_bayes(training_data$categorie~., training_data,laplace = 1)
nb

# Check levels of categorical variables in training data
cat_vars <- c("sexe", "situationFamiliale", "X2eme.voiture", "categorie")
for(var in cat_vars){
  print(paste0("Levels in training_data$", var, ": "))
  print(levels(training_data[, var]))
}

# Check levels of categorical variables in new data
cat_vars <- c("sexe", "situationFamiliale", "X2eme.voiture", "categorie")
for(var in cat_vars){
  print(paste0("Levels in testing_data$", var, ": "))
  print(levels(testing_data[, var]))
}

# Find common columns between training_data and testing_data
common_cols <- intersect(names(training_data), names(testing_data))

# Only keep common columns in client_ET
testing_data <- testing_data[, common_cols]
length(testing_data$categorie)
names(testing_data)
testing_data

# Test du classifieur : classe predite
nb_class <- predict(nb, testing_data, type = "class")
nb_class2 <- predict(nb, marketing, type = "class")
marketing
table(nb_class)


# Test du classifieur : probabilites pour chaque prediction testting_data and Marketing
nb_prob <- predict(nb, testing_data, type="prob")
nb_prob2 <- predict(nb, marketing, type="prob")
nb_prob

testing_data$categorie_predicted <- nb_class

marketing$categorie_predicted <- nb_class2
# Matrice de confusion
table( testing_data$categorie, nb_class)
table( marketing$categorie, nb_class2)

# Compute confusion matrix and accuracy for testing data
conf_matrix <- confusionMatrix(testing_data$categorie, nb_class)
# Compute confusion matrix and accuracy for marketing
conf_matrix <- confusionMatrix(marketing$categorie, nb_class2)
accuracy <- conf_matrix$overall["Accuracy"]
# Calculate precision, recall, and F1-score for each class
precision <- conf_matrix$overall["Precision"]
recall <- conf_matrix$categorie['Recall']
f1_score <- conf_matrix$categorie['F1']
# Calculate precision, recall, and F1-score for each class
precision <- numeric(length = length(unique(testing_data$categorie)))
recall <- numeric(length = length(unique(testing_data$categorie)))
f1_score <- numeric(length = length(unique(testing_data$categorie)))
# Calculate precision, recall, and F1-score for each class for marketing
precision <- numeric(length = length(unique(marketing$categorie)))
recall <- numeric(length = length(unique(marketing$categorie)))
f1_score <- numeric(length = length(unique(marketing$categorie)))
for (i in seq_along(precision)) {
  tryCatch({
    precision[i] <- conf_matrix$byClass[i, "Precision"]
  }, error = function(e) {
    precision[i] <- 0
  })
  tryCatch({
    recall[i] <- conf_matrix$byClass[i, "Recall"]
  }, error = function(e) {
    recall[i] <- 0
  })
  f1_score[i] <- 2 * (precision[i] * recall[i]) / (precision[i] + recall[i])
}

# Print results
print(paste0("Precision: ", precision))
print(paste0("Recall: ", recall))
print(paste0("F1-score: ", f1_score))
print(paste0("Accuracy: ", accuracy))

marketing
#-------------#
# C5.0        #
#-------------#
#install package c5.0
install.packages("C50")
library(C50)
summary(marketing)
summary(testing_data)
#convertion types because C5.0 hadle factor and num
training_data$categorie <- as.factor(training_data$categorie)
training_data$X2eme.voiture <- as.factor(training_data$X2eme.voiture)
testing_data$X2eme.voiture <- as.factor(testing_data$X2eme.voiture)
marketing$X2eme.voiture <- as.factor(marketing$X2eme.voiture)
training_data$situationFamiliale <- as.factor(training_data$situationFamiliale)

str(marketing)
names(testing_data)





# Training DECISION TREE 

tree_C50 <- C5.0(training_data$categorie~., training_data)
tree_C50
plot(tree_C50, type="simple")

# Test et taux de succes pour le 1er paramétrage pour C5.0()
test_C50 <- predict(tree_C50, testing_data, type="class")
print(taux_C51 <- nrow(testing_data[testing_data$categorie %in% test_C50,])/nrow(testing_data))



# Test et taux de succes pour le 1er paramétrage pour C5.0() for Marketing
test_C50 <- predict(tree_C50, marketing, type="class")
print(taux_C51 <- nrow(marketing[marketing$categorie %in% test_C50,])/nrow(marketing))


summary(training_data$categorie)
# Apprentissage 2nd paramétrage pour C5.0
str(training_data)

tree_C52 <- C5.0(categorie ~ ., data = training_data, control = C5.0Control(minCases = 10, noGlobalPruning = FALSE))
plot(tree_C52, type = "simple")

tree_C53 <- C5.0(training_data$categorie~., training_data, control = C5.0Control(minCases = 10, noGlobalPruning = T))
tree_C54 <- C5.0(training_data$categorie~., training_data, control = C5.0Control(minCases = 5, noGlobalPruning = F))
tree_C55 <- C5.0(training_data$categorie~., training_data, control = C5.0Control(minCases = 5, noGlobalPruning = T))
plot(tree_C53, type="simple")
plot(tree_C54, type="simple")
plot(tree_C55, type="simple")

#---------------------------------------------------------------#
  
  # Test et taux de succes pour le 2eme paramétrage pour C5.0()
test_C52 <- predict(tree_C52, testing_data, type="class")
print(taux_C52 <- nrow(testing_data[testing_data$categorie==test_C52,])/nrow(testing_data))
# Test et taux de succes pour le 3eme paramétrage pour C5.0()
test_C53 <- predict(tree_C53, testing_data, type="class")
print(taux_C53 <- nrow(testing_data[testing_data$categorie==test_C53,])/nrow(testing_data))
# Test et taux de succes pour le 4eme paramétrage pour C5.0()
test_C54 <- predict(tree_C54, testing_data, type="class")
print(taux_C54 <- nrow(testing_data[testing_data$categorie==test_C54,])/nrow(testing_data))
# Test et taux de succes pour le 5eme paramétrage pour C5.0()
test_C55 <- predict(tree_C55, testing_data, type="class")
print(taux_C55 <- nrow(testing_data[testing_data$categorie==test_C55,])/nrow(testing_data))

#-----------------#
# NEURAL NETWORKS #
#-----------------#


# Apprentissage du classifeur de type perceptron monocouche
install.packages("nnet")
library(nnet)


classifieur_nn <- nnet(categorie~age + sexe +taux+ situationFamiliale+nbEnfantsAcharge+X2eme.voiture, training_data, size=6)
# Get the predicted categories from the kknn object
predicted <- predict(classifieur_knn,testing_data,type = "class")

# Create a confusion matrix object
cm <- confusionMatrix(predicted, testing_data$categorie)



# Train the model using 10-fold cross-validation
model <- train(categorie ~ age + sexe + taux + situationFamiliale + nbEnfantsAcharge + X2eme.voiture, 
               data = training_data, 
               method = "nnet",
               trControl = ctrl,
               tuneLength = 5,
               trace = FALSE)

# Print the model results
print(model)




#---------------------#
# K-NEAREST NEIGHBORS #
#---------------------#
install.packages("kknn")
library(kknn)


classifieur_knn <- kknn(categorie ~ ., train = training_data, test = testing_data, k = 5, distance = 2, kernel = "optimal")

# Get the predicted categories from the kknn object
predicted <- predict(classifieur_knn)
# Create a confusion matrix object
cm <- confusionMatrix(predicted, testing_data$categorie)

# Create a confusion matrix plot using ggplot2 and caret
ggplot(data = data.frame(cm$table, actual = rownames(cm$table)), 
       aes(x = actual, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Actual", y = "Predicted", title = "Confusion Matrix") +
  theme_bw()






# Use the predict() function to predict the categories of the testing data
predicted_categories <- predict(classifieur_knn)

# Use the table() function to create a confusion matrix
confusion_matrix <- table(predicted_categories, testing_data$categorie)

# Calculate the accuracy of the model
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Print the accuracy
print(paste0("Accuracy: ", round(accuracy, 2)))


library(ggplot2)

# Sélectionner la colonne "categorie de vehicule" et la convertir en facteur
categorie_col <- as.factor(training_data$categorie)
# Appliquer le one-hot encoding
one_hot_encoded <- predict(dummyVars(~ categorie, data = training_data), newdata = training_data)
#plots
hist(training_data$age, main = "Distribution de l'âge")
plot(training_data$age, training_data$categorie, main = "Relation entre l'âge et la catégorie de voiture", xlab = "Âge", ylab = "Catégorie")
barplot(table(training_data$categorie, training_data$sexe), main = "Relation entre le sexe et la catégorie de voiture", xlab = "Catégorie", ylab = "Nombre", col = c("blue", "pink"))
table(training_data$categorie, training_data$situationFamiliale)
library(ggplot2)

# Création du graphique avec ggplot
ggplot(training_data, aes(x = age, y = taux, color = categorie)) + 
  geom_point() + 
  labs(title = "Taux en fonction de l'âge et la catégorie de voiture",
       x = "Age",
       y = "Taux")

# Sélectionner les variables numériques pour la création des clusters
num_vars <- training_data[, c("age", "taux", "nbEnfantsAcharge")]
# Appliquer k-means pour créer 3 clusters
set.seed(123)
kmeans_model <- kmeans(num_vars, centers = 3)

# Ajouter les clusters attribués à chaque observation dans la colonne "cluster"
training_data$cluster <- kmeans_model$cluster
# Créer un graphique en nuage de points pour visualiser les clusters
library(ggplot2)
ggplot(training_data, aes(x = age, y = taux, color = as.factor(cluster))) +
  geom_point() +
  labs(x = "Age", y = "Taux", color = "Cluster")

#------#
#k-means#
#------#

library(cluster)
# Reads the dataset 'training_data' and take the 10 rows as sample
df<- sample(1:nrow(training_data), 100)
x<-training_data[df,]
class(x)
x$X2eme.voiture <- as.factor(x$X2eme.voiture)
str(x)



# Calculate the distance matrix
mat_dist <- daisy(x)

# Apply k-means clustering with k=3
kmeans_model <- kmeans(mat_dist, centers = 3)

# Calculate the within-cluster sum of squares (WSS)
wss <- sum(kmeans_model$withinss)

# Calculate the between-cluster sum of squares (BSS)
centroids <- kmeans_model$centers
distances <- dist(centroids, method = "euclidean")
bss <- sum(distances^2)/2

# Calculate the total sum of squares (TSS)
tss <- sum(mat_dist^2)/2

# Calculate the average silhouette width
sil <- silhouette(kmeans_model$cluster, mat_dist)
silhouette <- mean(sil[, "sil_width"])

# Print the WSS, BSS, TSS, and silhouette coefficient
cat("WSS: ", wss, "\n")
cat("BSS: ", bss, "\n")
cat("TSS: ", tss, "\n")
cat("Average silhouette width: ", silhouette, "\n")
# Create a scatterplot of the clustering results
plot(x, col = kmeans_model$cluster)

# Add centroids to the plot
points(kmeans_model$centers, col = 1:3, pch = 8, cex = 2)

#does't want to install the factoextra
install.packages("factoextra")
library(factoextra)

# Plot the results
fviz_cluster(kmeans_model, data = mat_dist, stand = FALSE,
             ellipse.type = "t", ellipse.level = 0.95, geom = "point",
             palette = "jco", ggtheme = theme_classic())



#------#
#R-PART#
#------#

library(rpart)

# Create the model using rpart
rpart_model <- rpart(categorie ~ ., data = training_data)

# Print the model summary
summary(rpart_model)

# Plot the tree
plot(rpart_model)
text(rpart_model)
# Create the model using rpart
rpart_model <-rpart(categorie ~ age + sexe + taux + situationFamiliale + nbEnfantsAcharge + X2eme.voiture,
                    data = training_data)

summary(rpart_model)
# Plot the tree
plot(rpart_model)
text(rpart_model)

# Selection d'attribut par Coefficient de Gini et effectif minimal d'un noeud de 10
tree_rp1 <- rpart(categorie~., training_data, parms = list(split = "gini"), control = rpart.control(minbucket = 10))
plot(tree_rp1)
text(tree_rp1, pretty = 0)

# Selection d'attribut par Coefficient de Gini et effectif minimal d'un noeud de 5
tree_rp2 <- rpart(categorie~., training_data, parms = list(split = "gini"), control = rpart.control(minbucket = 5))
plot(tree_rp2)
text(tree_rp2, pretty = 0)

# Selection d'attribut par Information Gain et effectif minimal d'un noeud de 10
tree_rp3 <- rpart(categorie~., training_data, parms = list(split = "information"), control = rpart.control(minbucket = 10))
plot(tree_rp3)
text(tree_rp3, pretty = 0)

# Selection d'attribut par Information Gain et effectif minimal d'un noeud de 5
tree_rp4 <- rpart(categorie~., training_data, parms = list(split = "information"), control = rpart.control(minbucket = 5))
plot(tree_rp4)
text(tree_rp4, pretty = 0)

training_data
#----------------------------------------------------------------#
# TEST DES DES CLASSIFIEURS rpart() ET CALCUL DES TAUX DE SUCCES #
#----------------------------------------------------------------#

# Application de tree_rp1 (identique a tree_rp3) a l'ensemble de test
test_rp1 <- predict(tree_rp1, testing_data, type="class")
# Calcul du taux de succes : nombre de succes sur nombre total d'exemples de test
print(taux_rp1 <- nrow(testing_data[testing_data$categorie==test_rp1,])/nrow(testing_data))

# Application de tree2 (identique a tree_rp4) a l'ensemble de test
test_rp2 <- predict(tree_rp2, testing_data, type="class")
# Calcul du taux de succes : nombre de succes sur nombre total d'exemples de test
print(taux_rp2 <- nrow(testing_data[testing_data$categorie==test_rp2,])/nrow(testing_data))

#----#
#TREE#
#----#

#--------------------------------------------------------------------#
# APPRENTISSAGE DES CLASSIFIEURS tree() AVEC DIFFERENTS PARAMETRAGES #
#--------------------------------------------------------------------#

# Affichage de l'aide 
? tree()
install.packages("tree")
library("tree")
classifieur_tree <-tree(categorie~age + sexe +taux+nbEnfantsAcharge+X2eme.voiture,training_data)
plot(classifieur_tree)
text(classifieur_tree)

# Apprentissage 1er paramétrage pour tree()
tree_tr1 <- tree(categorie~., training_data, split = "deviance", control = tree.control(nrow(training_data), mincut = 10))
plot(tree_tr1)
text(tree_tr1, pretty = 0)

# Apprentissage 2nd paramétrage pour tree()
tree_tr2 <- tree(categorie~., training_data, split = "deviance", control = tree.control(nrow(training_data), mincut = 5))
plot(tree_tr2)
text(tree_tr2, pretty = 0)

# Apprentissage 3eme paramétrage pour tree()
tree_tr3 <- tree(categorie~., training_data, split = "gini", control = tree.control(nrow(training_data), mincut = 10))
plot(tree_tr3)
text(tree_tr3, pretty = 0)

# Apprentissage 4eme paramétrage pour tree()
tree_tr4 <- tree(categorie~., training_data, split = "gini", control = tree.control(nrow(training_data), mincut = 5))
plot(tree_tr4)
text(tree_tr4, pretty = 0)

#----------------------------------------------------------------#
# TEST DES DES CLASSIFIEURS tree() ET CALCUL DES TAUX DE SUCCES #
#----------------------------------------------------------------#

# Test et taux de succes pour le 1er paramétrage pour tree()
test_tr1 <- predict(tree_tr1, testing_data, type="class")
print(taux_tr1 <- nrow(testing_data[testing_data$categorie==test_tr1,])/nrow(testing_data))

# Test et taux de succes pour le 2nd paramétrage pour tree()
test_tr2 <- predict(tree_tr2, testing_data, type="class")
print(taux_tr2 <- nrow(testing_data[testing_data$categorie==test_tr2,])/nrow(testing_data))

# Test et taux de succes pour le 3eme paramétrage pour tree()
test_tr3 <- predict(tree_tr3, produit_ET, type="class")
print(taux_tr3 <- nrow(produit_ET[produit_ET$Produit==test_tr3,])/nrow(produit_ET))

# Test et taux de succes pour le 4eme paramétrage pour tree()
test_tr4 <- predict(tree_tr4, produit_ET, type="class")
print(taux_tr4 <- nrow(produit_ET[produit_ET$Produit==test_tr4,])/nrow(produit_ET))



#----#
#RANDOM FOREST#
#----#


install.packages("randomForest")
library(randomForest)
# Train random forest model
rf_model <- randomForest(categorie~., training_data, ntree = 500)

# Predict on test data
rf_pred <- predict(rf_model, testing_data)

# Evaluate model performance
conf_mat <- table(rf_pred, testing_data$categorie)

# Evaluate the accuracy of the model
accuracy <- sum(diag(conf_mat))/sum(conf_mat)
cat("Accuracy:", round(accuracy, 3))

# Print the confusion matrix
table(rf_pred, testing_data$categorie)


# Predict on test data Marketing
rf_pred <- predict(rf_model, marketing)
# Evaluate model performance Marketing
conf_mat2 <- table(rf_pred, testing_data$categorie)

# Evaluate the accuracy of the model Marketing
accuracy <- sum(diag(conf_mat2))/sum(conf_mat2)
cat("Accuracy:", round(accuracy, 3))
# Print the confusion matrix
conf_mat <- table(rf_pred, marketing$categorie)
conf_mat
# Print the confusion matrix Marketing
table(rf_pred, marketing$categorie)
marketing

#----#
#SVM#
#----#
str(training_data)
str(testing_data)
# load e1071 package
library(e1071)
head(testing_data)
# fit SVM model on training_data
svm_model <- svm(categorie ~ ., data = training_data, kernel = "linear", cost = 1)

# fit SVM model on training_data with probability = TRUE
svm_model <- svm(categorie ~ ., data = training_data, kernel = "linear", cost = 1, probability = TRUE)

# make predictions on testing_data
svm_pred <- predict(svm_model, newdata = testing_data)
# make predictions on Marketing data 
svm_pred <- predict(svm_model, newdata = marketing)

# create confusion matrix
conf_mat <- table(svm_pred, testing_data$categorie)
# create confusion matrix Marketing
conf_mat <- table(svm_pred, marketing$categorie)

# print confusion matrix
print(conf_mat)


# Calculate the accuracy
accuracy <- mean(svm_pred == testing_data$categorie)
print(paste("Accuracy: ", accuracy))


# Calculate the accuracy Marketing
accuracy <- mean(svm_pred == marketing$categorie)
print(paste("Accuracy: ", accuracy))
