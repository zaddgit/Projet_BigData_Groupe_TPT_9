#  Projet Big Data Analytics avec les Lacs de Données et les tables externes : BDA/DL

Projet Big Data et d’Analyse de la Clientèle d'un Concessionnaire Automobile pour la Recommandation de Modèle

DESCRIPTION DU PROJET

Contexte et objectifs

Vous avez été contacté par un concessionnaire automobile afin de l'aider à mieux cibler les véhicules susceptibles d'intéresser ses clients. Pour cela il met à votre disposition :
-Son catalogue de véhicules
-Son fichier clients concernant les achats de l'année en cours
-Un accès à toutes les informations sur les immatriculations effectuées cette année
-Une brève documentation des données Un vendeur (voir son interview ci-dessous)

Votre client sera satisfait si vous lui proposez un moyen afin :
-Qu'un vendeur puisse en quelques secondes évaluer le type de véhicule le plus susceptible d'intéresser des clients qui se présentent dans la concession
-Qu'il puisse envoyer une documentation précise sur le véhicule le plus adéquat pour des clients sélectionnés par son service marketing (voir ci-dessous)

Documentation des données
Les fichiers de données à votre disposition vous sont décrits dans les tables ci-dessous. Pour chaque attribut du fichier, vous sont donnés son nom, son type (numérique, caractères, catégoriel ou booléen) sa description et son domaine de valeurs. Certains attributs peuvent comporter des valeurs manquantes ou incorrectes (erreur de saisie par exemple). Celles-ci sont représentées par une cellule vide ou bien contenant une valeur hors du domaine de valeurs de la variable (valeurs « ? », « » ou « N/D » par exemple).

Informations données par le vendeur
« Les différents véhicules de notre catalogue répondent à des besoins différents. Certains sont petits afin de mieux circuler en ville, d'autres ont de l'espace pour transporter toute une famille tandis que certains sont plus puissants et destinés à une clientèle plus fortunée. Nous souhaitons définir différentes catégories de véhicules afin de mieux comprendre les désirs des clients et proposer aux nouveaux clients le véhicule le plus adapté à leurs besoins. ».

Analyse des Données par Data Mining, Machine Learning et Deep Learning et Activités
L’objectif est de construire un modèle de prédiction de la catégorie de véhicules (ou du modèle de véhicule) la plus susceptible de convenir à un client en fonction de ses caractéristiques (âge, sexe, statut social, nombre d’enfants, etc.). 

Les principales étapes consisteront :
• Répartir les véhicules et/ou les clients en différentes catégories correspondant chacune à différents besoins. 
• Mettre au point un modèle de prédiction de la catégorie de véhicules qui répondent aux besoins des clients à l’aide des approches de classification supervisée. Mettez en application une méthodologie de gestion de projet et établissez un plan de mise en œuvre du projet :
décrire le processus de mise en œuvre, de la sélection des données jusqu’à la détermination de l’algorithme de classification supervisée, utilisé pour prédire la catégorie de véhicules la plus adaptée au client, le plus performant et établir un plan de mise en œuvre à partir de ce cycle.


LES PARTIE TRAITER :
 * L’alimentation du data lake
 * HADOOP MAP REDUCE sur le fichier CO2 
 * Techniques de Data Visualisation du sous projet BDA/DL AVEC TABLEAU 
 * Analyse des Données du sous projet BDA/DL par les Techniques de Data Mining, Machine Learning et Deep Learning
 1) Analyse exploratoire des données :
 2) Identification des catégories de véhicules
 3) Application des catégories de véhicules définies aux données des Immatriculations
 4) Fusion des données Clients et Immatriculations
 5) Création d’un modèle de classification supervisée pour la prédiction de la catégorie de véhicules
 6) Application du modèle de prédiction aux données Marketing
