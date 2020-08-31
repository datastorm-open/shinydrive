# shinyfilesmanager

## objectifs

Module shiny permettant : 

- à un "admin" : de déposer / éditer / supprimer des fichiers dans un répertoire donné
    + upload d'un fichier
    + mettre un nom "temporelle" pour la sauvegarde
    + une description
    + on stockerai le tout dans un .yml avec à minima par fichier : nom initial, nom temporelle (date heure milliseconde), date d'upload, description voir les informations de download

- à un lecteur : affichage de la table (DT) des fichiers dispos (nom initial, date upload, description) + possibilité de télécharger

## input du Module

- admin : 
    + upload de fichier
    + champs description
    + bouton ajouter

- pour tout le monde : 
    + une DT avec les fichiers dispos
    + un bouton par ligne de téléchargement
    + si admin : un bouton modifier et un bouton supprimer

## A regarder : 

- existant ?
- shinymanager pour les DT / css

https://github.com/datastorm-open/shinymanager/blob/master/R/module-admin.R

## gestion dossiers / sous-dossiers

- admin : 
        + créer un sous-dossier (depuis l"interface d'affichage ou lors de l'ajout) / renommer un sous-dossier / supprimer tout un dossier
- tous : filtre d'affichage d'un sous-dossier

code : https://gitlab.com/datastorm_projects/rte_app_data/-/blob/master/script/module_dataSave.R