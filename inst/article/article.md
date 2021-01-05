# shinydrive

Dans la famille des packages **shiny** chez **Datastorm**, je demande le petit dernier, à savoir **shinydrive**.

Le but de **shinydrive** est simple : faciliter le partage de fichiers entre différents utilisateurs, le tout avec un module à insérer en quelques lignes de code dans une application **R/shiny** existante. Le module disposant de deux rôles : 

 - **l'administrateur** qui peut de son côté ajouter / modifier / supprimer des fichiers disponibles ensuite en téléchargement, et créer si besoin également des sous-dossiers

 - **le lecteur** qui peut uniquement télécharger un ou plusieurs fichiers disponibles

Le fonctionnement du package est très simple : 

- Le module se branche à un répertoire existant
- La gestion des fichiers importés se fait ensuite *via* un fichier ``yaml`` permettant de stocker : 
    + le nom d'origine du fichier ainsi que sa date et heure de mise à disposition. Cela permet en effet de renommer les fichiers dont le répertoire de stockage en évitant les conflits dans le cas ou l'administrateur ajouterai deux fichiers portant le même nom. Cela est totalement transparent pour le lecteur qui retrouvera bien le fichier avec le nom d'origine lors du téléchargement
    + le type de fichier
    + une description optionnelle de son contenu

![img](../demo_app/www/figures/files.PNG)

Côté **shiny**, nous retrouvons une interface claire et épurée, disponible actuellement en trois langues (français, anglais et chinois), et qui se présente sous cette forme : 

**administateur**

*Vue des fichiers, avec la possibilité de les éditer / supprimer / télécharger* 

![img](../demo_app/www/figures/admin.PNG)

*Popup pour l'insertion d'un nouveau fichier* 

![img](../demo_app/www/figures/admin_2.PNG)


**lecteur**

*Uniquement la possibilité de télécharger les fichiers* 

![img](../demo_app/www/figures/utilisateur.PNG)

*N.B : La dernière colonne du tableau, composée de *checkbox*, permet de sélectionner plusieurs fichiers avant de tous les télécharger ou les supprimer (administrateur)*

### Installation

Le package devrait arriver dans les prochaines semaines sur le **CRAN**. En attendant, vous pouvez l'installer directement depuis notre gitub https://github.com/datastorm-open : 

``` r
if(!require(devtools)) install.packages("devtools")
devtools::install_github("datastorm-open/shinydrive")
```

### Application de démonstration

Une application de démonstration est disponible 

- directement dans le package : 

``` r
runApp(system.file("demo_app", package = "shinydrive"))
```

- en ligne à l'adresse suivante : https://datastorm-demo.shinyapps.io/shinydrive/
- et dans la vidéo ci-dessous

### Utilisation

En insérant tout simplement le module dans votre application **shiny**, avec l'utilisation des deux fonctions suivantes : 

- ``shiny_drive_ui`` : dans le script *ui.R*, avec uniquement l'identifiant du module à renseigner
- ``shiny_drive_server`` : dans la partie du *server.R* avec à minima :
    + l'identifiant du module (id)
    + ainsi que le répertoire de stockage (save_dir)
    
En complément, il est possible de : 

  + définir le rôle, administrateur ou lecteur (admin_user)
  + la langue (lan)
  + de forcer le remplissage du champ "description" lors de l'insertion d'un nouveau fichier (force_desc)
  + et de restreindre les accès à un sous-ensemble de sous-dossiers si nécessaire (dir_access)


``` r
require(shinydrive)

ui <- fluidPage(
    shiny_drive_ui(id = "idm")
)

server <- function(input, output, session) {
    callModule(module = shiny_drive_server,
             id = "idm",
             admin_user = TRUE,
             save_dir =  getwd(),
             lan = "EN")
}

shinyApp(ui, server)
```

Finalement, il est également possible d'utiliser de gérer les fichiers en dehors de l'application **shiny** (génération quotidienne d'un rapport *via* un batch par exemple, et mise à disposition automatique dans une application) en utilisant les trois fonctions suivantes :

- ``add_file_in_dir`` pour ajouter un nouveau fichier
- ``edit_file_in_dir`` pour éditer un fichier existant
- ``suppress_file_in_dir`` pour supprimer un fichier 
