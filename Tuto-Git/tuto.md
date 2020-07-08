# Tutorial Git et Github / master socstat

Git est un logiciel de gestion de version décentralisé. Concrètement, il va nous permettre de synchroniser nos fichiers de code pour éviter que des versions "R-script-final-final-12.R" se baladent dans le drive, et de garder un historique de toutes les modifications apportées aux scripts, en incluant le nom de celui qui apporte la modification, la date de modification, et des commentaires simples décrivant le contenu de la modification.  

Git étant simplement un logiciel de gestion, un client qui servira d'interface entre vous et le dépôt est nécessaire.  

## I) Les bases : créer un compte github et installer un client Git:

**Github**

Github est un service d'hébergements de dépôts git en ligne.

J'ai créé un dépôt pour le projet Télé-repas à l'adresse suivante:  https://github.com/tenroc/Tele-Repas-Socstat

Pour que je puisse vous accorder les droits de modification sur le dépôt, il vous faut créer un compte github à l'adresse suivante: https://github.com/  
Puis me donner votre nom d'utilisateur pour que je puisse vous ajouter comme collaborateur.  

**Git**  

Comme signalé plus haut, nous allons utiliser un client pour gérer la synchronisation du code entre les différents contributeurs.  
Personnellement j'utilise la ligne de commande, mais vous pouvez utiliser Gitcola qui est un client graphique simple, gratuit, open source et multiplateforme. Vous pouvez bien évidemment utiliser le client que vous voulez. En revanche je ne décrirais ci dessous que le fonctionnement de Gitcola. Si vous voulez utiliser autre chose, vous devrez trouver la doc par vous même.  

En gros, le principe de Git est que tout les fichiers créés sont conservés dans un dépôt en ligne, que vous clonez sur votre machine (vous posséderez alors un dossier identique au dépôt, contenant tous les fichiers inclus dans celui ci). Toutes les modifications que vous effectuerez par la suite sur les fichiers stockés localement sur votre ordinateur pourront ensuite être synchronisée avec le dépôt en ligne, via le client git. De la même manière, toutes les modifications effectués par les contributeurs du projet seront synchronisée avec vos fichiers locaux.

Liste de ressources pour trouver un client git:  

* Client en ligne de commande: https://git-scm.com/
* Gitkraken: https://www.gitkraken.com/
* Site référençant les clients graphiques: https://git-scm.com/download/gui/
* Gitcola: https://git-cola.github.io/

## II) Cloner et ouvrir un dépôt:

**Gitcola**  

Gitcola est un client graphique pour Git. Avant toute chose, il vous faut cloner votre dépot (hébergé sur un serveur git, tel que github ou bitbucket). Cliquez sur cloner, puis collez l'adresse de votre dépôt.

![ScreenShot](https://github.com/tenroc/Tele-Repas-Socstat/blob/master/Tuto-Git/screenshots/gitco_2.png)
Ce qui dois apparaître à l'écran doit a peu près ressembler à ça:

![ScreenShot](https://github.com/tenroc/Tele-Repas-Socstat/blob/master/Tuto-Git/screenshots/gitco_1.png)

## III) Les bases de la gestion de dépôt:

Les bases tiennent en 4 principes de base, qui sont aussi des commandes git:

* *Pull* (ou "tirer": synchronise vos fichiers locaux (ceux sur votre ordinateur) avec les fichiers qui sont sur le dépôt en ligne. vérifiez bien d'avoir toujours la dernière version des scripts avant de commencer à travailler dessus.
* *Commit*: Quand vous avez fini de travailler sur un bout de code, il vous faut "commit" ce code. En gros vous signifiez à git que vos changements sont terminés et prêts à être intégré a la version principale du script. Les commits sont l'unité de base de la gestion de projet via git: l'historique des changements est basé sur les commits, un commentaire est attaché a chaque commit, etc. Il est par ailleurs en cas d'erreur possible de revenir à un version antérieure du script, tous les changements listés dans le commit supprimé sont alors annulés dans le code. Pour ces raisons il est important de faire des commits réguliers: Préférer faire un commit cohérent par paragraphes de code écrit, lié a un commentaire décrivant précisément les changements, plutôt qu'un commit en fin de session de travail, comportant des changements sur des parties de code multiples et très différentes.
* *Push* (ou "pousser"): synchronise vos fichiers locaux avec les fichiers qui sont sur le dépôt en ligne. L'inverse de pull.
* *Branch*: Créée une branche de code dérivée du code original: concrètement, à considérer comme une version alternative du code principal. Les deux versions sont indépendantes, elles peuvent être synchronisées entre elles, mais ce n'est pas obligatoire. Utile pour faire des tests, ou écrire des bouts de code spécifiques à une analyse particulière (ACM, etc.).

En règle générale: vous commencez, avant de travailler, par un "Pull", vous modifiez les fichiers que vous voulez modifier, vous faites un "commit" pour que git prenne en compte vos changements, puis vous faites un "push" pour publier vos changements. Il est parfois nécessaire de refaire un "Pull" avant un "Push" si des fichiers dans le dépôts ont étés modifiés par quelqu'un d'autre entre temps.

**Attention:** La modification simultanée par deux personnes différentes du même fichier entraîne un "conflit" de fichier, qu'il va falloir résoudre. Pour ça, des outils existent: le gestionnaire de différences. Il compare les différences entre deux versions d'un fichiers, vous pouvez ensuite les intégrer ou non dans une version unique en copiant/collant des bouts de texte, avant de signifier à git que le fichier est propre, et qu'il peut l'intégrer au dépôt.

**Gitcola**

Gitkraken vous affiche en temps réel à la fois les fichiers locaux qui ont étés modifiés depuis votre dernier commit, et les fichiers dans le dépôt qui ont été modifiés depuis votre dernier pull.
La modification par deux contributeurs différents d'un même bout de script peut créer un conflit, il est parfois plus prudent d'effectuer les changements désirés sur une branche différente de la branche principale, qui pourra être reversée dans la branche principale par la suite. De même, il est toujours conseillé d'effectuer un Pull avant d'effectuer un Push.

Les changements que vous effectuez sont signifiés de la manière suivante:

![ScreenShot](https://github.com/tenroc/Tele-Repas-Socstat/blob/master/Tuto-Git/screenshots/gitco_1.png)

* A gauche, les fichiers que vous avez modifiés sur votre machine (unstagged files, "non-suivis" ou "modifiés") qui n'ont pas été sélectionnés pour être intégrés à un commit.
* Les fichiers que vous avez modifiés sur votre machine (stagged files ou "pré-comittés", avec un triangle vert) que vous avez sélectionnés pour être intégrés à un commit. (vous pouvez sélectionner des fichiers en les sélectionnant, puis clic-droit -> "pré-commit")
* La fenêtre de commentaire en haut / centre: Vous y écrivez le titre et commentaire qui sera attaché à votre commit (les changements que vous avez réalisé dans le code).
* A droite, les différentes branches de votre dépôt.
* Au centre / bas: les modifications effectuées par les différents commits (ou diffs): très utiles, elle vous permettent de savoir ce qui a été changé dans les fichiers par un commit spécifique. Pour qu'elles apparaissent, sélectionnez un commit en haut à gauche.

Pour effectuer une action:

- Le bouton commit vous permet de créer un commit, après que vous ayez sélectionné des modifications à intégrer dans le commit (les fameux "pré-commits") et renseigné un titre.
- Actions -> Tirer (Pull) va synchroniser votre dossier local avec le serveur.
- Actions -> Pousser va synchroniser le serveur avec vos commits locaux (on ne peut pas verser de modifications dans le dépôt sans passer par l'étape commit).
- Après avoir cliqué sur push / pull, une fenêtre s'ouvre pour vous demander dans / depuis quelle branche vous voulez verser / télécharger vos commits (par défault origin / master), suite à quoi gitcola vous demande vos identifiants + mot de passe pour accéder au dépôt:

![ScreenShot](https://github.com/tenroc/Tele-Repas-Socstat/blob/master/Tuto-Git/screenshots/gitco_4.png)

Affichage -> DAG vous permet de visualiser l'historique des commits (qui a modifié quoi, quand?) et des branches.

![ScreenShot](https://github.com/tenroc/Tele-Repas-Socstat/blob/master/Tuto-Git/screenshots/gitco_6.png)

Y sont représentés un historique, avec noms et commentaires, de tous les changements effectués sur le code. Y apparaissent aussi toutes les branches ouvertes et fusionnées de l'arborescence. Vous ne pouvez travailler que sur une branche à la fois, et vos fichiers locaux seront automatiquement adaptés en conséquence lorsque vous faites "Pull".

Sur la fenêtre du DAG apparaissent:

- Une liste des commits, avec auteur et titre (haut / gauche)
- Une liste des modifications effectuées par un commit (différence, bas / gauche), qui apparaît en cliquant sur le commit voulu
- Une représentation graphique des branches et commits (haut / droite)
- Une liste des fichiers modifiés (bas / droite)

Pour quelques notions sur les branches et l'arborescence: regarder la vidéo d'introduction de 2 minutes publiée par gitkraken (en anglais): https://www.youtube.com/watch?v=j1rP21RcbH0  
Si tous ça n'est pas clair, je devrais pouvoir a peu prés gérer les différentes branches de notre dépôt, donc tout vas bien. Le plus important à comprendre restant comment commiter et pusher votre code.

## Conclusion: ##

Si tout ça vous paraît affreusement compliqué, ou qu'il y a quelque chose qui n'est pas clair, écrivez moi un mail, ou ouvrez un bug report sur github.  
Si vous n'avez absolument rien compris, sachez qu'il est aussi possible que j'administre (ou quelqu'un d'autre) le dépôt, et donc que tous les changements soient validés avant d'être pris en compte, ça demande tout de même d'intégrer les bases décrites plus haut, mais ça évite que qui que ce soit fasse une bêtise. ça pourrait aussi éventuellement permettre de reviewer le code avant de l'accepter.  
Le tuto est dans le dépôt github du cours, et est écrit en markdown, vous pouvez le modifier comme vous voulez.  

Quelques ressources supplémentaires:  

* https://guides.github.com/activities/hello-world/
* http://stackoverflow.com/questions/tagged/github <- pour obtenir de l'aide
