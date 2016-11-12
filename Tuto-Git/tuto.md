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
Personellement j'utilise Gitkraken, qui est un client graphique simple, gratuit, et multiplateforme. Vous pouvez cependant utiliser le client que vous voulez. En revanche je ne décrirais ci dessous que le fonctionnement de Gitkraken, du client web de Github et du client en ligne de commande. Si vous voulez utiliser autre chose, vous devrez trouver la doc par vous même.  

En gros, le principe de Git est que tout les fichiers créés sont conservés dans un dépôt en ligne, que vous clonez sur votre machine (vous posséderez alors un dossier identique au dépôt, contenant tous les fichiers inclus dans celui ci). Toutes les modifications que vous effectuerez par la suite sur les fichiers stockés localement sur votre ordinateur pourront ensuite être synchronisée avec le dépôt en ligne, via le client git (voir section Gitkraken, Git comand line et Client web Github). De la même manière, toutes les modifications effectués par les contributeurs du projet seront synchronisée avec vos fichiers locaux.

Liste de ressources pour trouver un client git:  

* Client en ligne de commande: https://git-scm.com/
* Gitkraken: https://www.gitkraken.com/
* Site référençant les clients graphiques: https://git-scm.com/download/gui/linux

## II) Cloner et ouvrir un dépôt:

**GitKraken**  

Gitkraken est un client graphique pour Git.  
Un login est demandé la première fois que vous ouvrez le programme, il est possible de s'identifier via le compte github que vous venez de créer en cliquant sur l'option *login with github*.  
Une fois que vous vous êtes identifiés, il vous faut générer une clé ssh. Cliquez sur les trois lignes en haut à droite de la fenêtre, puis sur "préférences". Une fois dans les préférences, cliquez sur "authentification", puis github.com, entrez de nouveau vos identifiants github en cliquant sur "connect to github" si besoin, puis cliquez sur "Generate SSH key and add to GitHub".  

![ScreenShot](https://github.com/tenroc/Tele-Repas-Socstat/blob/master/Tuto-Git/screenshots/screen1.png)

Une fois la clé ssh ajoutée a votre compte Github, vous pouvez quitter les préférences.  
Une fois revenus sur l'écran principal, cliquez sur "File", puis sur "Clone repo".  
Allez sur l'onglet Github, puis, cliquez sur Télé-repas-socstat (qui apparaît normalement si je vous ai ajouté en tant que collaborateurs, sinon vous pouvez normalement ajouter manuellement l'url du dépôt: https://github.com/tenroc/Tele-Repas-Socstat). Vous pouvez aussi modifier le chemin du dossier clone du dépôt qui sera créé sur votre machine dans le champ "Where to clone to" et "Full path".

![ScreenShot](https://github.com/tenroc/Tele-Repas-Socstat/blob/master/Tuto-Git/screenshots/screen2.png)

Confirmez en cliquant sur "clone the repo!", Attendez la fin du clonage, puis cliquez sur le bandeau qui viens d'apparaître: "Open now".  
Ce qui dois apparaître à l'écran doit a peu près ressembler à ça:

![ScreenShot](https://github.com/tenroc/Tele-Repas-Socstat/blob/master/Tuto-Git/screenshots/screen3.png)

**Git**

WIP

## III) Les bases de la gestion de dépôt:

Les bases tiennent en 4 principes de base, qui sont aussi des commandes git:

* Pull: synchronise vos fichiers locaux (ceux sur votre ordinateur) avec les fichiers qui sont sur le dépôt en ligne. vérifiez bien d'avoir toujours la dernière version des scripts avant de commencer à travailler dessus.
* Commit: Quand vous avez fini de travailler sur un bout de code, il vous faut "commit" ce code. En gros vous signifiez à git que vos changements sont terminés et prêts à être intégré a la version principale du script. Les comits sont l'unité de base de la gestion de projet via git: l'historique des changements est basé sur les commits, un commentaire est attaché a chaque commit, etc. Il est par ailleurs en cas d'erreur possible de revenir à un version antérieure du script, tous les changements listés dans le comit supprimé sont alors annulés dans le code. Pour ces raisons il est important de faire des commits réguliers: Préférer faire un commit cohérent par paragraphes de code écrit, lié a un commentaire décrivant précisément les changements, plutôt qu'un commit en fin de session de travail, comportant des changements sur des parties de code multiples et trés différentes.
* Push: synchronise vos fichiers locaux avec les fichiers qui sont sur le dépôt en ligne. L'inverse de pull.
* Branch: Créée une branche de code dérivée du code original: concrètement, à considérer comme une version alternative du code principal. Les deux versions sont indépendantes, elles peuvent être synchronisées entre elles, mais ce n'est pas obligatoire. Utile pour faire des tests, ou écrire des bouts de code spécifiques à une analyse particulière (ACM, etc.).

En règle générale: vous commencez, avant de travailler, par un "Pull", vous modifiez les fichiers que vous voulez modifier, vous faites un "commit" pour que git prenne en compte vos changements, puis vous faites un "push" pour publier vos changements. Il est parfois nécessaire de refaire un "Pull" avant un "Push" si des fichiers dans le dépôts ont étés modifiés par quelqu'un d'autre entre temps.

**Gitkraken**

Gitkraken vous affiche en temps réel à la fois les fichiers locaux qui ont étés modifiés depuis votre dernier commit, et les fichiers dans le dépôt qui ont été modifiés depuis votre dernier pull.
La modification par deux contributeurs différents d'un même bout de script peut créer un conflit, il est parfois plus prudent d'effectuer les changements désirés sur une branche différente de la branche principale, qui pourra être reversée dans la branche principale par la suite. De même, il est toujours conseillé d'effectuer un Pull avant d'effectuer un Push.

Les changements que vous effectuez sont signifiés de la manière suivante (il faut cliquer sur la ligne WIP de la fenêtre principale pour que le tiroir s'affiche):

![ScreenShot](https://github.com/tenroc/Tele-Repas-Socstat/blob/master/Tuto-Git/screenshots/screen4.png)

Le tiroir situé à droite de l'écran vous indique, de haut en bas:  

* Les fichiers que vous avez modifiés sur votre machine (unstagged files) qui n'ont pas été sélectionnés pour être intégrés à un commit.
* Les fichiers que vous avez modifiés sur votre machine (stagged files) que vous avez sélectionnés pour être intégrés à un commit. (vous pouvez sélectionner des fichiers en les cochant)
* La fenêtre de commentaire: Vous y écrivez le commentaire qui sera attaché à votre commit (les changements que vous avez réalisé dans le code). Summary représente le titre (ce qui apparaitra dans l'arborescence), Description le corps de texte.

La fenêtre située au milieu de l'écran représente l'arborescence des commits:

![ScreenShot](https://github.com/tenroc/Tele-Repas-Socstat/blob/master/Tuto-Git/screenshots/screen5.png)

Y sont représentés un historique, avec noms et commentaires, de tous les changements effectués sur le code. Y apparaissent aussi toutes les branches ouvertes et mergés de l'arborescence. Vous ne pouvez travailler que sur une branche à la fois, et vos fichiers locaux seront automatiquement adaptés en conséquence lorsque vous faites "Pull".
Pour quelques notions sur les branches et l'arborescence: regarder la vidéo d'introduction de 2 minutes publiée par gitkraken (en anglais): https://www.youtube.com/watch?v=j1rP21RcbH0  
Si tous ça n'est pas clair, je devrais pouvoir a peu prés gérer les différentes branches de notre dépôt, donc tout vas bien. Le plus important à comprendre restant comment commiter et pusher votre code.  

Apparaissent en grisé et en haut de l'écran, précédé de la mention WIP, les changements effectués par d'autres personnes, qui ont été publiés dans le dépôt, et que vous n'avez pas encore synchronisés avec vos fichiers locaux, ou les changements que vous avez effectués, et qui n'ont pas encore été pris en compte:

![ScreenShot](https://github.com/tenroc/Tele-Repas-Socstat/blob/master/Tuto-Git/screenshots/screen6.png)

Les icones à gauche des dernières lignes vous indiquent ou prennent place les changements, l'ordinateur pour les changements que vous avez effectués en local, la seconde icône pour les changements qui ont été effectués dans le dépôt. Lorsque les deux icônes apparaissent simultanément sur la dernière ligne, vos fichiers locaux sont synchrones avec ceux sur le dépôt.

![ScreenShot](https://github.com/tenroc/Tele-Repas-Socstat/blob/master/Tuto-Git/screenshots/screen7.png)

Si vous cliquez sur un commit dans l'arborescence: vous aurez accès au commentaire associé, ainsi qu'au nom de la personne qui l'a publié.

![ScreenShot](https://github.com/tenroc/Tele-Repas-Socstat/blob/master/Tuto-Git/screenshots/screen8.png)

Enfin, les icones en haut de l'écran vous permettent, de gauche à droite:  

![ScreenShot](https://github.com/tenroc/Tele-Repas-Socstat/blob/master/Tuto-Git/screenshots/screen9.png)

* D'annuler le dernier commit que vous avez réalisé
* De le restaurer
* D'effectuer un Pull (synchroniser vos fichiers locaux avec ceux dans le dépôt)
* D'effectuer un Push (Publier vos commits dans le dépôt)
* De créer une nouvelle branche

**Github Web client**

WIP

## Conclusion: ##

Si tout ça vous paraît affreusement compliqué, ou qu'il y a quelque chose qui n'est pas clair, écrivez moi un mail, ou ouvrez un bug report sur github.  
Si vous n'avez absolument rien capté, sachez qu'il est aussi possible que j'administre (ou quelqu'un d'autre) le dépôt, et donc que tous les changements soient validés avant d'être pris en compte, ça demande tout de même d'intégrer les bases décrites plus haut, mais ça évite que qui que ce soit fasse une bêtise. ça pourrait aussi éventuellement permettre de reviewer le code avant de l'accepter.  
Le tuto est dans le dépôt github du cours, et est écrit en markdown, vous pouvez le modifier comme vous voulez.  

Quelques ressources supplémentaires:  

* https://guides.github.com/activities/hello-world/
* http://stackoverflow.com/questions/tagged/github <- pour obtenir de l'aide
