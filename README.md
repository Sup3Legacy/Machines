# Machines (voir ```machine.ml```)

Mon but dans ce projet est de simuler le fonctionnement de machines élémentaires. J'ai pour cela exploré plusieurs pistes. Premièrement j'ai implémenté en Ocaml un micro-processeur virtuel puis je me suis concentré sur des machines plus simples, comme des machines de Turing ou un interpréteur ud langage Brainfuck.

# Simulation de micro-processeur

Pour commencer, j'ai mis en place les différents types de données dont j'aurais besoin. Les types sont : Empty, Int, Float, Inst (instruction machine), Char et Add (adresse mémoire).

Après, j'ai implémenté les différentes instruction de bases, comme la lecture de la mémoire, le chargement en registre et les opérations numériques élémentaires.

Pour simplifier la structure, au lieu de choisir une architecture utilisant une pile pour les données et les instruction, j'ai opté pour architecture mémoire + registre. La mémoire contient les instructions et le registre les données qui sont traitées.

Tel quel, le micro-processeur est déjà en mesure de calculer des fonctions itératives (grâces aux instructions de conditions et de saut), comme par exemple la fonction factorielle :

```ocaml
I 1;
IIN (Reg 0);
INC (Reg 0);
LOAD (Mem 0, Reg 1);
LOAD (Mem 0, Reg 2);
ILT (Reg 3, Reg 2, Reg 0);
JUMPIF (Reg 3, Mem 7, Mem 10);
IMULT (Reg 1, Reg 1, Reg 2);
INC (Reg 2);
JUMP (Mem 5);
IOUT (Reg 1);
HALT
```

# Langage et compilateur

Bien entendu, ce langage machine, bien qu'_a priori_ fonctionnel, n'est pas pratique à utiliser. C'est pourquoi j'ai commencé l'implémentation d'un compilatuer, capable de compiler un langage de haut niveau, le _Bisac_, langage fortement typé, en ce langage machine virtuel.

Cette partie du projet est pour l'instant en stand-by, m'étant rendu compte que j'ai besoin d'acquérir  certaines connaissances (par exemple la façon dont est implémenté un parser, ou bien les solutions possibles pour gérer des expressions complexes. En effet, lors de l'affectation :

```
a = 3 * (b / 2) + c;
```

Chaque opération intermédiaire doit être temporairement stockée dans le registre et je ne sais pas encore comment gérer les déplacement du pointeur dans ces situations.


Néanmoins, j'ai obtenu quelques premiers résultats avec un compilateur simplifié. Ce compilateur ne sait pour l'instant compiler que du code comportant des affectations simples et des commandes d'affichage à l'écran.

Par exemple, le code

```
int a = 0;
int b = 42;;
string foo = "bar";

print a;
inc a;
print a;

print b;
dec b;
print b;

print foo;
```

peut être compilé en le langage machine virtuel puis exécuté.

Pour gérer les variables, j'utilise une approche statique, où chaque variable distincte rencontrée lors de la compilation se voit attribué une case du registre. Ensuite, lorsque la variable est à nouveau rencontrée, l'opération est remplacée par l'instruction, avec en argument un pointeur vers la case du registre attribuée à la variable.
Cette approche a l'avantage d'être simple mais l'inconvénient de nécessité que toutes les variables, et toutes les cases mémoire utilisées pendant l'éxecution du programme soient reconnues et listées lors de la compilation.


# Machines V2 (voir ```machine V2.ml```)

J'ai ensuite tenté une deuxième approche. Au lieu d'utiliser un compilateur naïf et limité, mais simple, je suis en train d'implémenter un compilateur plus complexe mais plus général, permettant de créer un langage de haut niveau complet. 

Ce compilateur utilise une structure "classique", c'est-à-dire : code -> code 'nettoyé' et mis en forme -> tokens -> arbre syntaxique de tokens -> code en langage machine (Dans mon cas, je pense qu'il n'y a pas besoin de langage intermédiaire entre l'arbre syntaxique et le langage machine).

À ce jour (fin juin 2020), j'ai implémenté le tokenizer, qui transforme un code en ```Bisac``` de la forme :

```
int a = 0;
float b = 1.2;
string abc = 'isse';
print a;
inc a;
print a;
dec a;
print a;
print abc;
int c = ( a + a ) * a;
```

en un tableau de listes de tokens : 

```ocaml
[|
[INT; VARIABLE "a"; AFFECT; CONSTINT 0];
[FLOAT; VARIABLE "b"; AFFECT; CONSTFLOAT 1.2];
[STRING; VARIABLE "abc"; AFFECT; CONSTSTRING "isse"];
[FUNCTION "print"; VARIABLE "a"]; 
[FUNCTION "inc"; VARIABLE "a"];
[FUNCTION "print"; VARIABLE "a"]; 
[FUNCTION "dec"; VARIABLE "a"];
[FUNCTION "print"; VARIABLE "a"]; 
[FUNCTION "print"; VARIABLE "abc"];
[INT; VARIABLE "c"; AFFECT; PARO; VARIABLE "a"; PLUS; VARIABLE "a"; PARC; MULT; VARIABLE "a"]
|]
```
