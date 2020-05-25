# Machines

Mon but dans ce projet est de simuler le fonctionnement de machines élémentaires. J'ai pour cela exploré plusieurs pistes. Premièrement j'ai implémenté en Ocaml un micro-processeur virtuel puis je me susi concentré sur des machiens plus simples, comem des machines de Turing ou un interpréteur ud langage Brainfuck.

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


# Machine de Turing
