## FP and Scala (premise to BigData.be's session on Spark)

# Synopsis
In this talk, we'll see why and when the Scala Programming Language has been chosen by Big data companies or technologies.
Based on that, I'll elaborate some experiments illustrating these choices concretely.
Thus, the main focus will be put on expressiveness (or functional style and laziness).
At the end of the talk, you should be able to invent Spark by yourself (conceptually at least...).


# Intro: 5'
-----
Scala devient de plus important pour plusieurs raisons:
* reactive
* data analysis at scale

Pourquoi Scala et pas X|Y:
* jvm!
* familier pour les devs python, java, ruby, ...

# Who: 5'
---
sociétés => reprendre une partie du talk au Yajug

# FP?: 15'
---
définir une fonction en math: In=>Out
une fonction est facilement combinable
une fonction peut retrouner une fonction ou peut-être un paramètre
Exemples:
Générateur de data set: Y = ß0 + ß1*X1 + e
Estimer ß0 et ß1


# Lazy: 15'
----
Concept de stream et de future => reactive
Examples:
Future.map
Future.flatMap
For-comprehension


# Mashup: 5'
------
une fonction est un object => first class citizen
donc on peut l'envoyer ou la stocker
on peut donc envoyer des données vers des fonctions, ou des fonctions vers des données
sans effets de bord, retenir la chaine des fonctions appliquées à des entrées évite de devoir stocker la sortie (voir event sourcing, command sourcing)
Envoyer des données vers des fonctions => Akka (MPS)
Envoyer des fonctions vers des données => Spark