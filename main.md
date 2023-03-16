---
title: "Categorical sequences analysis"
output: 
  pdf_document:
    extra_dependencies: ["amsmath"]
---

## Optimal Matching

Optimal Matching Analysis (OMA) is a technique used in social sciences for the comparison of sequences with applications on different areas, in particular, life course and career path analysis.

Given two sequences, it is possible to transform one sequence into another using a set of operations on the states: insert, delete and replace. Numerical values are assigned to each of this operations (depending on the states involved) and are defined in a **cost matrix**. As a result, pairwise distances between the sequences are obtained which can be used to apply a clustering method.

## The algorithm

Given a set of $k$ states, say, $S = \{s_1, \dots, s_k\}$ a sequence of size $t > 0$ can be denoted as $X = (x_1, \dots, x_t)$, where $x_i \in S$ for $i = 1, \dots, t$. Also, the set of all possible sequences with states belonging to $S$ is denoted by $\mathbf{S}$.

Let $X, Y \in \mathbf{S}$ be two sequences of size $t_1$ and $t_2$, respectively, that we want to align. In order to align the sequences, we define a matrix $F$ of size $t_1 \times t_2$ and fill it as follows:

\begin{equation}
\tag{ini1}
F(1, 1) = 0
\end{equation}

\begin{equation}
\tag{ini2}
F(i, 1) = F(i-1, 0) - c \text{ for } i = 2, \dots, t_1
\end{equation}

\begin{equation}
\tag{ini3}
F(1, j) = F(1, j-1) - c \text{ for } j = 2, \dots, t_2
\end{equation}

\begin{equation}
\tag{iter}
F(i, j) = max \left{ F(i-1, j)-d, F(i, j-1)-d, F(i-1, j-1)+ \right}
\end{equation}

Then, the following operations $a_*:\mathbf{S} \mapsto \mathbf{S}$ can be defined:

-   Insert: $a_{x',i}^{I}(X) = (x_1, \dots, x_i, x', x_{i+1}, \dots, x_t)$
-   Delete: $a_{x_i}^{D}(X) = (x_1, \dots, x_{i-1}, x_{i+1}, \dots, x_t)$
-   Replace: $a_{x_i, x'}^{R}(X) = (x_1, \dots, x_{i-1}, x', x_{i+1}, \dots, x_t)$

Furthermore, a cost $c(a_*) \geq 0$ is associated with each operator and ,given two sequences $X_1$ and $X_2$, it is possible to arrive to $X_2$ from $X_1$ applying a sequence of operations $A = a_1, \dots, a_n$ and hence the cost associated with this transformation is defined as $c(A) = \sum_{i=1}^n c(a_i)$.

However, there might be more than one sequence $A$ that transforms $X_1$ into $X_2$, hence the distance d($X_1$, $X_2$) between the two sequences is defined by:

$$ d(X_1, X_2) = min_A \{c(A): X_2 = A(X_1)\} $$

This algorithm is based

### Example


## About the data

As part of the Women 40+ Healthy Aging Study, a large study that was conducted by the Department of Clinical Psychology and Psychotherapy of the University of Zurich, a psychometric instrument was developed in order to obtain information about the history of romantic relationships of women. The study was conducted between June 2017 and February 2018 with women between 40 and 75 years who (self-)reported good, very good or excellent health condition and the absence of acute or chronic somatic disease or mental disorder. The participants who reported psychotherapy or psychopharmacological treatment in the previous 6 months were excluded as well as habitual drinkers. Other exclusion criteria were pregnancy in the last 6 months, premature menopause, surgical menopause, intake of hormonal treatment (including contraceptives), shift-work and recent long-distance flight. The participants were recruited from the general population using online advertisement and flyers.

The questionnaire asked the participants to provide information about relationship phases starting from the age of 15 years until the current age at the time of the data collection. The phases were defined by the start and end age and for each phase and information about civil status, relationship status, living situation, children and quality of the relationship was collected. Before including the data corresponding to their own history, the participants were prompted to answer some of the questions based on an example. Some of the participants were excluded when the example entries were not correctly filled. In total 250 individuals were considered in the analysis.

In order to create a sequence for each participant the information about civil status, relationship status and *somethin else?* is taken into account. A yearly sequence is created and the states considered are the following:

*Here goes a table with numerical codes and corresponding state*

Optimal matching analysis is performed with the aim to obtain clusters of sequences that are similar and characterize the most common relationship history profiles.
