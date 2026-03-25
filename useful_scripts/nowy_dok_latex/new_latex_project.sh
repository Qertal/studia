#!/bin/bash

if [ -z "$1" ]; then
    echo "Użycie: new_latex_project.sh NAZWA_PROJEKTU"
    exit 1
fi

PROJECT_NAME="$1"
# TARGET_DIR="$HOME/projekty/$PROJECT_NAME"
TARGET_DIR="./$PROJECT_NAME"

if [ -d "$TARGET_DIR" ]; then
    echo "Folder $TARGET_DIR już istnieje."
    exit 1
fi

mkdir -p "$TARGET_DIR"/{sections,images,build}

cat > "$TARGET_DIR/main.tex" <<'EOF'
\documentclass[11pt,a4paper]{article}

% ===== Kodowanie i język =====
\usepackage[T1]{fontenc}
\usepackage[utf8]{inputenc}
\usepackage[polish]{babel}
\usepackage{lmodern}

% ===== Układ strony =====
\usepackage[a4paper,margin=2.5cm]{geometry}

% ===== Matematyka =====
\usepackage{amsmath, amssymb, amsfonts, amsthm, mathtools}

% ===== Grafika i rysunki =====
\usepackage{graphicx}
\usepackage{float}
\usepackage{xcolor}
\usepackage{tikz}

% ===== Algorytmy =====
\usepackage{algorithm}
\usepackage{algpseudocode}

% ===== Listy =====
\usepackage{enumitem}

% ===== Linki =====
\usepackage{hyperref}

% ===== Kod źródłowy =====
\usepackage[outputdir=build]{minted}

% ===== Cytowania i bibliografia =====
\usepackage{csquotes}
\usepackage[backend=biber,style=numeric]{biblatex}
\addbibresource{bibliography.bib}

% ===== Skróty matematyczne =====
\newcommand{\R}{\mathbb{R}}
\newcommand{\Z}{\mathbb{Z}}
\newcommand{\N}{\mathbb{N}}
\newcommand{\Q}{\mathbb{Q}}
\newcommand{\C}{\mathbb{C}}
\newcommand{\D}{\mathcal{D}}
\newcommand{\F}{\mathbb{F}}

% ===== Środowiska twierdzeń =====
\theoremstyle{plain}
\newtheorem{tw}{Twierdzenie}[section]
\newtheorem{lem}[tw]{Lemat}
\newtheorem{wn}[tw]{Wniosek}

\theoremstyle{definition}
\newtheorem{df}[tw]{Definicja}
\newtheorem{uw}[tw]{Uwaga}
\newtheorem{prz}[tw]{Przykład}

% ===== Ustawienia linków =====
\hypersetup{
    colorlinks=true,
    linkcolor=blue,
    citecolor=blue,
    urlcolor=blue
}

\title{Tytuł pracy}
\author{Autor}
\date{\today}

\begin{document}

\maketitle
\tableofcontents

\section{Wstęp}

To jest przykładowy tekst.

\section{Przykładowy wzór}

\[
f(x) = x^2 + 1
\]

\begin{df}
Niech $A \subset \R$. Mówimy, że...
\end{df}

\begin{tw}
Dla każdego $x \in \R$ zachodzi pewna własność.
\end{tw}

\begin{proof}
Dowód przykładowy.
\end{proof}

\section{Przykładowy algorytm}

\begin{algorithm}[H]
\caption{Przykładowy algorytm}
\begin{algorithmic}[1]
\State Ustaw wartość początkową
\For{$i=1,\dots,n$}
    \State Wykonaj krok algorytmu
\EndFor
\end{algorithmic}
\end{algorithm}

\section{Przykładowy kod}

\begin{minted}{python}
def hello():
    print("Hello, LaTeX!")
\end{minted}

Przykładowe cytowanie \cite{lamport1994}.

\printbibliography

\end{document}
EOF

cat > "$TARGET_DIR/bibliography.bib" <<'EOF'
@book{lamport1994,
  author    = {Leslie Lamport},
  title     = {LaTeX: A Document Preparation System},
  year      = {1994},
  publisher = {Addison-Wesley}
}
EOF

cat > "$TARGET_DIR/sections/intro.tex" <<'EOF'
\section{Wstęp}

To jest przykładowa sekcja w osobnym pliku.
EOF

echo "Projekt LaTeX utworzony w: $TARGET_DIR"