#!/bin/bash

if [ -z "$1" ]; then
    echo "Użycie: new_beamer_project.sh NAZWA_PROJEKTU"
    exit 1
fi

PROJECT_NAME="$1"
TARGET_DIR="./$PROJECT_NAME"

if [ -d "$TARGET_DIR" ]; then
    echo "Folder $TARGET_DIR już istnieje."
    exit 1
fi

mkdir -p "$TARGET_DIR"/{sections,images,build}

cat > "$TARGET_DIR/main.tex" <<'EOF'
\documentclass[9pt]{beamer}

% ===== Motyw =====
\usetheme{Warsaw}
\usecolortheme{lily}

% ===== Kodowanie i język =====
\usepackage[T1]{fontenc}
\usepackage[utf8]{inputenc}
\usepackage[polish]{babel}
\usepackage{lmodern}

% ===== Matematyka =====
\usepackage{amsmath,amssymb,amsfonts,amsthm,mathtools}
\usepackage{bm}

% ===== Tabele i grafika =====
\usepackage{graphicx}
\usepackage{booktabs}
\usepackage{multirow}

% ===== Dodatkowe =====
\usepackage{xcolor}
\usepackage{hyperref}
% \usepackage{enumitem}

% ===== Bibliografia =====
\usepackage{csquotes}
\usepackage[backend=biber,style=numeric]{biblatex}
\addbibresource{bibliography.bib}

% ===== Skróty matematyczne =====
\newcommand{\R}{\mathbb{R}}
\newcommand{\Z}{\mathbb{Z}}
\newcommand{\N}{\mathbb{N}}
\newcommand{\Q}{\mathbb{Q}}
\newcommand{\C}{\mathbb{C}}

% ===== Środowiska =====
\theoremstyle{plain}
\newtheorem{tw}{Twierdzenie}[section]
\newtheorem{lem}{Lemat}[section]

\theoremstyle{definition}
\newtheorem{df}{Definicja}[section]
\newtheorem{uw}{Uwaga}[section]

% ===== Własny headline / footline =====
\makeatletter
\setbeamertemplate{headline}
{
  \leavevmode%
  \hbox{%
  \begin{beamercolorbox}[wd=1\paperwidth,ht=2.25ex,dp=1ex,center]{section in head/foot}%
    \usebeamerfont{section in head/foot}\insertsectionhead
  \end{beamercolorbox}%
  }%
  \vskip0pt%
}

\setbeamertemplate{footline}
{
  \leavevmode%
  \hbox{%
  \begin{beamercolorbox}[wd=.60\paperwidth,ht=2.25ex,dp=1ex,center]{title in head/foot}%
    \usebeamerfont{title in head/foot}\insertshorttitle
  \end{beamercolorbox}%
  \begin{beamercolorbox}[wd=.20\paperwidth,ht=2.25ex,dp=1ex,center]{author in head/foot}%
    \usebeamerfont{author in head/foot}\insertshortauthor
  \end{beamercolorbox}%
  \begin{beamercolorbox}[wd=.20\paperwidth,ht=2.25ex,dp=1ex,center]{date in head/foot}%
    \usebeamerfont{date in head/foot}\insertframenumber{} / \inserttotalframenumber
  \end{beamercolorbox}
  }%
  \vskip0pt%
}
\makeatother

\setbeamertemplate{navigation symbols}{}

% ===== Metadane =====
\author{Autor}
\institute[PK]{Politechnika Krakowska}
\title{Tytuł prezentacji}
\subtitle{Podtytuł}
\date{\today}

\begin{document}

\begin{frame}
    \titlepage
\end{frame}

\begin{frame}{Spis treści}
    \tableofcontents
\end{frame}

\input{sections/intro}
\input{sections/theory}

\begin{frame}[allowframebreaks]{Bibliografia}
    \printbibliography
\end{frame}

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

\begin{frame}{Cel prezentacji}
    \begin{itemize}
        \item Pierwszy punkt
        \item Drugi punkt
        \item Trzeci punkt
    \end{itemize}
\end{frame}

\begin{frame}{Plan}
    \begin{enumerate}
        \item Wprowadzenie
        \item Część teoretyczna
        \item Podsumowanie
    \end{enumerate}
\end{frame}
EOF

cat > "$TARGET_DIR/sections/theory.tex" <<'EOF'
\section{Część teoretyczna}

\begin{frame}{Przykładowa definicja}
    \begin{df}
        Niech $A \subset \R^n$. Mówimy, że...
    \end{df}
\end{frame}

\begin{frame}{Przykładowe twierdzenie}
    \begin{tw}
        Dla każdego $x \in \R$ zachodzi pewna własność.
    \end{tw}
\end{frame}

\begin{frame}{Przykładowy wzór}
    \[
        f(x) = x^2 + 1
    \]
\end{frame}

\begin{frame}{Przykładowa tabela}
    \centering
    \begin{tabular}{lcc}
        \toprule
        Metoda & Wynik 1 & Wynik 2 \\
        \midrule
        A & 0.91 & 0.88 \\
        B & 0.93 & 0.90 \\
        \bottomrule
    \end{tabular}
\end{frame}

\begin{frame}{Cytowanie}
    Przykładowe odwołanie do źródła \cite{lamport1994}.
\end{frame}
EOF

echo "Projekt Beamer utworzony w: $TARGET_DIR"