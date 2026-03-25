# Nadaj mu uprawnienia

W terminalu:

```bash
mkdir -p ~/scripts
chmod +x ~/scripts/new_latex_project.sh
```

---

# Jak go używać

Na przykład:

```bash
~/scripts/new_latex_project.sh Projekt1
```

To utworzy:

```text
~/projekty/Projekt1/
├── bibliography.bib
├── build
├── images
├── main.tex
└── sections
    └── intro.tex
```

---

# Jak zrobić, żeby działało jak normalna komenda

Otwórz:

```bash
nano ~/.bashrc
```

i dodaj na końcu:

```bash
export PATH="$HOME/scripts:$PATH"
```

<!-- /home/qertal/REPO/studia/useful_scripts/nowy_dok_latex/new_latex_project.sh -->

Zapisz, potem:

```bash
source ~/.bashrc
```

Od tej pory wystarczy wpisać:

```bash
new_latex_project.sh Projekt1
```

---

# Jeszcze wygodniej: krótsza komenda

Możesz zmienić nazwę pliku na:

```bash
~/scripts/newtex
```

i wtedy:

```bash
newtex Projekt1
```

To już jest naprawdę wygodne.

---
