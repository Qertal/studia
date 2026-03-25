# 🔁 SyncTeX – szybka nawigacja TEX ↔ PDF (VS Code + LaTeX Workshop)

## 📌 TEX → PDF (forward search)

**Skrót:**

- `Ctrl + Alt + J`

**Co robi:**
- przenosi podgląd PDF do miejsca odpowiadającego aktualnej pozycji kursora w `.tex`

**Kiedy używać:**
- piszesz tekst i chcesz zobaczyć, gdzie to ląduje w PDF
- pracujesz nad wzorem / sekcją i chcesz szybko sprawdzić wynik

---

## 📌 PDF → TEX (inverse search)

**Akcja:**
- `Ctrl + klik` w PDF

**Co robi:**
- przenosi Cię do odpowiadającego fragmentu w pliku `.tex`

**Kiedy używać:**
- widzisz coś w PDF i chcesz znaleźć źródło w kodzie
- debugujesz layout / spacing / błędy

---

## ✨ Formatowanie kodu LaTeX

**Skrót:**

- `Shift + Alt + F`

**Co robi:**
- formatuje plik `.tex` (jeśli masz skonfigurowany formatter, np. `latexindent`)

---

## ⚠️ Warunki działania

Musi być włączone:

```json
"latex-workshop.synctex.afterBuild.enabled": true