#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
lab4_timeseries.py

Tłumaczenie ćwiczenia z Analizy Szeregów Czasowych (R) na Pythona.
Funkcje:
 - wczytanie CSV z kolumną dat i wartości
 - konwersja do pandas.Series z indeksem DatetimeIndex lub PeriodIndex
 - dekompozycja addytywna i multiplikatywna (statsmodels)
 - wizualizacje: wykres szeregu, komponenty dekompozycji
 - analiza reszt: ACF, QQ-plot, test Ljung-Box, test Shapiro
 - zapis wykresów do plików

Wymagane pakiety:
 pip install pandas matplotlib statsmodels scipy seaborn

Autor: wygenerowane automatycznie
"""

import argparse
import sys
from typing import Optional

import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from statsmodels.tsa.seasonal import seasonal_decompose
from statsmodels.graphics.tsaplots import plot_acf
from statsmodels.stats.diagnostic import acorr_ljungbox
from scipy import stats

sns.set(style="whitegrid")


def read_series(csv_path: str, value_col: str, date_col: Optional[str], freq: Optional[str], date_format: Optional[str]) -> pd.Series:
    df = pd.read_csv(csv_path, header=0, decimal='.')
    if date_col:
        if date_format:
            df[date_col] = pd.to_datetime(df[date_col], format=date_format)
        else:
            df[date_col] = pd.to_datetime(df[date_col], infer_datetime_format=True)
        df = df.set_index(date_col)
        # enforce frequency if provided
        if freq:
            try:
                df = df.asfreq(freq)
            except Exception:
                pass
    s = df[value_col].astype(float)
    s.index = df.index
    return s


def save_plot(fig, path: str):
    fig.tight_layout()
    fig.savefig(path, dpi=150)
    plt.close(fig)


def main():
    parser = argparse.ArgumentParser(description="Analiza szeregow czasowych (lab4) - Python")
    parser.add_argument("csv", help="Ścieżka do pliku CSV z danymi")
    parser.add_argument("--value-col", default="kolumna", help="Nazwa kolumny z wartościami")
    parser.add_argument("--date-col", default=None, help="Nazwa kolumny z datami (opcjonalnie)")
    parser.add_argument("--date-format", default=None, help="Format daty, np. %%Y-%%m-%%d (opcjonalnie)")
    parser.add_argument("--freq", default=None, help="Częstotliwość, np. 'M' (mieś.), 'Q', 'D' - opcjonalne")
    parser.add_argument("--start-year", type=int, default=None, help="Rok początkowy (jeśli brak daty w pliku)")
    parser.add_argument("--start-period", type=int, default=None, help="Okres początkowy/miesiąc (jeśli brak daty)")
    parser.add_argument("--output-prefix", default="lab4_output", help="Prefix dla wygenerowanych plików")
    parser.add_argument("--model", choices=["additive","multiplicative"], default="additive", help="Typ dekompozycji")
    parser.add_argument("--skip-decompose", action="store_true", help="Pominięcie dekompozycji (szybsze)")
    args = parser.parse_args()

    # Wczytaj serię
    if args.date_col is None and args.start_year is None:
        print("Musisz podać albo kolumnę dat (--date-col), albo rok początkowy (--start-year)")
        sys.exit(1)

    s = read_series(args.csv, args.value_col, args.date_col, args.freq, args.date_format)

    if args.date_col is None:
        # konstrukcja PeriodIndex z roku i okresu
        if args.start_period is None:
            args.start_period = 1
        idx = pd.period_range(start=pd.Period(year=args.start_year, month=args.start_period, freq=args.freq or 'M'), periods=len(s), freq=args.freq or 'M')
        s.index = idx.to_timestamp()

    # Rysunek szeregu
    fig, ax = plt.subplots(figsize=(10,4))
    s.plot(ax=ax, color='blue', lw=2)
    ax.set_title('Dane empiryczne: szereg czasowy X(t)')
    ax.set_xlabel('Time')
    ax.set_ylabel('X(t)')
    save_plot(fig, f"{args.output_prefix}_series.png")
    print(f"Zapisano wykres szeregu: {args.output_prefix}_series.png")

    if not args.skip_decompose:
        # dekompozycja
        try:
            decomp = seasonal_decompose(s, model=args.model, period=None)
        except Exception as e:
            # jeśli period nieznany, spróbuj wywnioskować
            inferred = pd.infer_freq(s.index)
            if inferred is None:
                print("Nie udało się wywnioskować częstotliwości - podaj --freq lub popraw indeks dat")
                sys.exit(1)
            print(f"Wykryto częstotliwość: {inferred}")
            decomp = seasonal_decompose(s, model=args.model, period=pd.tseries.frequencies.to_offset(inferred).n)

        # wykres komponentów
        fig = decomp.plot()
        fig.set_size_inches(10,8)
        save_plot(fig, f"{args.output_prefix}_decompose_{args.model}.png")
        print(f"Zapisano dekompozycję: {args.output_prefix}_decompose_{args.model}.png")

        # reszty
        resid = decomp.resid.dropna()
    else:
        resid = s - s.rolling(window=12, min_periods=1).mean()

    # ACF reszt
    fig, ax = plt.subplots(figsize=(8,4))
    plot_acf(resid, ax=ax, lags=20)
    ax.set_title('ACF dla reszt')
    save_plot(fig, f"{args.output_prefix}_resid_acf.png")
    print(f"Zapisano ACF reszt: {args.output_prefix}_resid_acf.png")

    # QQ-plot
    fig = plt.figure(figsize=(6,6))
    stats.probplot(resid, dist="norm", plot=plt)
    plt.title('QQ-plot reszt')
    save_plot(fig, f"{args.output_prefix}_resid_qq.png")
    print(f"Zapisano QQ-plot: {args.output_prefix}_resid_qq.png")

    # Test Ljung-Box
    try:
        lb = acorr_ljungbox(resid, lags=[5], return_df=True)
        print('Ljung-Box test (lag=5):')
        print(lb)
    except Exception as e:
        print('Błąd w Ljung-Box:', e)

    # Test Shapiro
    try:
        sh = stats.shapiro(resid)
        print('Shapiro-Wilk test:')
        print(sh)
    except Exception as e:
        print('Błąd w Shapiro test:', e)

if __name__ == '__main__':
    main()
