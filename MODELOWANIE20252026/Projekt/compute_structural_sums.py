#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
compute_structural_sums.py

Wersja oparta o relacje ze screena:
- E2(z) = ℘(z) + S2
- Em(z) = (-1)^m/(m-1)! * d^{m-2}℘(z)/dz^{m-2},  m>=3

Krata: kwadratowa (ω1=1, ω2=i), czyli ω = m + i n.
Wejście: CSV z kolumnami: a_re, a_im, r_norm (pozostałe kolumny są ignorowane).
Wyjście: cechy e2, e2_mod oraz e_{p,p}^{mod} dla p z --p_list.
"""

import argparse
import math
from pathlib import Path

import numpy as np
import pandas as pd


# ============================================================
# Krata kwadratowa i stała S2
# ============================================================

def _lattice_points_square(M: int) -> np.ndarray:
    """
    Punkty kraty: ω = m + i n dla |m|,|n|<=M.
    Zwraca tablicę complex128, zawiera ω=0.
    """
    shifts = range(-M, M + 1)
    pts = []
    for m in shifts:
        for n in shifts:
            pts.append(1 * m + 1j * n)
    return np.array(pts, dtype=np.complex128)


def lattice_S2_square(M: int) -> complex:
    r"""
    S2 = sum_{ω≠0} 1/ω^2

    UWAGA: to suma warunkowo zbieżna. Tutaj liczona symetrycznie w pudełku
    |m|,|n|<=M, co odpowiada „naturalnemu” obcięciu w Twoim podejściu.
    """
    pts = _lattice_points_square(M)
    pts = pts[pts != 0]
    with np.errstate(divide="ignore", invalid="ignore", over="ignore"):
        s2 = np.sum(1.0 / (pts ** 2))
    return complex(s2)


# ============================================================
# Weierstrass ℘ i jej pochodne (krata kwadratowa)
# ============================================================

def weierstrass_p_square(Z: np.ndarray, M: int = 12) -> np.ndarray:
    r"""
    Weierstrass ℘ dla kraty kwadratowej (ω1=1, ω2=i) w wersji uciętej:

      ℘(z) = 1/z^2 + Σ_{ω≠0} [ 1/(z+ω)^2 - 1/ω^2 ]

    Człon -1/ω^2 regularizuje sumę i daje zbieżność absolutną.
    """
    Z = np.asarray(Z, dtype=np.complex128)
    wp = np.zeros_like(Z, dtype=np.complex128)

    pts = _lattice_points_square(M)
    pts_nz = pts[pts != 0]

    with np.errstate(divide="ignore", invalid="ignore", over="ignore"):
        wp += 1.0 / (Z ** 2)
        for w in pts_nz:
            wp += 1.0 / ((Z + w) ** 2) - 1.0 / (w ** 2)

    return wp


def weierstrass_p_derivative_square(Z: np.ndarray, k: int, M: int = 12) -> np.ndarray:
    r"""
    k-ta pochodna ℘ po z (k>=1) dla kraty kwadratowej.

    Korzystamy z tożsamości (dla k>=1 suma jest absolutnie zbieżna):
      ℘^{(k)}(z) = (-1)^k (k+1)! Σ_{ω∈Λ} 1/(z+ω)^{k+2}

    Dla k=0 (sama ℘) używamy regularizacji w weierstrass_p_square().
    """
    if k < 0:
        raise ValueError("k musi być >= 0")
    if k == 0:
        return weierstrass_p_square(Z, M=M)

    Z = np.asarray(Z, dtype=np.complex128)
    out = np.zeros_like(Z, dtype=np.complex128)

    pts = _lattice_points_square(M)  # zawiera ω=0

    with np.errstate(divide="ignore", invalid="ignore", over="ignore"):
        for w in pts:
            out += 1.0 / ((Z + w) ** (k + 2))

    out *= ((-1) ** k) * math.factorial(k + 1)
    return out


def E_m_square_from_weierstrass(
    Z: np.ndarray,
    m: int,
    M: int = 12,
    S2_cache: complex | None = None
) -> np.ndarray:
    r"""
    Funkcje Eisensteina zgodne ze screenem:

      E2(z) = ℘(z) + S2
      Em(z) = (-1)^m/(m-1)! * d^{m-2}℘(z)/dz^{m-2}    dla m>=3

    E1 pomijamy (warunkowo zbieżna i niepotrzebna do Twoich cech).
    """
    if m == 2:
        wp = weierstrass_p_square(Z, M=M)
        s2 = S2_cache if S2_cache is not None else lattice_S2_square(M)
        return wp + s2

    if m >= 3:
        k = m - 2
        wp_k = weierstrass_p_derivative_square(Z, k=k, M=M)  # ℘^{(m-2)}
        coef = ((-1) ** m) / math.factorial(m - 1)
        return coef * wp_k

    raise ValueError("Obsługiwane są m=2 oraz m>=3 (E1 pominięte).")


# ============================================================
# Diagnostyka odległości
# ============================================================

def distance_diagnostics(a: np.ndarray) -> dict:
    """
    Minimalna odległość oraz percentyle odległości |a_i-a_j| (bez przekątnej).
    """
    Z = a[:, None] - a[None, :]
    np.fill_diagonal(Z, np.nan + 1j * np.nan)
    d = np.abs(Z)
    return {
        "dist_min": float(np.nanmin(d)),
        "dist_p1": float(np.nanpercentile(d, 1)),
        "dist_p5": float(np.nanpercentile(d, 5)),
        "dist_p10": float(np.nanpercentile(d, 10)),
    }


# ============================================================
# CSV -> a,r
# ============================================================

def load_disks_csv(csv_path: Path) -> tuple[np.ndarray, np.ndarray]:
    df = pd.read_csv(csv_path)

    required = {"a_re", "a_im", "r_norm"}
    missing = required - set(df.columns)
    if missing:
        raise ValueError(f"Brak kolumn w CSV: {sorted(missing)}")

    a = df["a_re"].to_numpy(dtype=float) + 1j * df["a_im"].to_numpy(dtype=float)
    r = df["r_norm"].to_numpy(dtype=float)
    return a.astype(np.complex128), r.astype(float)


# ============================================================
# e2, e2_mod
# ============================================================

def compute_e2_and_e2mod(a: np.ndarray, r: np.ndarray, M: int = 12) -> dict:
    N = len(a)
    if N < 2:
        raise ValueError("Za mało obiektów (N < 2).")

    # różnice położeń
    Z = a[:, None] - a[None, :]
    np.fill_diagonal(Z, np.nan + 1j * np.nan)

    # E2(z) = wp(z) + S2
    S2 = lattice_S2_square(M)
    E2 = E_m_square_from_weierstrass(Z, m=2, M=M, S2_cache=S2)

    # porządkowanie: wyzeruj NaN/inf i przekątną
    E2 = np.where(np.isfinite(E2.real), E2, 0.0 + 0.0j)
    np.fill_diagonal(E2, 0.0 + 0.0j)

    # klasyczne e2
    e2 = E2.sum() / (N * N)

    # wagi i f
    w = np.pi * r**2
    f = w.sum()

    # wersja ważona
    W = w[:, None] * w[None, :]
    np.fill_diagonal(W, 0.0)

    e2_mod = (W * E2).sum() / (f**2)

    return {
        "N": int(N),
        "M": int(M),
        "f": float(f),
        "e2_re": float(e2.real),
        "e2_im": float(e2.imag),
        "e2_abs": float(abs(e2)),
        "e2_mod_re": float(e2_mod.real),
        "e2_mod_im": float(e2_mod.imag),
        "e2_mod_abs": float(abs(e2_mod)),
    }


# ============================================================
# e_{p,p}^{mod}
# ============================================================

def compute_epp_mod(a: np.ndarray, r: np.ndarray, p: int, M: int = 12) -> dict:
    """
    e_{p,p}^{mod}:
      1/f^{p+1} * sum_j w_j^{p-1} * (sum_i w_i Ep(i,j)) * (sum_k w_k conj(Ep(j,k)))
    gdzie Ep = E_p(a_i-a_j).
    """
    if p < 2:
        raise ValueError("p musi być >= 2 (E1 pomijamy).")

    N = len(a)
    if N < 2:
        raise ValueError("Za mało obiektów (N < 2).")

    w = np.pi * r**2
    f = w.sum()

    Z = a[:, None] - a[None, :]
    np.fill_diagonal(Z, np.nan + 1j * np.nan)

    # Ep z Weierstrassa (dla p=2 używa wp+S2)
    S2 = lattice_S2_square(M) if p == 2 else None
    Ep = E_m_square_from_weierstrass(Z, m=p, M=M, S2_cache=S2)

    Ep = np.where(np.isfinite(Ep.real), Ep, 0.0 + 0.0j)
    np.fill_diagonal(Ep, 0.0 + 0.0j)

    u = (w[:, None] * Ep).sum(axis=0)               # po kolumnach (N,)
    v = (w[None, :] * np.conjugate(Ep)).sum(axis=1) # po wierszach  (N,)

    epp_mod = ((w**(p-1)) * u * v).sum() / (f**(p+1))

    return {
        "p": int(p),
        "epp_mod_re": float(epp_mod.real),
        "epp_mod_im": float(epp_mod.imag),
        "epp_mod_abs": float(abs(epp_mod)),
    }


# ============================================================
# CLI
# ============================================================

def main():
    parser = argparse.ArgumentParser(description="Sumy strukturalne z CSV (Weierstrass/Eisenstein)")
    parser.add_argument("csv", type=Path, help="CSV z kolumnami: a_re, a_im, r_norm")
    parser.add_argument("--M", type=int, default=12, help="Obcięcie kraty |m|,|n|<=M (domyślnie 12)")
    parser.add_argument(
        "--p_list",
        type=int,
        nargs="*",
        default=[3, 8],
        help="Lista p dla e_{p,p}^{mod} (np. --p_list 3 4 5 6 8). Uwaga: p>=2."
    )
    parser.add_argument("--log", action="store_true", help="Dodaj log1p(|cecha|) dla e_{p,p}^{mod}")
    parser.add_argument("--dist_diag", action="store_true", help="Dodaj diagnostykę odległości między punktami")
    parser.add_argument("--out", type=Path, default=None, help="Zapis wyników do CSV (jeden wiersz)")

    args = parser.parse_args()

    a, r = load_disks_csv(args.csv)

    features = compute_e2_and_e2mod(a, r, M=args.M)

    if args.dist_diag:
        features.update(distance_diagnostics(a))

    # e_{p,p}^{mod} dla listy p
    for p in args.p_list:
        rpp = compute_epp_mod(a, r, p=p, M=args.M)
        features[f"e{p}{p}_mod_re"] = rpp["epp_mod_re"]
        features[f"e{p}{p}_mod_im"] = rpp["epp_mod_im"]
        features[f"e{p}{p}_mod_abs"] = rpp["epp_mod_abs"]
        if args.log:
            features[f"e{p}{p}_mod_logabs"] = float(np.log1p(rpp["epp_mod_abs"]))

    # print
    print("\n=== FEATURES ===")
    for k in sorted(features.keys()):
        print(f"{k:18s}: {features[k]}")

    # save
    if args.out is not None:
        pd.DataFrame([features]).to_csv(args.out, index=False)
        print(f"\nZapisano do: {args.out}")


if __name__ == "__main__":
    main()
