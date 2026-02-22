import numpy as np
import pandas as pd
import argparse
from pathlib import Path


# ============================================================
# Eisenstein Ep dla siatki kwadratowej
# ============================================================

def eisenstein_Ep_square(Z: np.ndarray, p: int, M: int = 12) -> np.ndarray:
    """
    Ep(z) = sum_{(m,n)!=(0,0)} 1 / (z + m + i n)^p
    Siatka: ω1=1, ω2=i (kwadratowa), obcięcie |m|,|n|<=M.
    """
    Z = np.asarray(Z, dtype=np.complex128)
    Ep = np.zeros_like(Z, dtype=np.complex128)

    shifts = range(-M, M + 1)

    with np.errstate(invalid="ignore", divide="ignore", over="ignore"):
        for m in shifts:
            for n in shifts:
                if m == 0 and n == 0:
                    continue
                Ep += 1.0 / (Z + (m + 1j * n))**p

    return Ep


def eisenstein_E2_square(Z: np.ndarray, M: int = 12) -> np.ndarray:
    return eisenstein_Ep_square(Z, p=2, M=M)


# ============================================================
# Diagnostyka odległości
# ============================================================

def distance_diagnostics(a: np.ndarray) -> dict:
    """
    Zwraca minimalną odległość oraz percentyle odległości |a_i-a_j|
    (bez przekątnej).
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
# e2, e2_mod, e_{p,p}^{mod} na danych z RAM (a,r)
# ============================================================

def compute_e2_and_e2mod(a: np.ndarray, r: np.ndarray, M: int = 12) -> dict:
    N = len(a)
    if N < 2:
        raise ValueError("Za mało obiektów (N < 2).")

    Z = a[:, None] - a[None, :]
    np.fill_diagonal(Z, np.nan + 1j*np.nan)

    E2 = eisenstein_E2_square(Z, M=M)
    E2 = np.where(np.isfinite(E2.real), E2, 0.0 + 0.0j)
    np.fill_diagonal(E2, 0.0 + 0.0j)

    e2 = E2.sum() / (N * (N - 1))

    w = np.pi * r**2
    f = w.sum()

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


def compute_epp_mod(a: np.ndarray, r: np.ndarray, p: int, M: int = 12) -> dict:
    """
    Liczy e_{p,p}^{mod} w wersji łańcuchowej:
      e_{p,p}^{mod} = 1/f^{p+1} * sum_j w_j^{p-1} * (sum_i w_i Ep(i,j)) * (sum_k w_k conj(Ep(j,k)))
    """
    N = len(a)
    if N < 2:
        raise ValueError("Za mało obiektów (N < 2).")

    w = np.pi * r**2
    f = w.sum()

    Z = a[:, None] - a[None, :]
    np.fill_diagonal(Z, np.nan + 1j*np.nan)

    Ep = eisenstein_Ep_square(Z, p=p, M=M)
    Ep = np.where(np.isfinite(Ep.real), Ep, 0.0 + 0.0j)
    np.fill_diagonal(Ep, 0.0 + 0.0j)

    u = (w[:, None] * Ep).sum(axis=0)               # (N,)
    v = (w[None, :] * np.conjugate(Ep)).sum(axis=1) # (N,)

    epp_mod = ((w**(p-1)) * u * v).sum() / (f**(p+1))

    return {
        "p": int(p),
        "epp_mod_re": float(epp_mod.real),
        "epp_mod_im": float(epp_mod.imag),
        "epp_mod_abs": float(abs(epp_mod)),
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

    return a, r


# ============================================================
# CLI
# ============================================================

def main():
    parser = argparse.ArgumentParser(description="Sumy strukturalne z CSV")
    parser.add_argument("csv", type=Path, help="CSV z kolumnami: a_re, a_im, r_norm")
    parser.add_argument("--M", type=int, default=12, help="Obcięcie siatki Eisensteina (domyślnie 12)")
    parser.add_argument("--p_list", type=int, nargs="*", default=[3, 8],
                        help="Lista p dla e_{p,p}^{mod} (np. --p_list 3 4 5 6 8)")
    parser.add_argument("--log", action="store_true",
                        help="Dodaj cechy log1p(|cecha|) (zalecane przy dużych p)")
    parser.add_argument("--dist_diag", action="store_true",
                        help="Dodaj diagnostykę minimalnych odległości między punktami")
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

    # --- print ---
    print("\n=== FEATURES ===")
    for k in sorted(features.keys()):
        print(f"{k:18s}: {features[k]}")

    # --- save ---
    if args.out:
        pd.DataFrame([features]).to_csv(args.out, index=False)
        print(f"\nZapisano do: {args.out}")


if __name__ == "__main__":
    main()
