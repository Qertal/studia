import argparse
from pathlib import Path

import numpy as np
import plotly.graph_objects as go


def lattice_points_square(M: int) -> np.ndarray:
    shifts = range(-M, M + 1)
    pts = [m + 1j * n for m in shifts for n in shifts]
    return np.array(pts, dtype=np.complex128)


def weierstrass_p_square(Z: np.ndarray, M: int = 12) -> np.ndarray:
    """
    ℘(z) = 1/z^2 + Σ_{ω≠0} [ 1/(z+ω)^2 - 1/ω^2 ]
    (krata kwadratowa: ω = m + i n)
    """
    Z = np.asarray(Z, dtype=np.complex128)
    wp = np.zeros_like(Z, dtype=np.complex128)

    pts = lattice_points_square(M)
    pts_nz = pts[pts != 0]

    with np.errstate(divide="ignore", invalid="ignore", over="ignore"):
        wp += 1.0 / (Z ** 2)
        for w in pts_nz:
            wp += 1.0 / ((Z + w) ** 2) - 1.0 / (w ** 2)

    return wp


def mask_near_poles(X, Y, eps: float = 0.03):
    """
    Maskuje punkty blisko biegunów (krata Z+iZ), żeby heatmapa nie była zdominowana infinities.
    eps w jednostkach "komórki" (tu omega1=1).
    """
    # odległość do najbliższej liczby całkowitej w osi x i y
    dx = np.abs(X - np.round(X))
    dy = np.abs(Y - np.round(Y))
    return (dx < eps) & (dy < eps)


def heatmap_html(Zvals, title, outpath: Path):
    fig = go.Figure(
        data=go.Heatmap(
            z=Zvals,
            colorbar=dict(title=title),
        )
    )
    fig.update_layout(
        title=title,
        xaxis_title="x (Re z)",
        yaxis_title="y (Im z)",
    )
    fig.write_html(outpath, include_plotlyjs="cdn")
    print(f"[OK] Zapisano: {outpath}")


def main():
    parser = argparse.ArgumentParser(description="Wizualizacja Weierstrass ℘(z) dla kraty kwadratowej → HTML")
    parser.add_argument("--M", type=int, default=12, help="obcięcie kraty w sumie")
    parser.add_argument("--xmin", type=float, default=-0.5)
    parser.add_argument("--xmax", type=float, default=0.5)
    parser.add_argument("--ymin", type=float, default=-0.5)
    parser.add_argument("--ymax", type=float, default=0.5)
    parser.add_argument("--n", type=int, default=300, help="liczba punktów na oś (siatka n×n)")
    parser.add_argument("--eps_pole", type=float, default=0.02, help="maskowanie okolic biegunów")
    parser.add_argument("--outdir", type=Path, default=Path("plots_html"))
    args = parser.parse_args()

    args.outdir.mkdir(parents=True, exist_ok=True)

    x = np.linspace(args.xmin, args.xmax, args.n)
    y = np.linspace(args.ymin, args.ymax, args.n)
    X, Y = np.meshgrid(x, y)
    Z = X + 1j * Y

    wp = weierstrass_p_square(Z, M=args.M)

    # maskuj okolice biegunów (w kracie Z+iZ bieguny są w punktach całkowitych)
    pole_mask = mask_near_poles(X, Y, eps=args.eps_pole)
    wp = wp.copy()
    wp[pole_mask] = np.nan + 1j * np.nan

    # Najczęściej sensowne: log|wp|
    wp_abs = np.abs(wp)
    wp_logabs = np.log1p(wp_abs)

    heatmap_html(
        wp_logabs,
        title=f"log(1+|wp(z)|), M={args.M}",
        outpath=args.outdir / "wp_logabs.html",
    )

    heatmap_html(
        np.real(wp),
        title=f"Re(wp(z)), M={args.M}",
        outpath=args.outdir / "wp_re.html",
    )

    heatmap_html(
        np.imag(wp),
        title=f"Im(wp(z)), M={args.M}",
        outpath=args.outdir / "wp_im.html",
    )


if __name__ == "__main__":
    main()
