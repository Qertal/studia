import argparse
from pathlib import Path

import numpy as np
import pandas as pd
import plotly.graph_objects as go


def load_points(csv_path: Path):
    df = pd.read_csv(csv_path)
    for col in ["a_re", "a_im", "r_norm"]:
        if col not in df.columns:
            raise ValueError(f"Brak kolumny '{col}' w CSV.")
    return (
        df["a_re"].to_numpy(float),
        df["a_im"].to_numpy(float),
        df["r_norm"].to_numpy(float),
        df
    )


def pairwise_distances(a_re, a_im):
    dx = a_re[:, None] - a_re[None, :]
    dy = a_im[:, None] - a_im[None, :]
    D = np.sqrt(dx * dx + dy * dy)
    iu = np.triu_indices(D.shape[0], k=1)
    return D[iu]


# ============================================================
# (4) Histogram odległości
# ============================================================

def plot_distance_hist_html(dist, outpath: Path, bins: int = 60):
    p1, p5, p10, p50 = np.percentile(dist, [1, 5, 10, 50])
    dmin = dist.min()

    fig = go.Figure()

    fig.add_trace(
        go.Histogram(
            x=dist,
            nbinsx=bins,
            name="Odległości |a_i-a_j|"
        )
    )

    for val, label in [
        (dmin, "min"),
        (p1, "1%"),
        (p5, "5%"),
        (p10, "10%"),
        (p50, "median"),
    ]:
        fig.add_vline(x=val, line_dash="dash", annotation_text=label)

    fig.update_layout(
        title="Histogram odległości między obiektami (i < j)",
        xaxis_title="Odległość |a_i - a_j|",
        yaxis_title="Liczność",
        bargap=0.05
    )

    fig.write_html(outpath, include_plotlyjs="cdn")
    print(f"[OK] Zapisano HTML: {outpath}")


# ============================================================
# (5) Scatter w płaszczyźnie zespolonej
# ============================================================

def plot_complex_scatter_html(a_re, a_im, r_norm, outpath: Path):
    size = 2000.0 * (r_norm ** 2)
    size = np.clip(size, 6.0, 60.0)

    fig = go.Figure(
        data=go.Scatter(
            x=a_re,
            y=a_im,
            mode="markers",
            marker=dict(
                size=size,
                opacity=0.75,
            ),
            text=[f"r_norm={r:.4f}" for r in r_norm],
            hovertemplate=(
                "Re(a)=%{x:.4f}<br>"
                "Im(a)=%{y:.4f}<br>"
                "%{text}"
            )
        )
    )

    fig.update_layout(
        title="Położenia obiektów w płaszczyźnie zespolonej",
        xaxis_title="Re(a)",
        yaxis_title="Im(a)",
        yaxis_scaleanchor="x",
        yaxis_scaleratio=1
    )

    fig.write_html(outpath, include_plotlyjs="cdn")
    print(f"[OK] Zapisano HTML: {outpath}")


# ============================================================
# CLI
# ============================================================

def main():
    parser = argparse.ArgumentParser(
        description="Plotly: histogram odległości (4) i scatter zespolony (5) → HTML"
    )
    parser.add_argument("csv", type=Path, help="CSV z a_re, a_im, r_norm")
    parser.add_argument("--outdir", type=Path, default=Path("plots_html"))
    parser.add_argument("--bins", type=int, default=60)
    args = parser.parse_args()

    args.outdir.mkdir(parents=True, exist_ok=True)

    a_re, a_im, r_norm, _ = load_points(args.csv)

    dist = pairwise_distances(a_re, a_im)

    plot_distance_hist_html(
        dist,
        args.outdir / "04_distance_hist.html",
        bins=args.bins
    )

    plot_complex_scatter_html(
        a_re,
        a_im,
        r_norm,
        args.outdir / "05_complex_scatter.html"
    )


if __name__ == "__main__":
    main()
