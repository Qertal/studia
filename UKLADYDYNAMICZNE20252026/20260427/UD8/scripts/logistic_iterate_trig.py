from argparse import ArgumentParser
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np


def logistic_iterate_trig(x: np.ndarray, n: int) -> np.ndarray:
    theta = np.arcsin(np.sqrt(np.clip(x, 0.0, 1.0)))
    return np.sin((2**n) * theta) ** 2


def build_parser() -> ArgumentParser:
    parser = ArgumentParser(
        description=(
            "Rysuje n-krotne zlozenie mapy logistycznej f(x)=4x(1-x) "
            "uzywajac wzoru f_n(sin^2(theta)) = sin^2(2^n*theta)."
        )
    )
    parser.add_argument(
        "-n", "--iterate", type=int, default=3, help="Numer iteracji n."
    )
    parser.add_argument(
        "--points",
        type=int,
        default=2000,
        help="Liczba punktow siatki na [0,1].",
    )
    parser.add_argument(
        "--out",
        type=str,
        default=None,
        help="Sciezka wyjsciowa PDF/PNG (domyslnie: figures/logistic_iterate_n{n}.pdf).",
    )
    return parser


def main() -> None:
    parser = build_parser()
    args = parser.parse_args()

    if args.iterate < 0:
        raise ValueError("Parametr n musi byc nieujemny.")
    if args.points < 2:
        raise ValueError("Liczba punktow musi byc >= 2.")

    root = Path(__file__).resolve().parents[1]
    out_dir = root / "figures"
    out_dir.mkdir(parents=True, exist_ok=True)
    out_file = (
        Path(args.out)
        if args.out
        else out_dir / f"logistic_iterate_n{args.iterate}.pdf"
    )

    x = np.linspace(0.0, 1.0, args.points)
    y = logistic_iterate_trig(x, args.iterate)

    fig, ax = plt.subplots(figsize=(7.2, 4.2))
    ax.plot(
        x,
        y,
        color="#a33f2f",
        linewidth=1.7,
        label=rf"$f^{{({args.iterate})}}(x)$ przez $\,\sin^2(2^n\theta)$",
    )
    ax.set_title(rf"Mapa logistyczna: iteracja $n={args.iterate}$")
    ax.set_xlim(0.0, 1.0)
    ax.set_ylim(-0.02, 1.02)
    ax.set_xlabel(r"$x$")
    ax.set_ylabel(r"$f^{(n)}(x)$")
    ax.grid(True, alpha=0.25)
    ax.legend(loc="upper right", frameon=False)
    fig.tight_layout()
    fig.savefig(out_file)
    plt.close(fig)

    print(f"Zapisano wykres: {out_file}")


if __name__ == "__main__":
    main()
