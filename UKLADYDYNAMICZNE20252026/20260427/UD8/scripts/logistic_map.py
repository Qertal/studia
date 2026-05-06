from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np


def logistic_map(x: np.ndarray) -> np.ndarray:
    return 4 * x * (1 - x)


root = Path(__file__).resolve().parents[1]
out_dir = root / "figures"
out_dir.mkdir(parents=True, exist_ok=True)
out_file = out_dir / "logistic_map.pdf"

x = np.linspace(0.0, 1.0, 800)
y = logistic_map(x)

fig, ax = plt.subplots(figsize=(6.0, 4.0))
ax.plot(x, y, color="#8a4f2b", linewidth=2.5, label=r"$f(x)=4x(1-x)$")
ax.plot(
    [0.0, 0.25, 0.5, 0.75, 1.0],
    [0.0, 0.75, 1.0, 0.75, 0.0],
    "o",
    color="#1f6f8b",
    markersize=5,
)
ax.axvline(0.5, color="#666666", linestyle="--", linewidth=1)
ax.axhline(0.0, color="#222222", linewidth=1)
ax.set_xlim(0.0, 1.0)
ax.set_ylim(-0.05, 1.05)
ax.set_xlabel(r"$x$")
ax.set_ylabel(r"$f(x)$")
ax.set_xticks([0.0, 0.25, 0.5, 0.75, 1.0])
ax.set_yticks([0.0, 0.5, 1.0])
ax.grid(True, alpha=0.25)
ax.legend(loc="upper center", frameon=False)
fig.tight_layout()
fig.savefig(out_file)
plt.close(fig)
