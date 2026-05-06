from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np


def tent_map(x: np.ndarray) -> np.ndarray:
    return 1 - 2 * np.abs(x - 0.5)


root = Path(__file__).resolve().parents[1]
out_dir = root / "figures"
out_dir.mkdir(parents=True, exist_ok=True)
out_file = out_dir / "tent_map.pdf"

x = np.linspace(0.0, 1.0, 800)
y = tent_map(x)

fig, ax = plt.subplots(figsize=(6.0, 4.0))
ax.plot(x, y, color="#0b6aa2", linewidth=2.5, label=r"$T(x)=1-2|x-\frac{1}{2}|$")
ax.plot([0.0, 0.5, 1.0], [0.0, 1.0, 0.0], "o", color="#b23a48", markersize=5)
ax.axvline(0.5, color="#666666", linestyle="--", linewidth=1)
ax.axhline(0.0, color="#222222", linewidth=1)
ax.set_xlim(0.0, 1.0)
ax.set_ylim(-0.05, 1.05)
ax.set_xlabel(r"$x$")
ax.set_ylabel(r"$T(x)$")
ax.set_xticks([0.0, 0.25, 0.5, 0.75, 1.0])
ax.set_yticks([0.0, 0.5, 1.0])
ax.grid(True, alpha=0.25)
ax.legend(loc="upper center", frameon=False)
fig.tight_layout()
fig.savefig(out_file)
plt.close(fig)
