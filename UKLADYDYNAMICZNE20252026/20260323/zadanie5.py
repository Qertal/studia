import numpy as np
import matplotlib.pyplot as plt


def henon_map(a: float, b: float, x: float, y: float) -> tuple[float, float]:
    """
    Jedno przejście odwzorowania Hénona:
        h_{a,b}(x,y) = (a - b*y - x^2, x)
    """
    return a - b * y - x**2, x


def henon_orbit(a: float, b: float, x0: float, y0: float, n: int) -> tuple[np.ndarray, np.ndarray]:
    """
    Oblicza orbitę długości n startując z punktu (x0, y0).
    """
    xs = np.empty(n + 1, dtype=float)
    ys = np.empty(n + 1, dtype=float)

    xs[0], ys[0] = x0, y0
    x, y = x0, y0

    for i in range(1, n + 1):
        x, y = henon_map(a, b, x, y)
        xs[i], ys[i] = x, y

    return xs, ys


def period_2_points(a: float, b: float) -> list[tuple[float, float]]:
    """
    Zwraca punkty ściśle orbity okresu 2, jeśli istnieją.
    Warunek istnienia:
        a > 3/4 * (1+b)^2
    """
    threshold = 0.75 * (1 + b) ** 2
    if a <= threshold:
        return []

    d = np.sqrt(4 * a - 3 * (1 + b) ** 2)

    p1 = ((1 + b + d) / 2, (1 + b - d) / 2)
    p2 = ((1 + b - d) / 2, (1 + b + d) / 2)

    return [p1, p2]


def plot_task5(a: float, b: float, x0: float, y0: float, n: int = 200, burn_in: int = 0) -> None:
    """
    Rysunek do Zadania 5:
    - trajektoria odwzorowania Hénona,
    - prosta x+y=1+b,
    - punkty orbity okresu 2 (jeśli istnieją),
    - strzałki między punktami orbity okresu 2.
    """
    xs, ys = henon_orbit(a, b, x0, y0, n)
    p2 = period_2_points(a, b)

    plt.figure(figsize=(8, 8))

    # trajektoria
    plt.plot(xs[burn_in:], ys[burn_in:], marker="o", markersize=3, linewidth=1, alpha=0.7, label="Trajektoria")
    plt.scatter(xs[0], ys[0], s=80, marker="s", label="Punkt startowy")

    # zakres osi dopasowany do danych i punktów okresu 2
    all_x = list(xs[burn_in:])
    all_y = list(ys[burn_in:])

    if p2:
        all_x.extend([p[0] for p in p2])
        all_y.extend([p[1] for p in p2])

    x_min, x_max = min(all_x) - 0.5, max(all_x) + 0.5
    y_min, y_max = min(all_y) - 0.5, max(all_y) + 0.5

    # prosta x + y = 1 + b, czyli y = 1+b-x
    x_line = np.linspace(x_min, x_max, 400)
    y_line = 1 + b - x_line
    plt.plot(x_line, y_line, linestyle="--", linewidth=2, label=rf"$x+y=1+{b}$")

    # punkty orbity okresu 2
    if p2:
        p1, p2b = p2
        plt.scatter([p1[0], p2b[0]], [p1[1], p2b[1]], s=120, marker="x", label="Orbita okresu 2")

        # podpisy punktów
        plt.text(p1[0] + 0.03, p1[1] + 0.03, rf"$P_1=({p1[0]:.3f},{p1[1]:.3f})$")
        plt.text(p2b[0] + 0.03, p2b[1] + 0.03, rf"$P_2=({p2b[0]:.3f},{p2b[1]:.3f})$")

        # strzałki pokazujące przejście P1 -> P2 -> P1
        plt.annotate("", xy=p2b, xytext=p1, arrowprops=dict(arrowstyle="->", lw=1.5))
        plt.annotate("", xy=p1, xytext=p2b, arrowprops=dict(arrowstyle="->", lw=1.5))

    plt.title(f"Zadanie 5: prosta x+y=1+b oraz orbita okresu 2\n a={a}, b={b}")
    plt.xlabel("x")
    plt.ylabel("y")
    plt.xlim(x_min, x_max)
    plt.ylim(y_min, y_max)
    plt.grid(alpha=0.3)
    plt.legend()
    plt.gca().set_aspect("equal", adjustable="box")
    plt.show()