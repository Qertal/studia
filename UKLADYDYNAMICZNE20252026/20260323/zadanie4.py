import numpy as np
import matplotlib.pyplot as plt


def henon(a, b, x, y):
    return a - b*y - x**2, x


def orbit(a, b, x0, y0, k):
    xs, ys = [], []
    x, y = x0, y0
    for _ in range(k):
        x, y = henon(a, b, x, y)
        xs.append(x)
        ys.append(y)
    return np.array(xs), np.array(ys)

import numpy as np
import matplotlib.pyplot as plt


# =========================
# 1. Odwzorowanie Hénona
# =========================

def henon_map(a: float, b: float, x: float, y: float) -> tuple[float, float]:
    x_new = a - b * y - x**2
    y_new = x
    return x_new, y_new


def henon_orbit(a: float, b: float, x0: float, y0: float, n: int) -> tuple[np.ndarray, np.ndarray]:
    xs = np.empty(n + 1)
    ys = np.empty(n + 1)

    xs[0], ys[0] = x0, y0
    x, y = x0, y0

    for i in range(1, n + 1):
        x, y = henon_map(a, b, x, y)
        xs[i], ys[i] = x, y

    return xs, ys


# =========================
# 2. Punkty stałe
# =========================

def fixed_points(a: float, b: float) -> list[tuple[float, float]]:
    """
    Zwraca listę punktów stałych odwzorowania Hénona.
    Rozwiązujemy:
        x^2 + (1+b)x - a = 0
        y = x
    """
    delta = (1 + b)**2 + 4 * a

    if delta < 0:
        return []
    elif np.isclose(delta, 0):
        x = -(1 + b) / 2
        return [(x, x)]
    else:
        sqrt_delta = np.sqrt(delta)
        x1 = (-(1 + b) + sqrt_delta) / 2
        x2 = (-(1 + b) - sqrt_delta) / 2
        return [(x1, x1), (x2, x2)]


# =========================
# 3. Orbita okresu 2
# =========================

def period_2_orbit(a: float, b: float) -> list[tuple[float, float]]:
    """
    Zwraca dwa punkty ściśle orbity okresu 2, jeśli istnieje.
    Warunek:
        a > 3/4 * (1+b)^2
    """
    threshold = 0.75 * (1 + b)**2

    if a <= threshold:
        return []

    d = np.sqrt(4 * a - 3 * (1 + b)**2)
    p1 = ((1 + b + d) / 2, (1 + b - d) / 2)
    p2 = ((1 + b - d) / 2, (1 + b + d) / 2)
    return [p1, p2]


# =========================
# 4. Rysunek do Zadania 3:
#    parabola + prosta
# =========================

def plot_fixed_points_geometry(a: float, b: float, x_min: float = -3, x_max: float = 3, num: int = 1000) -> None:
    """
    Rysuje:
        y = x^2
        y = a - (1+b)x
    oraz zaznacza punkty przecięcia.
    """
    x = np.linspace(x_min, x_max, num)
    y_parabola = x**2
    y_line = a - (1 + b) * x

    pts = fixed_points(a, b)

    plt.figure(figsize=(8, 6))
    plt.plot(x, y_parabola, label=r"$y=x^2$")
    plt.plot(x, y_line, label=rf"$y={a}-(1+{b})x$")

    for px, py in pts:
        plt.scatter(px, px**2, s=80, label=rf"Punkt stały: $({px:.3f},{py:.3f})$")

    plt.axhline(0, linewidth=0.8)
    plt.axvline(0, linewidth=0.8)
    plt.title("Zadanie 3: interpretacja geometryczna punktów stałych")
    plt.xlabel("x")
    plt.ylabel("y")
    plt.legend()
    plt.grid(alpha=0.3)
    plt.show()


# =========================
# 5. Overlay:
#    trajektoria + punkty stałe + orbita okresu 2
# =========================

def plot_overlay(
    a: float,
    b: float,
    x0: float,
    y0: float,
    n: int = 5000,
    burn_in: int = 100
) -> None:
    xs, ys = henon_orbit(a, b, x0, y0, n)
    fp = fixed_points(a, b)
    p2 = period_2_orbit(a, b)

    plt.figure(figsize=(8, 6))

    # trajektoria
    plt.scatter(xs[burn_in:], ys[burn_in:], s=2, alpha=0.6, label="Trajektoria")

    # punkty stałe
    if fp:
        fx = [p[0] for p in fp]
        fy = [p[1] for p in fp]
        plt.scatter(fx, fy, s=120, marker="x", label="Punkty stałe")

    # orbita okresu 2
    if p2:
        p2x = [p[0] for p in p2]
        p2y = [p[1] for p in p2]
        plt.scatter(p2x, p2y, s=120, marker="o", facecolors="none", label="Orbita okresu 2")

        # strzałki między punktami
        plt.annotate("", xy=p2[1], xytext=p2[0], arrowprops=dict(arrowstyle="->", lw=1.5))
        plt.annotate("", xy=p2[0], xytext=p2[1], arrowprops=dict(arrowstyle="->", lw=1.5))

    plt.title(f"Overlay: trajektoria + punkty szczególne, a={a}, b={b}")
    plt.xlabel("x")
    plt.ylabel("y")
    plt.legend()
    plt.grid(alpha=0.3)
    plt.show()


# =========================
# 6. Bifurkacja względem a
# =========================

def plot_bifurcation(
    b: float,
    a_min: float = -1.5,
    a_max: float = 1.5,
    a_steps: int = 800,
    n_total: int = 1500,
    n_keep: int = 100,
    x0: float = 0.1,
    y0: float = 0.1
) -> None:
    """
    Dla wielu wartości parametru a:
    - iterujemy układ
    - odrzucamy początek
    - rysujemy końcowe wartości x_n
    """
    a_values = np.linspace(a_min, a_max, a_steps)

    A_plot = []
    X_plot = []

    for a in a_values:
        x, y = x0, y0
        orbit_x = []

        diverged = False
        for i in range(n_total):
            x, y = henon_map(a, b, x, y)

            # zabezpieczenie przed rozbieganiem
            if abs(x) > 1e6 or abs(y) > 1e6 or not np.isfinite(x) or not np.isfinite(y):
                diverged = True
                break

            if i >= n_total - n_keep:
                orbit_x.append(x)

        if not diverged:
            A_plot.extend([a] * len(orbit_x))
            X_plot.extend(orbit_x)

    plt.figure(figsize=(10, 6))
    plt.scatter(A_plot, X_plot, s=0.2)
    plt.title(f"Bifurkacja odwzorowania Hénona względem parametru a, b={b}")
    plt.xlabel("a")
    plt.ylabel(r"Długoterminowe wartości $x_n$")
    plt.grid(alpha=0.3)
    plt.show()
