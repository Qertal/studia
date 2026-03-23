import numpy as np
import matplotlib.pyplot as plt


def henon_map(a: float, b: float, x: float, y: float) -> tuple[float, float]:
    """
    Wykonuje jeden krok odwzorowania Hénona:
        x_{n+1} = a - b*y_n - x_n^2
        y_{n+1} = x_n
    """
    x_new = a - b * y - x**2
    y_new = x
    return x_new, y_new


def henon_orbit(
    a: float,
    b: float,
    x0: float,
    y0: float,
    k: int
) -> tuple[np.ndarray, np.ndarray]:
    """
    Oblicza orbitę punktu początkowego (x0, y0)
    dla k iteracji odwzorowania Hénona.
    """
    xs = np.empty(k + 1, dtype=float)
    ys = np.empty(k + 1, dtype=float)

    xs[0] = x0
    ys[0] = y0

    x, y = x0, y0

    for i in range(1, k + 1):
        x, y = henon_map(a, b, x, y)
        xs[i] = x
        ys[i] = y

    return xs, ys


def plot_henon(
    a: float,
    b: float,
    x0: float,
    y0: float,
    k: int,
    burn_in: int = 0,
    point_size: float = 1.0
) -> None:
    """
    Rysuje orbitę odwzorowania Hénona.
    
    Parametry:
    - a, b: parametry odwzorowania
    - x0, y0: punkt początkowy
    - k: liczba iteracji
    - burn_in: liczba pomijanych początkowych iteracji
    - point_size: rozmiar punktów na wykresie
    """
    if k <= 0:
        raise ValueError("Liczba iteracji k musi być dodatnia.")
    if burn_in < 0:
        raise ValueError("burn_in nie może być ujemne.")
    if burn_in > k:
        raise ValueError("burn_in nie może być większe od k.")

    xs, ys = henon_orbit(a, b, x0, y0, k)

    plt.figure(figsize=(8, 6))
    plt.scatter(xs[burn_in:], ys[burn_in:], s=point_size)
    plt.title(f"Odwzorowanie Hénona: a={a}, b={b}, punkt startowy=({x0}, {y0}), k={k}")
    plt.xlabel("x")
    plt.ylabel("y")
    plt.grid(True, alpha=0.3)
    plt.show()