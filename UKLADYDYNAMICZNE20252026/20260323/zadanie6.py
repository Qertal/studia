import numpy as np
import matplotlib.pyplot as plt


def generalized_henon_map(
    x: float,
    y: float,
    a: float,
    b: float,
    c: float,
    d: float,
    e: float,
    r: float,
    s: float,
    t: float,
    w: float,
    u: float
) -> tuple[float, float]:
    """
    Wykonuje jeden krok uogólnionego odwzorowania Hénona:
        h(x,y) = (a*x^2 + b*y^2 + c*x + d*y + e,
                  r*x^2 + s*y^2 + t*x + w*y + u)
    """
    x_new = a * x**2 + b * y**2 + c * x + d * y + e
    y_new = r * x**2 + s * y**2 + t * x + w * y + u
    return x_new, y_new


def generalized_henon_orbit(
    x0: float,
    y0: float,
    k: int,
    a: float,
    b: float,
    c: float,
    d: float,
    e: float,
    r: float,
    s: float,
    t: float,
    w: float,
    u: float
) -> tuple[np.ndarray, np.ndarray]:
    """
    Oblicza orbitę punktu (x0, y0) dla k iteracji.
    Zwraca tablice xs, ys długości k+1.
    """
    if k <= 0:
        raise ValueError("Liczba iteracji k musi być dodatnia.")

    xs = np.empty(k + 1, dtype=float)
    ys = np.empty(k + 1, dtype=float)

    xs[0] = x0
    ys[0] = y0

    x, y = x0, y0

    for i in range(1, k + 1):
        x, y = generalized_henon_map(x, y, a, b, c, d, e, r, s, t, w, u)
        xs[i] = x
        ys[i] = y

        # zabezpieczenie przed rozbieżnością numeryczną
        if not np.isfinite(x) or not np.isfinite(y):
            xs = xs[:i]
            ys = ys[:i]
            print(f"Iteracja przerwana w kroku {i}: wartości przestały być skończone.")
            break

    return xs, ys


def plot_generalized_henon(
    x0: float,
    y0: float,
    k: int,
    a: float,
    b: float,
    c: float,
    d: float,
    e: float,
    r: float,
    s: float,
    t: float,
    w: float,
    u: float,
    burn_in: int = 0,
    point_size: float = 3.0,
    connect_points: bool = False
) -> None:
    """
    Rysuje orbitę uogólnionego odwzorowania Hénona.

    Parametry:
    - burn_in: liczba początkowych iteracji pomijanych na wykresie
    - point_size: rozmiar punktów
    - connect_points: czy łączyć kolejne punkty linią
    """
    xs, ys = generalized_henon_orbit(
        x0=x0, y0=y0, k=k,
        a=a, b=b, c=c, d=d, e=e,
        r=r, s=s, t=t, w=w, u=u
    )

    if burn_in < 0:
        raise ValueError("burn_in nie może być ujemne.")
    if burn_in >= len(xs):
        raise ValueError("burn_in jest zbyt duże względem liczby obliczonych iteracji.")

    xs_plot = xs[burn_in:]
    ys_plot = ys[burn_in:]

    plt.figure(figsize=(8, 6))

    if connect_points:
        plt.plot(xs_plot, ys_plot, linewidth=0.8, alpha=0.8, label="Trajektoria")
        plt.scatter(xs_plot, ys_plot, s=point_size, alpha=0.8)
    else:
        plt.scatter(xs_plot, ys_plot, s=point_size, alpha=0.8, label="Punkty orbity")

    # zaznaczenie punktu startowego
    plt.scatter([xs[0]], [ys[0]], s=60, marker="x", label="Punkt początkowy")

    plt.title("Uogólnione odwzorowanie Hénona — długoterminowe zachowanie")
    plt.xlabel("x")
    plt.ylabel("y")
    plt.grid(alpha=0.3)
    plt.legend()
    plt.show()