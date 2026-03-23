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
    x_new = a * x**2 + b * y**2 + c * x + d * y + e
    y_new = r * x**2 + s * y**2 + t * x + w * y + u
    return x_new, y_new


def compute_orbit(
    x0: float,
    y0: float,
    n: int,
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
    divergence_limit: float = 1e6
) -> tuple[np.ndarray, np.ndarray, bool]:
    xs = np.empty(n + 1, dtype=float)
    ys = np.empty(n + 1, dtype=float)

    xs[0] = x0
    ys[0] = y0

    x, y = x0, y0
    diverged = False

    for i in range(1, n + 1):
        x, y = generalized_henon_map(x, y, a, b, c, d, e, r, s, t, w, u)
        xs[i] = x
        ys[i] = y

        if not np.isfinite(x) or not np.isfinite(y) or abs(x) > divergence_limit or abs(y) > divergence_limit:
            diverged = True
            return xs[:i+1], ys[:i+1], diverged

    return xs, ys, diverged


def plot_orbit(
    params: dict,
    x0: float = 0.1,
    y0: float = 0.1,
    n: int = 20000,
    burn_in: int | None = None,
    point_size: float = 0.15,
    title: str | None = None,
    divergence_limit: float = 1e6,
    min_points_to_plot: int = 300
) -> None:
    xs, ys, diverged = compute_orbit(
        x0=x0,
        y0=y0,
        n=n,
        a=params["a"],
        b=params["b"],
        c=params["c"],
        d=params["d"],
        e=params["e"],
        r=params["r"],
        s=params["s"],
        t=params["t"],
        w=params["w"],
        u=params["u"],
        divergence_limit=divergence_limit
    )

    total_points = len(xs)

    if burn_in is None:
        burn_in = min(1000, max(50, total_points // 10))
    else:
        burn_in = min(burn_in, max(0, total_points // 5))

    if total_points <= burn_in + 5:
        print(f"{title}: trajektoria zbyt krótka (punktów: {total_points}, burn_in: {burn_in})")
        return

    xs_plot = xs[burn_in:]
    ys_plot = ys[burn_in:]

    # dodatkowe obcięcie skrajnych wartości, żeby pojedyncze odloty nie psuły skali
    finite_mask = np.isfinite(xs_plot) & np.isfinite(ys_plot)
    xs_plot = xs_plot[finite_mask]
    ys_plot = ys_plot[finite_mask]

    if len(xs_plot) < 5:
        print(f"{title}: po filtracji zostało za mało punktów do sensownego rysunku.")
        return

    # sprawdzenie, czy to prawie punkt / prawie cykl o bardzo małym rozrzucie
    spread_x = np.max(xs_plot) - np.min(xs_plot)
    spread_y = np.max(ys_plot) - np.min(ys_plot)

    plt.figure(figsize=(7, 7))
    plt.scatter(xs_plot, ys_plot, s=point_size, alpha=0.7)

    if title is None:
        title = "Uogólniony układ Hénona"

    if diverged:
        title += " [trajektoria przerwana]"

    if spread_x < 1e-4 and spread_y < 1e-4:
        title += " [zbieżność do punktu / krótkiego cyklu]"

    plt.title(title)
    plt.xlabel("x")
    plt.ylabel("y")
    plt.grid(alpha=0.3)

    # sensowne ustawienie osi
    if spread_x > 0 and spread_y > 0:
        margin_x = 0.08 * spread_x
        margin_y = 0.08 * spread_y
        plt.xlim(np.min(xs_plot) - margin_x, np.max(xs_plot) + margin_x)
        plt.ylim(np.min(ys_plot) - margin_y, np.max(ys_plot) + margin_y)

    plt.gca().set_aspect("equal", adjustable="box")
    plt.show()