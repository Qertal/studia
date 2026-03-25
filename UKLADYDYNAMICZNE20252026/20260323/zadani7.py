import matplotlib.pyplot as plt


def generate_shape(params, x0, y0, k_total, k_burn_in=200):
    """Generuje punkty uogólnionego odwzorowania, odrzucając stany przejściowe."""
    a, b, c, d, e, r, s, t, w, u = params
    x, y = x0, y0

    # Rozgrzewka (pozwalamy układowi 'osiąść' na swoim właściwym kształcie)
    for _ in range(k_burn_in):
        x_new = a * x**2 + b * y**2 + c * x + d * y + e
        y_new = r * x**2 + s * y**2 + t * x + w * y + u
        x, y = x_new, y_new

        # Zabezpieczenie: jeśli parametry "wybuchają" do nieskończoności, przerywamy
        if abs(x) > 1000 or abs(y) > 1000:
            return [], []

    # Zbieranie właściwych punktów
    xs, ys = [], []
    for _ in range(k_total):
        x_new = a * x**2 + b * y**2 + c * x + d * y + e
        y_new = r * x**2 + s * y**2 + t * x + w * y + u
        xs.append(x_new)
        ys.append(y_new)
        x, y = x_new, y_new

    return xs, ys


def plot_shape():
    # --- Słownik magicznych parametrów (a, b, c, d, e, r, s, t, w, u) ---
    zbiory_graniczne = {
        "Punkt": (0, 0, 0.5, 0, 0, 0, 0, 0, 0.5, 0),
        "Linia (Przekątna)": (0, 0, 0.5, 0.5, 0, 0, 0, 0.5, 0.5, 0),
        "Wir (Spirala do środka)": (0, 0, 0.95, -0.1, 0, 0, 0, 0.1, 0.95, 0),
        "Jajo (Elipsa)": (0, 0, 0.99, -0.14, 0, 0, 0, 0.14, 0.99, 0),
        "Klasyczny Hénon": (-1.4, 0, 0, 1, 1, 0, 0, 0.3, 0, 0),
        "Wulkan (pierścień)": (
            -0.35,
            -0.35,
            0.92,
            0.0,
            0.0,
            0.35,
            -0.35,
            0.0,
            0.92,
            0.0,
        ),
        "Gwiazda (rozetka)": (
            -0.22,
            0.22,
            0.96,
            -0.18,
            0.0,
            0.22,
            -0.22,
            0.18,
            0.96,
            0.0,
        ),
    }

    start_points = {
        "Punkt": (1.0, 0.0),
        "Linia (Przekątna)": (1.0, 0.0),
        "Wir (Spirala do środka)": (1.0, 0.0),
        "Jajo (Elipsa)": (1.0, 0.0),
        "Klasyczny Hénon": (0.1, 0.1),
        "Wulkan (pierścień)": (0.8, 0.2),
        "Gwiazda (rozetka)": (1.0, 0.0),
    }

    # --- Rysowanie siatki wykresów ---
    fig, axes = plt.subplots(3, 3, figsize=(14, 14))
    axes = axes.flatten()

    # Iterujemy po słowniku i rysujemy każdy kształt
    for i, (nazwa, parametry) in enumerate(zbiory_graniczne.items()):
        ax = axes[i]
        x0, y0 = start_points[nazwa]

        xs, ys = generate_shape(parametry, x0=x0, y0=y0, k_total=12000, k_burn_in=400)

        if len(xs) == 0:
            ax.text(0.5, 0.5, "Układ rozbieżny", ha="center")
        else:
            ax.scatter(xs, ys, s=0.12, color="red")

        ax.set_title(nazwa)
        ax.grid(True, linestyle=":", alpha=0.5)
        ax.set_aspect("equal", adjustable="datalim")

    # Usuwamy puste wykresy (mamy 7 kształtów, a 9 miejsc)
    for j in range(len(zbiory_graniczne), 9):
        fig.delaxes(axes[j])

    plt.tight_layout()
    plt.show()
