import numpy as np
import matplotlib.pyplot as plt


def preimage_intervals(m, a, b):
    """
    Zwraca listę przedziałów tworzących E_m^{-1}([a,b]).
    Każdy element listy ma postać (left, right).
    """
    return [((a + j) / m, (b + j) / m) for j in range(m)]


def plot_preimage_on_interval(m, a, b, ax=None, color="tab:blue"):
    """
    Rysuje przeciwobraz E_m^{-1}([a,b]) na odcinku [0,1].
    """
    intervals = preimage_intervals(m, a, b)

    if ax is None:
        fig, ax = plt.subplots(figsize=(12, 2.5))

    # oś bazowa
    ax.hlines(0, 0, 1, color="black", linewidth=1)

    # oryginalny przedział [a,b] nad osią
    ax.plot(
        [a, b],
        [0.15, 0.15],
        color="red",
        linewidth=6,
        solid_capstyle="butt",
        label="[$a,b$]",
    )

    # przeciwobrazy na osi głównej
    for left, right in intervals:
        ax.plot([left, right], [0, 0], color=color, linewidth=6, solid_capstyle="butt")

    ax.set_xlim(-0.02, 1.02)
    ax.set_ylim(-0.3, 0.3)
    ax.set_yticks([])
    ax.set_xticks(np.linspace(0, 1, 11))
    ax.set_title(f"Przeciwobraz $E_{{{m}}}^{{-1}}([a,b])$ na odcinku [0,1]")
    ax.grid(axis="x", linestyle="--", alpha=0.3)

    return ax


def plot_preimage_on_circle(m, a, b, ax=None, color="tab:blue", base_color="red"):
    """
    Rysuje [a,b] oraz E_m^{-1}([a,b]) na okręgu S^1.
    Parametr x z [0,1) utożsamiamy z kątem 2*pi*x.
    """
    intervals = preimage_intervals(m, a, b)

    if ax is None:
        fig, ax = plt.subplots(figsize=(7, 7))

    # okrąg bazowy
    t = np.linspace(0, 2 * np.pi, 600)
    ax.plot(np.cos(t), np.sin(t), color="black", linewidth=1)

    # pomocnicza funkcja rysująca łuk odpowiadający [u,v]
    def draw_arc(u, v, r=1.0, lw=5, color="tab:blue"):
        theta = np.linspace(2 * np.pi * u, 2 * np.pi * v, 200)
        ax.plot(r * np.cos(theta), r * np.sin(theta), color=color, linewidth=lw)

    # rysujemy oryginalny przedział [a,b]
    draw_arc(a, b, r=1.05, lw=6, color=base_color)

    # rysujemy przeciwobrazy
    for left, right in intervals:
        draw_arc(left, right, r=1.0, lw=5, color=color)

    # zaznaczenie kilku punktów referencyjnych
    marks = [0, 0.25, 0.5, 0.75]
    labels = ["0", "1/4", "1/2", "3/4"]
    for x, label in zip(marks, labels):
        theta = 2 * np.pi * x
        ax.plot(
            [0.95 * np.cos(theta), 1.08 * np.cos(theta)],
            [0.95 * np.sin(theta), 1.08 * np.sin(theta)],
            color="gray",
            linewidth=1,
        )
        ax.text(
            1.16 * np.cos(theta),
            1.16 * np.sin(theta),
            label,
            ha="center",
            va="center",
            fontsize=11,
        )

    ax.set_aspect("equal")
    ax.axis("off")
    ax.set_title(f"Przeciwobraz $E_{{{m}}}^{{-1}}([a,b])$ na okręgu $S^1$")

    return ax


def plot_both(m, a, b):
    """
    Rysuje jednocześnie:
    - przeciwobraz na odcinku [0,1],
    - przeciwobraz na okręgu S^1.
    """
    fig = plt.figure(figsize=(12, 8))

    ax1 = plt.subplot(2, 1, 1)
    plot_preimage_on_interval(m, a, b, ax=ax1)

    ax2 = plt.subplot(2, 1, 2)
    plot_preimage_on_circle(m, a, b, ax=ax2)

    plt.tight_layout()
    plt.show()


def trajectory_E2(x0, steps=100):
    xs = [x0]
    x = x0
    for _ in range(steps):
        x = (2 * x) % 1
        xs.append(x)
    return np.array(xs)
