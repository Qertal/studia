from pathlib import Path
import random

import numpy as np
from PIL import Image, ImageDraw


SEED = 20260420
OUTPUT_DIR = Path("figures")
OUTPUT_DIR.mkdir(exist_ok=True)


def baker_map(x, y):
    if x < 0.5:
        return 2 * x, y / 2
    return 2 * x - 1, (y + 1) / 2


def iterate_point(x0, y0, steps):
    points = [(x0, y0)]
    for _ in range(steps):
        points.append(baker_map(*points[-1]))
    return points


def create_morskie_oko(size=320):
    y = np.linspace(0.0, 1.0, size)
    x = np.linspace(0.0, 1.0, size)
    xx, yy = np.meshgrid(x, y)

    image = np.zeros((size, size, 3), dtype=np.uint8)

    sky_mask = yy < 0.48
    sky_t = np.clip(yy / 0.48, 0, 1)
    image[..., 0] = np.where(sky_mask, (120 + 80 * sky_t).astype(np.uint8), image[..., 0])
    image[..., 1] = np.where(sky_mask, (185 + 40 * sky_t).astype(np.uint8), image[..., 1])
    image[..., 2] = np.where(sky_mask, (235 + 15 * sky_t).astype(np.uint8), image[..., 2])

    mountain1 = yy > (-1.7 * xx + 1.1)
    mountain2 = yy > (1.6 * xx - 0.4)
    mountains = (yy < 0.62) & (mountain1 | mountain2) & (yy > 0.2)
    image[mountains] = np.array([88, 102, 92], dtype=np.uint8)

    snow = mountains & (yy < 0.33)
    image[snow] = np.array([242, 244, 247], dtype=np.uint8)

    forest = (yy >= 0.48) & (yy < 0.63)
    image[forest] = np.array([49, 104, 66], dtype=np.uint8)

    lake_band = yy >= 0.63
    depth = np.clip((yy - 0.63) / 0.37, 0, 1)
    image[..., 0] = np.where(lake_band, (24 + 10 * depth).astype(np.uint8), image[..., 0])
    image[..., 1] = np.where(lake_band, (92 + 35 * depth).astype(np.uint8), image[..., 1])
    image[..., 2] = np.where(lake_band, (132 + 60 * depth).astype(np.uint8), image[..., 2])

    reflection = lake_band & (np.abs(xx - 0.5) < 0.18 + 0.12 * (yy - 0.63))
    image[reflection] = np.clip(image[reflection] + np.array([20, 25, 30]), 0, 255)

    shoreline = np.abs(yy - (0.63 + 0.015 * np.sin(8 * np.pi * xx))) < 0.01
    image[shoreline] = np.array([210, 214, 194], dtype=np.uint8)

    return image


def baker_inverse_image(image, iterations):
    transformed = image.copy()
    for _ in range(iterations):
        height, width, _ = transformed.shape
        next_image = np.zeros_like(transformed)
        for row in range(height):
            v = row / (height - 1)
            if v < 0.5:
                source_v = 2 * v
                top_half = False
            else:
                source_v = 2 * v - 1
                top_half = True
            src_row = min(height - 1, int(round(source_v * (height - 1))))
            for col in range(width):
                u = col / (width - 1)
                if top_half:
                    source_u = (u + 1) / 2
                else:
                    source_u = u / 2
                src_col = min(width - 1, int(round(source_u * (width - 1))))
                next_image[row, col] = transformed[src_row, src_col]
        transformed = next_image
    return transformed


def draw_trajectory(points, size=900, margin=70):
    image = Image.new("RGB", (size, size), "white")
    draw = ImageDraw.Draw(image)
    left, top = margin, margin
    right, bottom = size - margin, size - margin

    draw.rectangle([left, top, right, bottom], outline="black", width=3)
    draw.line([left + (right - left) / 2, top, left + (right - left) / 2, bottom], fill=(140, 140, 140), width=2)
    draw.line([left, top + (bottom - top) / 2, right, top + (bottom - top) / 2], fill=(140, 140, 140), width=2)

    def to_pixel(point):
        x, y = point
        px = left + x * (right - left)
        py = bottom - y * (bottom - top)
        return px, py

    pixels = [to_pixel(point) for point in points]
    draw.line(pixels, fill=(11, 94, 129), width=4)

    for index, (px, py) in enumerate(pixels):
        radius = 7
        draw.ellipse([px - radius, py - radius, px + radius, py + radius], fill=(188, 48, 48), outline="black")
        draw.text((px + 8, py - 18), str(index), fill="black")

    draw.text((left - 10, top - 40), "(0,1)", fill="black")
    draw.text((right - 25, bottom + 10), "(1,0)", fill="black")
    draw.text((left - 25, bottom + 10), "(0,0)", fill="black")
    draw.text((right - 25, top - 40), "(1,1)", fill="black")
    draw.text((left + 10, top + 10), "Trajektoria punktu dla przeksztalcenia piekarza", fill="black")
    return image


def make_panel(images, labels, columns=3, tile_size=320, padding=28, title_height=34):
    rows = (len(images) + columns - 1) // columns
    width = columns * tile_size + (columns + 1) * padding
    height = rows * (tile_size + title_height) + (rows + 1) * padding
    panel = Image.new("RGB", (width, height), "white")
    draw = ImageDraw.Draw(panel)

    for index, (array, label) in enumerate(zip(images, labels)):
        row = index // columns
        col = index % columns
        x0 = padding + col * tile_size
        y0 = padding + row * (tile_size + title_height)
        tile = Image.fromarray(array).resize((tile_size, tile_size))
        panel.paste(tile, (x0, y0))
        draw.rectangle([x0, y0, x0 + tile_size, y0 + tile_size], outline="black", width=2)
        draw.text((x0 + 8, y0 + tile_size + 6), label, fill="black")

    return panel


def main():
    rng = random.Random(SEED)
    x0 = rng.random()
    y0 = rng.random()
    points = iterate_point(x0, y0, 9)

    base_image = create_morskie_oko()
    iterates = [base_image] + [baker_inverse_image(base_image, n) for n in range(1, 6)]

    trajectory_image = draw_trajectory(points)
    panel_image = make_panel(
        images=iterates,
        labels=["Morskie Oko", "B", "B^2", "B^3", "B^4", "B^5"],
    )

    trajectory_path = OUTPUT_DIR / "zadanie2_trajektoria.png"
    panel_path = OUTPUT_DIR / "zadanie2_iteracje.png"
    trajectory_image.save(trajectory_path)
    panel_image.save(panel_path)

    print(f"Punkt poczatkowy: ({x0:.6f}, {y0:.6f})")
    for n, (x, y) in enumerate(points):
        print(f"n={n}: ({x:.6f}, {y:.6f})")
    print(f"Zapisano: {trajectory_path}")
    print(f"Zapisano: {panel_path}")


if __name__ == "__main__":
    main()
