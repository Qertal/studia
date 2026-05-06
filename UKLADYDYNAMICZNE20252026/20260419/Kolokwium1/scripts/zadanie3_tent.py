from pathlib import Path

from PIL import Image, ImageDraw


OUTPUT_DIR = Path("figures")
OUTPUT_DIR.mkdir(exist_ok=True)


def tent_map(x):
    if 0 <= x < 0.5:
        return 2 * x
    return 2 - 2 * x


def to_pixel(x, y, size=900, margin=90):
    left = margin
    right = size - margin
    top = margin
    bottom = size - margin
    px = left + x * (right - left)
    py = bottom - y * (bottom - top)
    return px, py


def draw_axes(draw, size=900, margin=90):
    left = margin
    right = size - margin
    top = margin
    bottom = size - margin

    draw.rectangle([left, top, right, bottom], outline="black", width=3)
    for tick in range(6):
        value = tick / 5
        px, py = to_pixel(value, 0, size, margin)
        draw.line([px, bottom - 6, px, bottom + 6], fill="black", width=2)
        draw.text((px - 12, bottom + 12), f"{value:.1f}", fill="black")
        px0, py0 = to_pixel(0, value, size, margin)
        draw.line([left - 6, py0, left + 6, py0], fill="black", width=2)
        draw.text((left - 58, py0 - 8), f"{value:.1f}", fill="black")

    draw.text((right - 8, bottom + 42), "x", fill="black")
    draw.text((left - 36, top - 28), "y", fill="black")


def draw_graphs(draw):
    # Diagonal y=x
    draw.line([to_pixel(0, 0), to_pixel(1, 1)], fill=(140, 140, 140), width=3)
    # Tent map
    draw.line([to_pixel(0, 0), to_pixel(0.5, 1), to_pixel(1, 0)], fill=(17, 103, 177), width=4)
    draw.text((140, 110), "y = x", fill=(90, 90, 90))
    draw.text((520, 120), "y = T(x)", fill=(17, 103, 177))


def cobweb_points(start, steps):
    pts = [(start, 0.0), (start, tent_map(start))]
    current = start
    for _ in range(steps - 1):
        image = tent_map(current)
        pts.append((image, image))
        current = image
        pts.append((current, tent_map(current)))
    return pts


def draw_cobweb(draw, start, color, steps=6):
    points = cobweb_points(start, steps)
    pixels = [to_pixel(x, y) for x, y in points]
    draw.line(pixels, fill=color, width=3)
    for x, y in points[1::2]:
        px, py = to_pixel(x, y)
        draw.ellipse([px - 5, py - 5, px + 5, py + 5], fill=color, outline="black")


def main():
    image = Image.new("RGB", (900, 900), "white")
    draw = ImageDraw.Draw(image)

    draw_axes(draw)
    draw_graphs(draw)
    draw_cobweb(draw, start=2 / 5, color=(188, 48, 48))
    draw_cobweb(draw, start=4 / 5, color=(46, 125, 50))

    draw.text((480, 720), "orbita startujaca z 2/5", fill=(188, 48, 48))
    draw.text((480, 752), "orbita startujaca z 4/5", fill=(46, 125, 50))

    output_path = OUTPUT_DIR / "zadanie3_pajeczyna.png"
    image.save(output_path)
    print(f"Zapisano: {output_path}")
    print("Punkty orbity dlugosci 2: 2/5 oraz 4/5")


if __name__ == "__main__":
    main()
