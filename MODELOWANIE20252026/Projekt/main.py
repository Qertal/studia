import numpy as np
import imageio.v3 as iio

from skimage import exposure, filters, morphology, measure, segmentation
from scipy import ndimage as ndi

from pathlib import Path
import matplotlib.pyplot as plt
from skimage import measure, segmentation
import imageio.v3 as iio
from io import BytesIO

# ---------- I/O + ROI ----------
def load_and_crop(path, y_max=2060):
    img = iio.imread(path)
    if img.ndim != 2:
        raise ValueError("Oczekuję obrazu 2D (grayscale).")
    return img[:y_max, :]

# ---------- Preprocess ----------
def preprocess(img, use_clahe=False, sigma=1.0):
    x = img.astype(np.float32)
    x = (x - x.min()) / (x.max() - x.min() + 1e-12)  # [0,1]

    # HAADF zwykle ma OK kontrast; CLAHE czasem dodaje artefakty – daję domyślnie False
    if use_clahe:
        x = exposure.equalize_adapthist(x, clip_limit=0.01)

    # lekkie odszumianie
    x = filters.gaussian(x, sigma=sigma, preserve_range=True)
    return x

# ---------- Segmentacja: JASNE OBIEKTY ----------
def segment_bright_objects_steps(x, min_size=80, min_hole=60, use_watershed=True):
    # 1) próg
    t = filters.threshold_otsu(x)
    mask_raw = x > t

    # 2) czyszczenie
    mask_clean = morphology.remove_small_objects(mask_raw, min_size=min_size)
    mask_clean = morphology.remove_small_holes(mask_clean, area_threshold=min_hole)

    labels_ws = None

    # 3) watershed (opcjonalnie)
    if use_watershed:
        dist = ndi.distance_transform_edt(mask_clean)
        peaks = morphology.local_maxima(dist)
        markers = measure.label(peaks)
        labels_ws = segmentation.watershed(-dist, markers, mask=mask_clean)
        mask_after = labels_ws > 0
    else:
        mask_after = mask_clean

    # 4) finalne etykiety
    labels_final = measure.label(mask_after)

    return mask_raw, mask_clean, labels_ws, labels_final


# ---------- Dyski ekwiwalentne ----------
def disks_from_labels(labels):
    props = measure.regionprops(labels)
    H, W = labels.shape

    disks = []
    for p in props:
        cy, cx = p.centroid  # (row, col)
        r_px = np.sqrt(p.area / np.pi)

        # do komórki jednostkowej (-1/2, 1/2)^2
        ax = (cx / W) - 0.5
        ay = (cy / H) - 0.5
        a = ax + 1j * ay

        r_norm = r_px / W
        disks.append((a, r_norm, cx, cy, r_px))

    return disks

# ---------- CSV ----------
def save_disks_csv(disks, out_csv):
    import csv
    with open(out_csv, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["a_re", "a_im", "r_norm", "cx_px", "cy_px", "r_px"])
        for a, r_norm, cx, cy, r_px in disks:
            w.writerow([a.real, a.imag, r_norm, cx, cy, r_px])

# ---------- Overlay ----------
def overlay_preview(img, disks, title="Centroidy"):
    import matplotlib.pyplot as plt
    plt.figure()
    plt.imshow(img, cmap="gray")
    xs = [d[2] for d in disks]
    ys = [d[3] for d in disks]
    plt.scatter(xs, ys, s=10)
    plt.title(f"{title} (N={len(disks)})")
    plt.axis("off")
    plt.show()

from pathlib import Path
import matplotlib.pyplot as plt
from skimage import measure, segmentation
import imageio.v3 as iio
from io import BytesIO

def _save_gray(path, arr, title=None):
    plt.figure()
    plt.imshow(arr, cmap="gray")
    if title:
        plt.title(title)
    plt.axis("off")
    plt.savefig(path, dpi=200, bbox_inches="tight")
    plt.close()

def save_segmentation_steps(out_dir, img, x, mask_raw, mask_clean, labels_final):
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    _save_gray(out_dir / "01_original.png", img, "1) Oryginał")
    _save_gray(out_dir / "02_preprocess.png", x, "2) Po preprocess")
    _save_gray(out_dir / "03_mask_raw.png", mask_raw.astype(np.uint8)*255, "3) Maska po progu (raw)")
    _save_gray(out_dir / "04_mask_clean.png", mask_clean.astype(np.uint8)*255, "4) Maska po czyszczeniu")

    # kontury maski na oryginale
    contours = measure.find_contours(mask_clean.astype(float), 0.5)
    plt.figure()
    plt.imshow(img, cmap="gray")
    for c in contours:
        plt.plot(c[:, 1], c[:, 0], linewidth=1)
    plt.title("5) Kontury maski na oryginale")
    plt.axis("off")
    plt.savefig(out_dir / "05_contours_overlay.png", dpi=200, bbox_inches="tight")
    plt.close()

    # granice etykiet na oryginale
    boundaries = segmentation.find_boundaries(labels_final, mode="outer")
    plt.figure()
    plt.imshow(img, cmap="gray")
    plt.imshow(boundaries.astype(float), alpha=0.7)
    plt.title("6) Granice etykiet na oryginale")
    plt.axis("off")
    plt.savefig(out_dir / "06_label_boundaries.png", dpi=200, bbox_inches="tight")
    plt.close()

def _fig_to_rgb_array(fig):
    buf = BytesIO()
    fig.savefig(buf, format="png", dpi=150, bbox_inches="tight")
    plt.close(fig)
    buf.seek(0)
    return iio.imread(buf.getvalue())

def make_segmentation_gif(out_gif, img, x, mask_raw, mask_clean, labels_final, duration=0.9):
    frames = []

    def add_frame(title, base, overlay=None, overlay_alpha=0.35, contours=None):
        fig = plt.figure()
        plt.imshow(base, cmap="gray")
        if overlay is not None:
            plt.imshow(overlay, alpha=overlay_alpha)
        if contours is not None:
            for c in contours:
                plt.plot(c[:, 1], c[:, 0], linewidth=1)
        plt.title(title)
        plt.axis("off")
        frames.append(_fig_to_rgb_array(fig))

    add_frame("1) Oryginał", img)
    add_frame("2) Po preprocess", x)
    add_frame("3) Maska po progu", img, overlay=mask_raw.astype(float))
    add_frame("4) Maska po czyszczeniu", img, overlay=mask_clean.astype(float))

    contours = measure.find_contours(mask_clean.astype(float), 0.5)
    add_frame("5) Kontury maski", img, contours=contours)

    boundaries = segmentation.find_boundaries(labels_final, mode="outer").astype(float)
    add_frame("6) Granice etykiet", img, overlay=boundaries, overlay_alpha=0.7)

    out_gif = Path(out_gif)
    out_gif.parent.mkdir(parents=True, exist_ok=True)
    iio.imwrite(out_gif, frames, duration=duration, loop=0)

def make_growing_labels_gif(out_gif, img, labels_final, step=3, duration=0.12):
    frames = []
    max_lab = int(labels_final.max())
    current = np.zeros_like(labels_final, dtype=bool)

    for k in range(1, max_lab + 1, step):
        for j in range(k, min(k + step, max_lab + 1)):
            current |= (labels_final == j)

        fig = plt.figure()
        plt.imshow(img, cmap="gray")
        plt.imshow(current.astype(float), alpha=0.35)
        plt.title(f"Narastająco: etykiety ≤ {min(k+step-1, max_lab)} / {max_lab}")
        plt.axis("off")
        frames.append(_fig_to_rgb_array(fig))

    out_gif = Path(out_gif)
    out_gif.parent.mkdir(parents=True, exist_ok=True)
    iio.imwrite(out_gif, frames, duration=duration, loop=0)



if __name__ == "__main__":
    path = r"15.47.34 Scanning Acquire-2-Acquire HAADF.tif"

    img = load_and_crop(path, y_max=2060)
    x = preprocess(img, use_clahe=True, sigma=1.0)

    mask_raw, mask_clean, labels_ws, labels_final = segment_bright_objects_steps(
        x,
        min_size=20,
        min_hole=30,
        use_watershed=True
    )

    disks = disks_from_labels(labels_final)

    print("N_disks:", len(disks))
    save_disks_csv(disks, "haadf_disks_bright.csv")
    overlay_preview(img, disks, title="HAADF – jasne obiekty")

    # --- DEBUG / wizualizacje kroków ---
    out_dir = "debug_steps_haadf"
    save_segmentation_steps(out_dir, img, x, mask_raw, mask_clean, labels_final)
    make_segmentation_gif(f"{out_dir}/segmentation_steps.gif", img, x, mask_raw, mask_clean, labels_final, duration=0.9)
    make_growing_labels_gif(f"{out_dir}/growing_labels.gif", img, labels_final, step=3, duration=0.12)

    print(f"Zapisano obrazki i GIF-y do folderu: {out_dir}")
