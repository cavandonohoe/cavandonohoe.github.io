"""Randomized depth-first maze with glowing wall outlines.

The generator mirrors maze-art.html: it uses the same mulberry32 PRNG, the
same 32-bit seed normalization, the same neighbor order (north, east, south,
west) and the same start cell, so a given columns/rows/seed combination
produces the same maze in Python and in the browser.

Usage:
    python maze_art.py --cols 22 --rows 14 --seed 7 --output maze_art.png
"""
import argparse

import numpy as np

DEFAULT_COLS, DEFAULT_ROWS, DEFAULT_SEED = 22, 14, 7
DIRS = [(-1, 0, 0, 2), (0, 1, 1, 3), (1, 0, 2, 0), (0, -1, 3, 1)]
UINT32 = 0xFFFFFFFF


def mulberry32(seed):
    """Return a float generator in [0, 1) matching the browser's random()."""
    a = seed & UINT32

    def next_float():
        nonlocal a
        a = (a + 0x6D2B79F5) & UINT32
        t = ((a ^ (a >> 15)) * (a | 1)) & UINT32
        t ^= (t + (((t ^ (t >> 7)) * (t | 61)) & UINT32)) & UINT32
        return ((t ^ (t >> 14)) & UINT32) / 4294967296

    return next_float


def _check_size(name, value):
    if isinstance(value, bool) or not isinstance(value, (int, np.integer)) or value < 1:
        raise ValueError(f"{name} must be a positive integer, got {value!r}")
    return int(value)


def normalize_seed(seed):
    """Map any integer seed to the unsigned 32-bit value the browser uses."""
    if isinstance(seed, bool) or not isinstance(seed, (int, np.integer)):
        raise ValueError(f"seed must be an integer, got {seed!r}")
    return int(seed) & UINT32


def maze_walls(cols=DEFAULT_COLS, rows=DEFAULT_ROWS, seed=DEFAULT_SEED):
    """Return a (rows, cols, 4) boolean wall array (north, east, south, west)."""
    cols, rows = _check_size("cols", cols), _check_size("rows", rows)
    rng = mulberry32(normalize_seed(seed))
    walls = np.ones((rows, cols, 4), dtype=bool)
    seen = np.zeros((rows, cols), dtype=bool)
    seen[0, 0] = True
    stack = [(0, 0)]
    while stack:
        r, c = stack[-1]
        options = [(r + dr, c + dc, a, b) for dr, dc, a, b in DIRS
                   if 0 <= r + dr < rows and 0 <= c + dc < cols and not seen[r + dr, c + dc]]
        if not options:
            stack.pop()
            continue
        nr, nc, a, b = options[int(rng() * len(options))]
        walls[r, c, a] = False
        walls[nr, nc, b] = False
        seen[nr, nc] = True
        stack.append((nr, nc))
    return walls


def maze_segments(cols=DEFAULT_COLS, rows=DEFAULT_ROWS, seed=DEFAULT_SEED):
    walls = maze_walls(cols, rows, seed)
    rows, cols = walls.shape[:2]
    segments = []
    for r in range(rows):
        for c in range(cols):
            x, y = c, rows - r - 1
            if walls[r, c, 0]:
                segments.append([(x, y + 1), (x + 1, y + 1)])
            if walls[r, c, 3]:
                segments.append([(x, y), (x, y + 1)])
            if r == rows - 1 and walls[r, c, 2]:
                segments.append([(x, y), (x + 1, y)])
            if c == cols - 1 and walls[r, c, 1]:
                segments.append([(x + 1, y), (x + 1, y + 1)])
    return segments


def render(cols=DEFAULT_COLS, rows=DEFAULT_ROWS, seed=DEFAULT_SEED, output="maze_art.png"):
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib.collections import LineCollection

    segments = maze_segments(cols, rows, seed)
    fig, ax = plt.subplots(figsize=(14, 9), facecolor="#060b11")
    ax.set_facecolor("#060b11")
    layers = [(10, .025, "#4cc9ff"), (5, .075, "#65d8ff"), (2, .32, "#a7ecff"), (.75, .96, "#e5faff")]
    for width, alpha, color in layers:
        ax.add_collection(LineCollection(segments, colors=color, linewidths=width, alpha=alpha))
    ax.set_xlim(-.8, cols + .8)
    ax.set_ylim(-.8, rows + .8)
    ax.set_aspect("equal")
    ax.axis("off")
    fig.savefig(output, dpi=180, facecolor=fig.get_facecolor(), bbox_inches="tight", pad_inches=.1)
    plt.close(fig)
    return output


def _positive_int(value):
    try:
        return _check_size("value", int(value))
    except ValueError as err:
        raise argparse.ArgumentTypeError(f"expected a positive integer, got {value!r}") from err


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--cols", type=_positive_int, default=DEFAULT_COLS,
                        help=f"maze columns (default {DEFAULT_COLS}; the browser allows 4-65)")
    parser.add_argument("--rows", type=_positive_int, default=DEFAULT_ROWS,
                        help=f"maze rows (default {DEFAULT_ROWS}; the browser allows 4-45)")
    parser.add_argument("--seed", type=int, default=DEFAULT_SEED,
                        help=f"integer seed, reduced to 32 bits like the browser "
                             f"(default {DEFAULT_SEED})")
    parser.add_argument("--output", default="maze_art.png", help="PNG path (default maze_art.png)")
    args = parser.parse_args(argv)
    print(render(args.cols, args.rows, args.seed, args.output))


if __name__ == "__main__":
    main()
