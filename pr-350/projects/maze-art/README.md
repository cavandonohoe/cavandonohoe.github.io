# Procedural Maze Art

Interactive browser demo: [maze-art.html](../../maze-art.html), live at <https://cavandonohoe.github.io/maze-art.html>.

A connected maze generated with randomized depth-first search, rendered as glowing outlines. Adjust columns, rows and seed; export a PNG. The browser normalizes the seed to an unsigned 32-bit integer and writes the value it actually used back into the Seed field.

The Python version renders a static PNG with NumPy and Matplotlib:

```sh
python maze_art.py --cols 22 --rows 14 --seed 7 --output maze_art.png
```

All flags are optional; the defaults match the browser's defaults (22 columns, 14 rows, seed 7). Columns and rows must be positive integers; the browser clamps them to 4-65 columns and 4-45 rows. The seed is any integer and is reduced to 32 bits the same way the browser does.

The Python generator ports the browser's mulberry32 PRNG, start cell and neighbor order, so the same columns, rows and seed produce the same wall layout in both. The rendering (line widths, glow, image size) is similar but not pixel-identical.

Inspired by an installation photograph. This is **not** an implementation of the displayed optimization equation; that would require definitions of its variables and constraints.
