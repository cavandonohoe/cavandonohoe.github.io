import numpy as np
import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection

# Randomized depth-first maze with glowing wall outlines.
def maze_segments(cols=22, rows=14, seed=7):
    rng = np.random.default_rng(seed)
    walls = np.ones((rows, cols, 4), dtype=bool)  # north east south west
    seen = np.zeros((rows, cols), dtype=bool)
    seen[0, 0] = True
    stack = [(0, 0)]
    dirs = [(-1, 0, 0, 2), (0, 1, 1, 3), (1, 0, 2, 0), (0, -1, 3, 1)]
    while stack:
        r, c = stack[-1]
        options = [(r+dr, c+dc, a, b) for dr, dc, a, b in dirs
                   if 0 <= r+dr < rows and 0 <= c+dc < cols and not seen[r+dr, c+dc]]
        if not options:
            stack.pop()
            continue
        nr, nc, a, b = options[rng.integers(len(options))]
        walls[r, c, a] = False
        walls[nr, nc, b] = False
        seen[nr, nc] = True
        stack.append((nr, nc))
    segments = []
    for r in range(rows):
        for c in range(cols):
            x, y = c, rows-r-1
            if walls[r,c,0]: segments.append([(x,y+1),(x+1,y+1)])
            if walls[r,c,3]: segments.append([(x,y),(x,y+1)])
            if r == rows-1 and walls[r,c,2]: segments.append([(x,y),(x+1,y)])
            if c == cols-1 and walls[r,c,1]: segments.append([(x+1,y),(x+1,y+1)])
    return segments

def render(cols=22, rows=14, seed=7, output="maze_art.png"):
    segments = maze_segments(cols, rows, seed)
    fig, ax = plt.subplots(figsize=(14,9), facecolor="#060b11")
    ax.set_facecolor("#060b11")
    for width, alpha, color in [(10,.025,"#4cc9ff"),(5,.075,"#65d8ff"),(2,.32,"#a7ecff"),(.75,.96,"#e5faff")]:
        ax.add_collection(LineCollection(segments, colors=color, linewidths=width, alpha=alpha))
    ax.set_xlim(-.8, cols+.8); ax.set_ylim(-.8, rows+.8)
    ax.set_aspect("equal"); ax.axis("off")
    fig.savefig(output, dpi=180, facecolor=fig.get_facecolor(), bbox_inches="tight", pad_inches=.1)
    plt.close(fig)
    return output

if __name__ == "__main__":
    render()
