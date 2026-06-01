"""
Figure 2 — SWOT strategic-position scatter (full sample, N=293).

X axis: Internal_diff  (S_index - W_index)
Y axis: External_diff  (O_index - T_index)
Color : participation (participant vs non-participant)
Marker: strategic quadrant (SO / WO / ST / WT)

Reads the scored data file. Adjust DATA_PATH and column names to match
strategy_data_with_SWOT_scores.xlsx.
"""
import matplotlib.pyplot as plt
import pandas as pd

DATA_PATH = "strategy_data_with_SWOT_scores.xlsx"

COL_INTERNAL = "Internal_diff"
COL_EXTERNAL = "External_diff"
COL_PARTICIPATE = "livestream_flag"   # 1 = participant, 0 = non-participant
COL_STRATEGY = "Strategy"             # values: SO / WO / ST / WT

MARKERS = {"SO": "o", "WO": "s", "ST": "^", "WT": "D"}
PART_COLORS = {1: "#1565C0", 0: "#C62828"}
PART_LABEL = {1: "Participant", 0: "Non-participant"}

df = pd.read_excel(DATA_PATH)

fig, ax = plt.subplots(figsize=(7.5, 6.5))

# Quadrant guides
ax.axhline(0, color="grey", lw=0.8, ls="--", zorder=1)
ax.axvline(0, color="grey", lw=0.8, ls="--", zorder=1)

for strat, mk in MARKERS.items():
    for flag, col in PART_COLORS.items():
        sub = df[(df[COL_STRATEGY] == strat) & (df[COL_PARTICIPATE] == flag)]
        if len(sub):
            ax.scatter(sub[COL_INTERNAL], sub[COL_EXTERNAL],
                       marker=mk, c=col, s=42, alpha=0.75,
                       edgecolors="white", linewidths=0.4, zorder=3)

# Quadrant labels
xmax = df[COL_INTERNAL].abs().max()
ymax = df[COL_EXTERNAL].abs().max()
ax.text(xmax * 0.6, ymax * 0.9, "SO", fontsize=14, fontweight="bold",
        color="grey", alpha=0.5)
ax.text(-xmax * 0.7, ymax * 0.9, "WO", fontsize=14, fontweight="bold",
        color="grey", alpha=0.5)
ax.text(xmax * 0.6, -ymax * 0.95, "ST", fontsize=14, fontweight="bold",
        color="grey", alpha=0.5)
ax.text(-xmax * 0.7, -ymax * 0.95, "WT", fontsize=14, fontweight="bold",
        color="grey", alpha=0.5)

ax.set_xlabel("Internal_diff  (S − W)", fontsize=11)
ax.set_ylabel("External_diff  (O − T)", fontsize=11)
ax.set_title("Farmers' SWOT Strategic Positions and Participation",
             fontsize=12, fontweight="bold")

# Two-part legend: color = participation, marker = strategy
from matplotlib.lines import Line2D
color_handles = [Line2D([0], [0], marker="o", color="w",
                        markerfacecolor=PART_COLORS[f], markersize=9,
                        label=PART_LABEL[f]) for f in (1, 0)]
marker_handles = [Line2D([0], [0], marker=MARKERS[s], color="grey",
                         linestyle="", markersize=8, label=s)
                  for s in MARKERS]
leg1 = ax.legend(handles=color_handles, title="Participation",
                 loc="upper left", fontsize=9, framealpha=0.9)
ax.add_artist(leg1)
ax.legend(handles=marker_handles, title="Strategic type",
          loc="lower right", fontsize=9, framealpha=0.9)

fig.tight_layout()
fig.savefig("figures/fig2_swot_scatter.png", dpi=300, bbox_inches="tight")
fig.savefig("figures/fig2_swot_scatter.pdf", bbox_inches="tight")
print("Saved figures/fig2_swot_scatter.{png,pdf}")
