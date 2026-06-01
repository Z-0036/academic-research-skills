"""
Figure 1 — AHP expert-derived SWOT weight treemap.

Dimension-level weights are confirmed:
    S = 0.391, W = 0.252, O = 0.182, T = 0.174  (internal 0.643 / external 0.357)

Item-level weights (per dimension, must sum to 1 WITHIN each dimension) are
placeholders below. Replace ITEM_WEIGHTS with the real AHP item weights to
render the two-level (dimension -> item) treemap. If left as None, the script
renders the dimension-level treemap only.
"""
import matplotlib.pyplot as plt
import squarify

DIM_WEIGHTS = {"S": 0.391, "W": 0.252, "O": 0.182, "T": 0.174}

DIM_COLORS = {"S": "#2E7D32", "W": "#C62828", "O": "#1565C0", "T": "#F9A825"}

# Replace with real within-dimension AHP item weights (each dict sums to 1.0).
ITEM_WEIGHTS = None
# Example structure once available:
# ITEM_WEIGHTS = {
#     "S": {"Prod_Quality": .30, "Tech_Level": .25, "Comm_Skill": .25, "Price_Comp": .20},
#     "W": {"Biz_Scale": .30, "Financial": .30, "Live_Exp": .20, "Device_Diff": .20},
#     "O": {"Mkt_Demand": .30, "Gov_Support": .25, "Platform_Sup": .25, "Net_Infra": .20},
#     "T": {"Cons_Aware": .25, "Comp_Rivalry": .25, "Cons_Trust": .25, "Logistics": .25},
# }

DIM_LABEL = {"S": "Strengths", "W": "Weaknesses",
             "O": "Opportunities", "T": "Threats"}


def lighten(hex_color, factor):
    hex_color = hex_color.lstrip("#")
    r, g, b = (int(hex_color[i:i + 2], 16) for i in (0, 2, 4))
    r = int(r + (255 - r) * factor)
    g = int(g + (255 - g) * factor)
    b = int(b + (255 - b) * factor)
    return f"#{r:02X}{g:02X}{b:02X}"


fig, ax = plt.subplots(figsize=(8, 5.5))

if ITEM_WEIGHTS is None:
    sizes = [DIM_WEIGHTS[d] for d in DIM_WEIGHTS]
    colors = [DIM_COLORS[d] for d in DIM_WEIGHTS]
    labels = [f"{DIM_LABEL[d]} ({DIM_WEIGHTS[d]:.3f})" for d in DIM_WEIGHTS]
else:
    sizes, colors, labels = [], [], []
    for d in DIM_WEIGHTS:
        for i, (item, w) in enumerate(ITEM_WEIGHTS[d].items()):
            global_w = DIM_WEIGHTS[d] * w
            sizes.append(global_w)
            colors.append(lighten(DIM_COLORS[d], 0.15 + 0.18 * i))
            labels.append(f"{item}\n{global_w:.3f}")

squarify.plot(sizes=sizes, label=labels, color=colors, alpha=0.92,
              ax=ax, text_kwargs={"fontsize": 9, "color": "black"},
              pad=True)
ax.axis("off")
ax.set_title("AHP Expert-Derived SWOT Dimension Weights",
             fontsize=13, fontweight="bold", pad=12)

fig.tight_layout()
fig.savefig("figures/fig1_ahp_treemap.png", dpi=300, bbox_inches="tight")
fig.savefig("figures/fig1_ahp_treemap.pdf", bbox_inches="tight")
print("Saved figures/fig1_ahp_treemap.{png,pdf}")
