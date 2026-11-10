# ============================================================
# FULL MCDM METHODOLOGY FOR SRI LANKAN EXPORT PRIORITIZATION
# 3 Weighting Methods (AHP, Entropy, Hybrid) × TOPSIS
# Plus Sensitivity Analysis
# ============================================================

# ============================================================
# STEP 0: LOAD PACKAGES
# ============================================================
packages <- c("tidyverse", "readr", "dplyr", "ggplot2", "tibble", "gt")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])
library(tidyverse); library(readr); library(dplyr)
library(ggplot2); library(tibble); library(gt)

# ============================================================
# STEP 1: LOAD ALL CSV FILES
# ============================================================
exports_df  <- read_csv("exports_srilanka.csv", name_repair = "universal")
imports_df  <- read_csv("imports_srilanka.csv", name_repair = "universal")
world_df    <- read_csv("total_world_exports.csv", name_repair = "universal")
growth_df   <- read_csv("exports_growth.csv", name_repair = "universal")
share_df    <- read_csv("Srilanka_export_in_share_of_total_world_exports.csv",
                        name_repair = "universal")
qty_gr_df   <- read_csv("export_quantity_growth.csv", name_repair = "universal")
quantity_df <- read_csv("exports_srilanka_quantity.csv", name_repair = "universal")

# ============================================================
# STEP 2: STANDARDIZE COLUMN NAMES
# ============================================================
expected_cols <- c("year","Tea","Coconut","Rubber","Spices",
                   "Food","Fish","Tobacco","Fruit_nut_vege")

fix_names <- function(df) {
  if (ncol(df) == length(expected_cols) + 1) df <- df[, -1]
  if (ncol(df) == length(expected_cols)) colnames(df) <- expected_cols
  df
}
exports_df  <- fix_names(exports_df); imports_df  <- fix_names(imports_df)
world_df    <- fix_names(world_df);   growth_df   <- fix_names(growth_df)
share_df    <- fix_names(share_df);   qty_gr_df   <- fix_names(qty_gr_df)
quantity_df <- fix_names(quantity_df)

# ============================================================
# STEP 3: RESHAPE WIDE → LONG
# ============================================================
longify <- function(df, val_name) {
  df %>% pivot_longer(-year, names_to = "product", values_to = val_name)
}
exp_long    <- longify(exports_df,  "exports")
imp_long    <- longify(imports_df,  "imports")
world_long  <- longify(world_df,    "world_exports")
rev_gr_long <- longify(growth_df,   "revenue_growth")
share_long  <- longify(share_df,    "world_contrib")
qty_gr_long <- longify(qty_gr_df,   "qty_growth")
qty_long    <- longify(quantity_df, "quantity")

# ============================================================
# STEP 4: MERGE INTO MASTER TABLE
# ============================================================
trade_df <- exp_long %>%
  left_join(imp_long,      by = c("year", "product")) %>%
  left_join(world_long,    by = c("year", "product")) %>%
  left_join(rev_gr_long,   by = c("year", "product")) %>%
  left_join(share_long,    by = c("year", "product")) %>%
  left_join(qty_gr_long,   by = c("year", "product")) %>%
  left_join(qty_long,      by = c("year", "product"))

# ============================================================
# STEP 5: CONSTANTS
# ============================================================
X_it <- 1.29963e+11
X_wt <- 26300000000000

# ============================================================
# STEP 6: COMPUTE CRITERIA
# ============================================================
trade_df <- trade_df %>%
  mutate(
    RCA  = (exports / X_it) / (world_exports / X_wt),
    RSCA = (RCA - 1) / (RCA + 1),
    TBI  = (exports - imports) / (exports + imports),
    import_dependency = imports / exports,
    sl_contribution   = exports / X_it,
    unit_price = exports / quantity
  )

# ============================================================
# STEP 7: AGGREGATE TO PRODUCT LEVEL (9 criteria)
# ============================================================
criteria_table <- trade_df %>%
  group_by(product) %>%
  summarise(
    RSCA            = mean(RSCA, na.rm = TRUE),
    TBI             = mean(TBI, na.rm = TRUE),
    Revenue_Growth  = mean(revenue_growth, na.rm = TRUE),
    Qty_Growth      = mean(qty_growth, na.rm = TRUE),
    World_Contrib   = mean(world_contrib, na.rm = TRUE),
    SL_Contrib      = mean(sl_contribution, na.rm = TRUE),
    Import_Dep      = mean(import_dependency, na.rm = TRUE),
    Volatility      = sd(revenue_growth, na.rm = TRUE),
    Mean_Unit_Price = mean(unit_price, na.rm = TRUE),
    SD_Unit_Price   = sd(unit_price, na.rm = TRUE),
    Price_Stability = Mean_Unit_Price / SD_Unit_Price
  )

cat("\n=== CRITERIA TABLE ===\n")
print(criteria_table)

# ============================================================
# STEP 8: DEFINE CRITERIA NAMES
# ============================================================
criteria_names <- c("RSCA", "TBI", "Revenue_Growth", "Qty_Growth",
                    "World_Contrib", "SL_Contrib", "Import_Dep",
                    "Volatility", "Price_Stability")

benefit_cols <- c("RSCA", "TBI", "Revenue_Growth", "Qty_Growth",
                  "World_Contrib", "SL_Contrib")

cost_cols    <- c("Import_Dep", "Volatility", "Price_Stability")

stopifnot(length(criteria_names) == 9)

# ============================================================
# STEP 9: BUILD DECISION MATRIX (8 × 9)
# ============================================================
decision_matrix <- criteria_table %>% select(product, all_of(criteria_names))
products <- decision_matrix$product
X <- as.matrix(decision_matrix[, -1])
colnames(X) <- criteria_names
rownames(X) <- products

cat("\n=== DECISION MATRIX ===\n")
print(round(X, 4))

# ============================================================
# PHASE 3.1: AHP WEIGHTS
# ============================================================
ahp_matrix <- matrix(c(
  # RSCA  TBI    RG    QG    WC    SLC  ID    Vol   PS
  1,     1,     3,    3,    2,    2,   4,    7,    2,    # RSCA
  1,     1,     2,    3,    2,    1,   3,    6,    1,    # TBI
  1/3,   1/2,   1,    1,    1,    1/2, 1,    3,    1,    # RG
  1/3,   1/3,   1,    1,    1,    1/2, 1,    2,    1/2,  # QG
  1/2,   1/2,   1,    1,    1,    1,   2,    3,    1,    # WC
  1/2,   1,     2,    2,    1,    1,   2,    5,    1,    # SLC
  1/4,   1/3,   1,    1,    1/2,  1/2, 1,    2,    1/2,  # ID
  1/7,   1/6,   1/3,  1/2,  1/3,  1/5, 1/2,  1,    1/4,  # Vol
  1/2,   1,     1,    2,    1,    1,   2,    4,    1     # PS
), nrow = 9, byrow = TRUE)


rownames(ahp_matrix) <- criteria_names
colnames(ahp_matrix) <- criteria_names

# Normalize and compute weights
norm_ahp <- sweep(ahp_matrix, 2, colSums(ahp_matrix), "/")
w_ahp <- rowMeans(norm_ahp)
w_ahp <- w_ahp / sum(w_ahp)
names(w_ahp) <- criteria_names

# Consistency check
Aw <- ahp_matrix %*% w_ahp
lambda_max <- mean(Aw / w_ahp)
CI <- (lambda_max - 9) / 8
RI <- 1.45
CR <- CI / RI

cat("\n=== AHP WEIGHTS ===\n")
print(round(w_ahp, 4))
cat("λ_max =", round(lambda_max, 4), "| CI =", round(CI, 4),
    "| CR =", round(CR, 4), "→",
    ifelse(CR < 0.10, "✅ Consistent", "❌ Revise"), "\n")


# ============================================================
# STEP 14: VISUALIZATION — AHP WEIGHTS BAR GRAPH
# ============================================================
weights_df <- data.frame(
  Criterion = criteria_names,
  Weight    = as.numeric(w_ahp)     # ← fixed
)

weights_df <- weights_df %>%
  mutate(
    Tier = case_when(
      Criterion == "RSCA" ~ "Tier 1 (Primary)",
      Criterion %in% c("TBI", "SL_Contrib") ~ "Tier 2 (High)",
      Criterion %in% c("Revenue_Growth", "Qty_Growth", "World_Contrib",
                       "Price_Stability") ~ "Tier 3 (Medium)",
      TRUE ~ "Tier 4 (Low)"
    )
  )

p_weights <- ggplot(weights_df,
                    aes(x = reorder(Criterion, Weight),
                        y = Weight,
                        fill = Tier)) +
  geom_col(width = 0.7, color = "black", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.3f", Weight)),
            hjust = -0.15, size = 4.2, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c(
    "Tier 1 (Primary)" = "#1B5E20",
    "Tier 2 (High)"    = "#2E7D32",
    "Tier 3 (Medium)"  = "#F9A825",
    "Tier 4 (Low)"     = "#C62828"
  )) +
  scale_y_continuous(limits = c(0, max(weights_df$Weight) * 1.20),
                     breaks = seq(0, 0.30, 0.05),
                     expand = c(0, 0)) +
  labs(
    title = "AHP Criterion Weights (9 criteria)",
    subtitle = paste0("Priority weights derived from pairwise comparison matrix (CR = ",
                      round(CR, 3), ")"),
    x = NULL,
    y = "Priority Weight",
    fill = "Importance Tier"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.text.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 11, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

print(p_weights)


# ============================================================
# PHASE 3.2: SHANNON ENTROPY WEIGHTS
# ============================================================
# Step 1: Min-max normalize to [0, 1]
# X- desition matrix
X_minmax <- apply(X, 2, function(col) {
  (col - min(col, na.rm = TRUE)) / (max(col, na.rm = TRUE) - min(col, na.rm = TRUE))
}) + 0.001   # Add epsilon to avoid log(0)

# Step 2: Compute proportions (Formula 3)
P <- sweep(X_minmax, 2, colSums(X_minmax), "/")

# Step 3: Compute entropy (Formula 4)
K <- nrow(P)
log_K <- log(K)
phi <- numeric(ncol(P))
for (j in 1:ncol(P)) {
  p_j <- P[, j]
  p_j_pos <- p_j[p_j > 0]
  phi[j] <- -(1/log_K) * sum(p_j_pos * log(p_j_pos))
}
names(phi) <- colnames(X)

# Step 4: Compute weights (Formula 5)
d_j <- 1 - phi
w_entropy <- d_j / sum(d_j)
names(w_entropy) <- colnames(X)

cat("\n=== SHANNON ENTROPY WEIGHTS ===\n")
print(round(w_entropy, 4))

# ============================================================
# CORRECTED SHANNON ENTROPY WEIGHTING FUNCTION
# ============================================================
calc_shannon_entropy <- function(mat) {
  # Step 1: Ensure all matrix values are strictly positive 
  # (Shift matrix by min value + small epsilon if any non-positive numbers exist)
  mat_pos <- apply(mat, 2, function(col) col - min(0, min(col, na.rm = TRUE)) + 1e-6)
  
  # Step 2: Compute relative proportions p_ij
  p <- sweep(mat_pos, 2, colSums(mat_pos), FUN = "/")
  
  # Step 3: Compute Entropy phi_j
  m <- nrow(mat)             # Number of alternatives (m = 8 products)
  k <- 1 / log(m)            # Normalization constant k = 1 / ln(m)
  
  # Calculate entropy per criterion (handling zero probabilities safely)
  entropy <- -k * colSums(p * log(p + 1e-12), na.rm = TRUE)
  
  # Step 4: Degree of diversification (d_j) and Final Weights (w_j)
  d <- 1 - entropy
  w <- d / sum(d)
  
  return(w)
}

# Execute Entropy Weighting
w_entropy <- calc_shannon_entropy(X)

cat("\n=== CORRECTED SHANNON ENTROPY WEIGHTS ===\n")
print(round(w_entropy, 4))

# ============================================================
# PHASE 3.3: HYBRID WEIGHTS (α = 0.5)
# ============================================================
alpha <- 0.5
w_hybrid <- alpha * w_ahp + (1 - alpha) * w_entropy
w_hybrid <- w_hybrid / sum(w_hybrid)
names(w_hybrid) <- criteria_names

cat("\n=== HYBRID WEIGHTS (α = 0.5) ===\n")
print(round(w_hybrid, 4))


# ============================================================
# PHASE 4: TOPSIS FUNCTION
# ============================================================
run_topsis <- function(X, w, benefit, cost) {
  # Step 1: Vector normalization
  norm_X <- sweep(X, 2, sqrt(colSums(X^2)), "/")
  
  # Step 2: Weighted normalized matrix
  w_aligned <- w[colnames(X)]
  weighted_X <- sweep(norm_X, 2, w_aligned, "*")
  
  # Step 3: PIS and NIS
  PIS <- NIS <- numeric(ncol(X))
  for (j in 1:ncol(X)) {
    if (colnames(X)[j] %in% benefit) {
      PIS[j] <- max(weighted_X[, j])
      NIS[j] <- min(weighted_X[, j])
    } else {
      PIS[j] <- min(weighted_X[, j])
      NIS[j] <- max(weighted_X[, j])
    }
  }
  names(PIS) <- colnames(X); names(NIS) <- colnames(X)
  
  # Step 4: Distances
  d_plus  <- sqrt(rowSums((weighted_X - matrix(PIS, nrow=nrow(X),
                                               ncol=ncol(X), byrow=TRUE))^2))
  d_minus <- sqrt(rowSums((weighted_X - matrix(NIS, nrow=nrow(X),
                                               ncol=ncol(X), byrow=TRUE))^2))
  
  # Step 5: Closeness coefficient
  CC <- d_minus / (d_plus + d_minus)
  
  data.frame(
    Product = rownames(X),
    d_plus  = round(d_plus, 4),
    d_minus = round(d_minus, 4),
    CC      = round(CC, 4),
    Rank    = rank(-CC)
  ) %>% arrange(Rank)
}

# ============================================================
# PHASE 4: APPLY TOPSIS WITH ALL 3 WEIGHTS
# ============================================================
res_ahp     <- run_topsis(X, w_ahp, benefit_cols, cost_cols)
res_entropy <- run_topsis(X, w_entropy, benefit_cols, cost_cols)
res_hybrid  <- run_topsis(X, w_hybrid, benefit_cols, cost_cols)

cat("\n=== AHP-TOPSIS RANKING ===\n"); print(res_ahp)
cat("\n=== ENTROPY-TOPSIS RANKING ===\n"); print(res_entropy)
cat("\n=== HYBRID-TOPSIS RANKING ===\n"); print(res_hybrid)



# ============================================================
# 3-IN-1 RANKING TABLE — PRODUCT-CENTRIC FORMAT
# ============================================================

# --- Combine ranks side by side per product ---
combined <- data.frame(
  Product         = res_ahp$Product,
  AHP_Rank        = res_ahp$Rank,
  Entropy_Rank    = res_entropy$Rank[match(res_ahp$Product, res_entropy$Product)],
  Hybrid_Rank     = res_hybrid$Rank[match(res_ahp$Product, res_hybrid$Product)],
  Avg_Rank        = round((res_ahp$Rank +
                             res_entropy$Rank[match(res_ahp$Product, res_entropy$Product)] +
                             res_hybrid$Rank[match(res_ahp$Product, res_hybrid$Product)]) / 3, 2)
) %>%
  mutate(Consensus_Rank = rank(Avg_Rank)) %>%
  arrange(Consensus_Rank)

cat("\n=== 3-IN-1 RANKING TABLE ===\n")
print(combined)

# ============================================================
# CONSENSUS RANK BAR CHART — Ranking PRODUCT
# ============================================================

# --- Build consensus table ---
consensus <- data.frame(
  Product        = res_ahp$Product,
  AHP_Rank       = res_ahp$Rank,
  Entropy_Rank   = res_entropy$Rank[match(res_ahp$Product, res_entropy$Product)],
  Hybrid_Rank    = res_hybrid$Rank[match(res_ahp$Product, res_hybrid$Product)]
) %>%
  mutate(
    Avg_Rank       = round((AHP_Rank + Entropy_Rank + Hybrid_Rank) / 3, 2),
    Consensus_Rank = rank(Avg_Rank)
  ) %>%
  arrange(Consensus_Rank)

# --- Fix product order (best at top) ---
consensus$Product <- factor(consensus$Product,
                            levels = consensus$Product[order(consensus$Consensus_Rank,
                                                             decreasing = TRUE)])

# --- Plot in the style of the example ---
p <- ggplot(consensus,
            aes(x = Consensus_Rank, y = Product, fill = Product)) +
  geom_col(width = 0.7, color = "black", linewidth = 0.4) +
  geom_text(aes(label = Consensus_Rank),
            hjust = -0.15, size = 5, fontface = "bold") +
  scale_fill_viridis_d(option = "D", guide = "none") +
  scale_x_continuous(limits = c(0, 9),
                     breaks = 1:8,
                     expand = c(0, 0)) +
  labs(
    title    = "Consensus Ranking of Sri Lankan Export Products",
    subtitle = "Average rank across AHP, Entropy, and Hybrid weighting methods",
    x        = "Consensus Rank (1 = best)",
    y        = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 15, hjust = 0),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.text.y   = element_text(size = 12, face = "bold", color = "black"),
    axis.text.x   = element_text(size = 11),
    axis.title.x  = element_text(size = 12, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "none"
  )

print(p)
#ggsave("consensus_ranking_styled.png", p, width = 10, height = 6, dpi = 300)
#ggsave("consensus_ranking_styled.pdf", p, width = 10, height = 6)


# ============================================================
# PHASE 5: SENSITIVITY ANALYSIS
# ============================================================
sensitivity_analysis <- function(remove_crit) {
  keep <- setdiff(criteria_names, remove_crit)
  X_sub <- X[, keep]
  
  # Recompute AHP weights (sub-matrix)
  ahp_sub <- ahp_matrix[keep, keep]
  norm_sub <- sweep(ahp_sub, 2, colSums(ahp_sub), "/")
  w_sub <- rowMeans(norm_sub)
  w_sub <- w_sub / sum(w_sub)
  names(w_sub) <- keep
  
  benefit_sub <- intersect(benefit_cols, keep)
  cost_sub    <- intersect(cost_cols, keep)
  
  run_topsis(X_sub, w_sub, benefit_sub, cost_sub)
}

# Run sensitivity for each criterion
sens_list <- lapply(criteria_names, function(c) {
  res <- sensitivity_analysis(c)
  data.frame(
    Product = res$Product,
    Rank    = res$Rank,
    Scenario = paste0("Without_", c)
  )
})
sens_all <- bind_rows(sens_list)

sens_table <- sens_all %>%
  pivot_wider(names_from = Scenario, values_from = Rank) %>%
  arrange(Product)

cat("\n=== SENSITIVITY ANALYSIS ===\n")
print(sens_table)

# ============================================================
# PHASE 6: VISUALIZATIONS
# ============================================================
# --- 6.3 Consensus ranking bar chart ---
p_consensus <- ggplot(consensus,
                      aes(x = reorder(Product, -Consensus_Rank),
                          y = Consensus_Rank, fill = factor(Consensus_Rank))) +
  geom_col(width = 0.7, color = "black", linewidth = 0.3) +
  geom_text(aes(label = Consensus_Rank), hjust = -0.3,
            size = 4.5, fontface = "bold") +
  coord_flip() +
  scale_fill_viridis_d(option = "D", direction = -1) +
  labs(
    title = "Consensus Ranking of Sri Lankan Export Products",
    subtitle = "Average of AHP, Entropy, and Hybrid TOPSIS rankings",
    x = NULL, y = "Consensus Rank", fill = "Rank"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

print(p_consensus)
#ggsave("consensus_ranking.png", p_consensus, width = 10, height = 6, dpi = 300)

# ============================================================
# PHASE 7: EXPORT RESULTS
# ============================================================



















# ============================================================
# CONSISTENT TIER-COLORED BAR CHARTS
# AHP, Shannon Entropy, Hybrid Weights
# ============================================================


# ============================================================
# SHARED TIER FUNCTION (based on ranking position)
# ============================================================
assign_tiers <- function(weights_vec) {
  df <- data.frame(
    Criterion = names(weights_vec),
    Weight    = as.numeric(weights_vec)
  ) %>%
    arrange(desc(Weight)) %>%
    mutate(
      Rank = row_number(),
      Tier = case_when(
        Rank <= 2                ~ "Highest",
        Rank <= 4                ~ "High",
        Rank <= 6                ~ "Medium",
        TRUE                     ~ "Low"
      )
    ) %>%
    select(-Rank)
  
  # Fix criterion order to keep the same order in all charts
  df$Criterion <- factor(df$Criterion, levels = df$Criterion[order(df$Weight)])
  df
}

# ============================================================
# SHARED COLOR PALETTE
# ============================================================
tier_colors <- c(
  "Highest" = "#B71C1C",   # dark red
  "High"    = "#F9A825",   # amber
  "Medium"  = "#66BB6A",   # light green
  "Low"     = "#1B5E20"    # dark green
)

# ============================================================
# SHARED PLOT FUNCTION
# ============================================================
plot_weights <- function(df, title_text, subtitle_text) {
  ggplot(df,
         aes(x = Criterion, y = Weight, fill = Tier)) +
    geom_col(width = 0.7, color = "black", linewidth = 0.3) +
    geom_text(aes(label = sprintf("%.3f", Weight)),
              hjust = -0.15, size = 4.2, fontface = "bold") +
    coord_flip() +
    scale_fill_manual(values = tier_colors) +
    scale_y_continuous(limits = c(0, max(df$Weight) * 1.25),
                       breaks = seq(0, 0.30, 0.05),
                       expand = c(0, 0)) +
    labs(
      title    = title_text,
      subtitle = subtitle_text,
      x        = NULL,
      y        = "Weight",
      fill     = "Importance Tier"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 14, hjust = 0),
      plot.subtitle = element_text(size = 11, color = "gray40"),
      axis.text.y   = element_text(size = 11, face = "bold"),
      axis.text.x   = element_text(size = 10),
      axis.title.x  = element_text(size = 11, face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      legend.position    = "bottom",
      legend.title       = element_text(face = "bold")
    )
}

# ============================================================
# 1. AHP WEIGHTS CHART
# ============================================================
ahp_df <- assign_tiers(w_ahp)

p_ahp <- plot_weights(
  ahp_df,
  "AHP Criterion Weights (9 criteria)",
  paste0("Priority weights from pairwise comparison matrix (CR = ",
         round(CR, 3), ")")
)

print(p_ahp)
#ggsave("AHP_weights_bar.png", p_ahp, width = 11, height = 7, dpi = 300)

# ============================================================
# 2. SHANNON ENTROPY WEIGHTS CHART
# ============================================================
entropy_df <- assign_tiers(w_entropy)

p_entropy <- plot_weights(
  entropy_df,
  "Shannon Entropy Criterion Weights (9 criteria)",
  "Objective weights derived from data variability"
)

print(p_entropy)
#ggsave("Entropy_weights_bar.png", p_entropy, width = 11, height = 7, dpi = 300)

# ============================================================
# 3. HYBRID WEIGHTS CHART
# ============================================================
hybrid_df <- assign_tiers(w_hybrid)

p_hybrid <- plot_weights(
  hybrid_df,
  "Hybrid Criterion Weights (9 criteria, α = 0.5)",
  "Combined weights from AHP (50%) and Shannon Entropy (50%)"
)

print(p_hybrid)

#ggsave("Hybrid_weights_bar.png", p_hybrid, width = 11, height = 7, dpi = 300)

# ============================================================
# 4. COMBINED 3-IN-1 CHART (Optional)
# ============================================================
combined_df <- bind_rows(
  ahp_df     %>% mutate(Method = "AHP"),
  entropy_df %>% mutate(Method = "Entropy"),
  hybrid_df  %>% mutate(Method = "Hybrid")
) %>%
  mutate(Method = factor(Method, levels = c("AHP", "Entropy", "Hybrid")))

p_3in1 <- ggplot(combined_df,
                 aes(x = reorder(Criterion, Weight),
                     y = Weight,
                     fill = Tier)) +
  geom_col(position = position_dodge(width = 0.85),
           width = 0.7, color = "black", linewidth = 0.3) +
  coord_flip() +
  facet_wrap(~ Method, ncol = 3) +
  scale_fill_manual(values = tier_colors) +
  scale_y_continuous(limits = c(0, 0.30),
                     breaks = seq(0, 0.30, 0.05),
                     expand = c(0, 0)) +
  labs(
    title    = "Criterion Weights Across 3 Weighting Methods",
    subtitle = "AHP (subjective) vs Entropy (objective) vs Hybrid (α = 0.5)",
    x        = NULL,
    y        = "Weight",
    fill     = "Importance Tier"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.text.y   = element_text(size = 9, face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position    = "bottom",
    strip.text         = element_text(face = "bold", size = 12)
  )

print(p_3in1)







#_____________________________________________________________________________________________________________________________






# ============================================================
# SAW — Simple Additive Weighting
# ============================================================

run_saw <- function(X, w, benefit, cost) {
  # Step 1: Normalize
  norm_X <- matrix(0, nrow = nrow(X), ncol = ncol(X))
  rownames(norm_X) <- rownames(X)
  colnames(norm_X) <- colnames(X)
  
  for (j in 1:ncol(X)) {
    if (colnames(X)[j] %in% benefit) {
      norm_X[, j] <- X[, j] / max(X[, j], na.rm = TRUE)
    } else {
      norm_X[, j] <- min(X[, j], na.rm = TRUE) / X[, j]
    }
  }
  
  # Step 2: Weighted sum
  w_aligned <- w[colnames(X)]
  S <- as.numeric(norm_X %*% w_aligned)
  
  # Step 3: Rank
  data.frame(
    Product = rownames(X),
    SAW_Score = round(S, 4),
    Rank      = rank(-S)
  ) %>% arrange(Rank)
}

# Apply SAW with all 3 weight sets
saw_ahp     <- run_saw(X, w_ahp, benefit_cols, cost_cols)
saw_entropy <- run_saw(X, w_entropy, benefit_cols, cost_cols)
saw_hybrid  <- run_saw(X, w_hybrid, benefit_cols, cost_cols)

cat("\n=== SAW-AHP ===\n"); print(saw_ahp)
cat("\n=== SAW-ENTROPY ===\n"); print(saw_entropy)
cat("\n=== SAW-HYBRID ===\n"); print(saw_hybrid)


