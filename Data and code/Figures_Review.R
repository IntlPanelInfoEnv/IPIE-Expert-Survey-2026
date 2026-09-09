# ============================================================================
# IPIE 2026 Expert Survey — figures added in response to review
# Estimates with 95% Wilson intervals: the outlook trend, the group
# comparisons the report narrates, and the generative-AI latent classes.
# Run Review_Analyses.R first (writes the review_*.csv inputs).
# ============================================================================
suppressMessages({library(rio); library(tidyverse)})
OUT <- "../Figures"

# R's default pdf() maps the hyphen to U+2212 (minus) in the embedded font, so
# hyphenated labels such as "data-protection" render with a minus sign. quartz
# keeps U+002D on macOS; cairo_pdf does elsewhere.
pdf_dev <- if (capabilities("aqua")) {
  function(filename, ...) grDevices::quartz(file = filename, type = "pdf", ...)
} else grDevices::cairo_pdf

# 2025/2026 report palette
red <- "#d50e00"; blue <- "#0072B2"; orange <- "#E69F00"; grey <- "gray45"

theme_ipie <- theme_bw(base_size = 16) +
  theme(axis.title = element_text(size = 15, color = "black"),
        axis.text = element_text(size = 14, color = "black"),
        panel.grid.minor = element_blank(),
        legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 14),
        plot.margin = margin(10, 16, 8, 8))

#### Figure 7b. Outlook trend 2023-2026 with CIs ####

tr <- import("review_trend.csv")
f_trend <- ggplot(tr, aes(year, pct)) +
  geom_line(color = red, linewidth = 1.1) +
  geom_errorbar(aes(ymin = lo, ymax = hi), color = red, width = 0.08, linewidth = 0.7) +
  geom_point(color = red, size = 3.5) +
  geom_text(aes(y = hi, label = sprintf("%.0f%%", pct)), vjust = -0.9, size = 5.5) +
  scale_y_continuous(limits = c(40, 90), breaks = seq(40, 90, 10)) +
  labs(x = NULL, y = "% expecting the information environment to worsen") +
  theme_ipie + theme(legend.position = "none")
ggsave(file.path(OUT, "fig_trend_outlook.pdf"), f_trend, width = 9, height = 6, device = pdf_dev)
ggsave(file.path(OUT, "fig_trend_outlook.png"), f_trend, width = 9, height = 6, dpi = 300, bg = "white")

#### Figure 9. Developing vs developed, all narrated comparisons, with CIs ####

# Rebuild the point estimates and intervals for the items the report contrasts.
# Every developed/developing comparison the report states in prose, taken from the
# Findings section rather than by eye, so the figure is a complete audit of the
# report's own group claims. Grouped by the battery each item comes from.
# `all_denom` = share of all experts in the group (multi-select or no missingness);
# otherwise the denominator is valid responses to that item.
source_items <- tribble(
  ~block, ~col, ~label, ~cats, ~all_denom,
  "Healthy environment", "Q15_1", "Diversity of voices is essential",
    list("Absolutely essential"), FALSE,

  "Threats", "Q36_3", "Misinformation a big or extreme threat",
    list(c("Big threat", "An extreme threat")), FALSE,
  "Threats", "Q36_1", "Polarization a big or extreme threat",
    list(c("Big threat", "An extreme threat")), FALSE,
  "Threats", "Q36_6", "Filter bubbles a big or extreme threat",
    list(c("Big threat", "An extreme threat")), FALSE,
  "Threats", "Q36_2", "Generative AI a big or extreme threat",
    list(c("Big threat", "An extreme threat")), FALSE,

  "The public", "Q_6_1", "Can find diverse viewpoints easily",
    list(c("Very easy. Online content includes many diverse viewpoints.",
           "Somewhat easy. A variety of viewpoints are available, though some are difficult to find.")), TRUE,
  "The public", "Q_8_1", "Most feel confident telling fact from falsehood",
    list(c("Many, more than half of the population", "The vast majority of the population")), TRUE,
  "The public", "Q_8_2", "Information supports civic understanding",
    list(c("To a considerable extent", "To a very great extent")), TRUE,

  "Research conditions", "Q47c", "Government censorship",
    list("Government censorship"), TRUE,
  "Research conditions", "Q47s", "Government surveillance",
    list("Government surveillance or monitoring"), TRUE,

  "Research conditions", "Q47f", "Government control of research funding",
    list("Government control over research funding"), TRUE,
  "Research conditions", "Q47p", "Pressure to align with a political agenda",
    list("Pressures to align current research"), TRUE,

  "Technology and AI", "Q46_3", "Generative AI effects positive",
    list(c("Slightly positive", "Positive", "Very positive")), TRUE,
  "Technology and AI", "Q46_2", "Recommender systems effects positive",
    list(c("Slightly positive", "Positive", "Very positive")), TRUE,
  "Technology and AI", "Q43_2", "AI summaries improve accuracy of information",
    list(c("Improve a little", "Improve", "Improve a lot")), TRUE,
  "Technology and AI", "Q43_4", "AI summaries improve accuracy of conclusions",
    list(c("Improve a little", "Improve", "Improve a lot")), TRUE,
  "Technology and AI", "Q21_1", "AI-generated text will worsen environment",
    list(c("Slightly worsen", "Somewhat worsen", "Greatly worsen")), TRUE,
  "Technology and AI", "Q21_4", "AI-generated video will worsen environment",
    list(c("Slightly worsen", "Somewhat worsen", "Greatly worsen")), TRUE
)

developed_countries <- c("Australia","Austria","Belgium","Canada","Czech Republic","Denmark",
  "France","Germany","Greece","Ireland","Israel","Italy","Japan","Netherlands","New Zealand",
  "Norway","Portugal","Singapore","Slovakia","South Korea","Spain","Sweden","Switzerland","UK",
  "USA","Croatia","San Marino","Andorra","Latvia","Lithuania","Estonia","Malta","Liechtenstein",
  "Monaco","Slovenia","Cyprus","Taiwan","Finland","Iceland","Luxembourg")
d <- import("Data.xlsx") %>%
  filter(as.numeric(Progress) > 90) %>% mutate(across(where(is.character), trimws))
cc <- d$Q9; cc <- gsub("Ireland \\{Republic\\}", "Ireland", cc)
cc[cc == "Korea South"] <- "South Korea"; cc[cc == "United Kingdom"] <- "UK"
cc[cc == "United States"] <- "USA"; cc[cc == "Russian Federation"] <- "Russia"
d$Developed <- ifelse(cc %in% developed_countries, "Developed", "Developing")
# the three barrier items are select-all, so they are matched on the Q47 string
d$Q47c <- d$Q47; d$Q47s <- d$Q47; d$Q47p <- d$Q47; d$Q47f <- d$Q47
# the developed group is split so the US can be read separately throughout
d$Grp <- ifelse(cc == "USA", "US",
                ifelse(d$Developed == "Developed", "Other developed", "Developing"))

wilson <- function(x, n) {
  z <- 1.959964; p <- x / n; dd <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / dd
  hw <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / dd
  tibble(pct = 100 * p, lo = 100 * pmax(0, centre - hw), hi = 100 * pmin(1, centre + hw))
}

cmp <- pmap_dfr(source_items, function(block, col, label, cats, all_denom) {
  cats <- cats[[1]]
  is_bar <- col %in% c("Q47c", "Q47s", "Q47p", "Q47f")
  map_dfr(c("Developing", "Other developed", "US"), function(g) {
    s <- d$Grp == g
    hit <- if (is_bar) grepl(cats, d[[col]], fixed = TRUE) else d[[col]] %in% cats
    n <- if (all_denom || is_bar) sum(s) else sum(s & !is.na(d[[col]]))
    bind_cols(tibble(block = block, item = label, grp = g),
              wilson(sum(hit & s, na.rm = TRUE), n))
  })
})
cmp$block <- factor(cmp$block, levels = unique(source_items$block))
cmp$grp <- factor(cmp$grp, levels = c("Other developed", "US", "Developing"))
ord <- cmp %>% filter(grp == "Developing") %>% arrange(block, pct) %>% pull(item)
cmp$item <- factor(cmp$item, levels = rev(ord))
dodge <- position_dodge(0.6)
# One faceted panel per block, stacked with gtable so that each panel's height is
# proportional to its number of rows (facet_wrap would give every panel the same
# height). Direct labels stand in for a legend, in the whitespace of the first block.
blocks <- levels(cmp$block)
top <- tibble(grp  = factor(c("Developing", "US", "Other developed"), levels = levels(cmp$grp)),
              lab  = c("Developing", "USA", "Other developed"),
              xlab = 64, ypos = 1.36 - 0.36 * (0:2), block = blocks[1])
block_plot <- function(b, last) {
  d <- cmp %>% filter(block == b) %>% droplevels()
  p <- ggplot(d, aes(pct, item, color = grp)) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = 0.8, position = dodge) +
    geom_point(size = 3.2, position = dodge) +
    facet_wrap(~ block) +
    scale_color_manual(values = c("Developing" = orange, "Other developed" = blue, "US" = red)) +
    scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                       expand = expansion(mult = c(0.02, 0.02))) +
    labs(x = if (last) "% of experts (95% CI)" else NULL, y = NULL) +
    theme_bw(base_size = 15) +
    theme(axis.title.x = element_text(size = 14, color = "black"),
          axis.text = element_text(size = 13.5, color = "black"),
          strip.background = element_rect(fill = "grey94", color = NA),
          strip.text = element_text(size = 13, face = "bold", hjust = 0),
          panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "none",
          plot.margin = margin(4, 18, if (last) 8 else 4, 8))
  if (b == blocks[1]) p <- p + geom_text(data = top, aes(x = xlab, y = ypos, label = lab, color = grp),
                                         hjust = 0, size = 3.7, fontface = "bold",
                                         inherit.aes = FALSE, show.legend = FALSE)
  if (!last) p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  p
}
grobs <- lapply(seq_along(blocks), function(i) ggplotGrob(block_plot(blocks[i], i == length(blocks))))
# equal widths so the y labels align, then panel heights proportional to row counts
widths <- do.call(grid::unit.pmax, lapply(grobs, function(g) g$widths))
n_rows <- sapply(blocks, function(b) n_distinct(cmp$item[cmp$block == b]))
for (i in seq_along(grobs)) {
  grobs[[i]]$widths <- widths
  pr <- grobs[[i]]$layout$t[grepl("^panel", grobs[[i]]$layout$name)]
  grobs[[i]]$heights[pr] <- grid::unit(n_rows[i] + 0.6, "null")
}
f_cmp <- do.call(rbind, c(grobs, size = "first"))
save_cmp <- function(file, ...) { ggsave(file, f_cmp, width = 10, height = 9.8, ...) }
save_cmp(file.path(OUT, "fig_economy_comparison.pdf"), device = pdf_dev)
save_cmp(file.path(OUT, "fig_economy_comparison.png"), dpi = 300, bg = "white")
print(cmp %>% mutate(across(where(is.numeric), ~round(.x, 1))), n = Inf)

#### Figure A1. Generative-AI latent classes, by format ####

suppressMessages(library(poLCA))
recode_ai <- function(x) case_when(x %in% c("Greatly worsen", "Somewhat worsen", "Slightly worsen") ~ 1,
                                   x == "Neither" ~ 2,
                                   x %in% c("Slightly improve", "Somewhat improve", "Greatly improve") ~ 3)
L <- d %>% transmute(Q21_1 = recode_ai(Q21_1), Q21_2 = recode_ai(Q21_2),
                     Q21_3 = recode_ai(Q21_3), Q21_4 = recode_ai(Q21_4)) %>% drop_na()
set.seed(42)
m <- poLCA(cbind(Q21_1, Q21_2, Q21_3, Q21_4) ~ 1, data = L, nclass = 3, nrep = 20, verbose = FALSE)

fmt <- c(Q21_1 = "Text", Q21_2 = "Images", Q21_3 = "Voices", Q21_4 = "Video")
cls_lab <- c("Expect worsening (62%)", "Expect improvement (27%)", "Divided by format (11%)")
probs <- imap_dfr(m$probs, function(mat, q)
  tibble(format = fmt[[q]], class = seq_len(nrow(mat)), p_worsen = 100 * mat[, 1]))
# order classes: worsening / improving / divided, by their share
shares <- round(m$P * 100)
probs$class_lab <- factor(cls_lab[match(probs$class, order(-shares))],
                          levels = cls_lab)
probs$format <- factor(probs$format, levels = c("Text", "Images", "Voices", "Video"))

f_lca <- ggplot(probs, aes(format, p_worsen, fill = class_lab)) +
  geom_col(position = position_dodge(0.78), width = 0.72) +
  geom_text(aes(label = sprintf("%.0f", p_worsen)), position = position_dodge(0.78),
            vjust = -0.4, size = 4.6) +
  scale_fill_manual(values = c(red, blue, orange)) +
  scale_y_continuous(limits = c(0, 108), breaks = seq(0, 100, 25),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "% within group expecting this format to worsen") +
  theme_ipie + theme(panel.grid.major.x = element_blank()) +
  guides(fill = guide_legend(nrow = 1))
ggsave(file.path(OUT, "fig_ai_classes.pdf"), f_lca, width = 11, height = 6.5, device = pdf_dev)
ggsave(file.path(OUT, "fig_ai_classes.png"), f_lca, width = 11, height = 6.5, dpi = 300, bg = "white")

cat("Review figures written to", OUT, "\n")

