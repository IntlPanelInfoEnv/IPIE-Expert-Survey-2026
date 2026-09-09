# ============================================================================
# IPIE 2026 Expert Survey — cross-wave trend figures (2024-2026)
# Estimates with 95% Wilson intervals for the items that are worded identically
# across waves. Q36 threats are excluded (battery cut from 13 to 6 items in 2026).
# One shared dot-plot grammar for all three, matching the economy-comparison figure.
# ============================================================================
suppressMessages({library(rio); library(tidyverse)})
OUT <- "../Figures"

# R's default pdf() maps the hyphen to U+2212 (minus) in the embedded font, so
# hyphenated labels such as "data-protection" render with a minus sign. quartz
# keeps U+002D on macOS; cairo_pdf does elsewhere.
pdf_dev <- if (capabilities("aqua")) {
  function(filename, ...) grDevices::quartz(file = filename, type = "pdf", ...)
} else grDevices::cairo_pdf

yr_pal <- c("2024" = "#9ecae1", "2025" = "#4292c6", "2026" = "#08519c")

wilson <- function(x, n) {
  z <- 1.959964; p <- x / n; d <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / d
  hw <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / d
  tibble(pct = 100 * p, lo = 100 * pmax(0, centre - hw), hi = 100 * pmin(1, centre + hw), n = n)
}

# shared look: items on y ordered by the latest wave, one dot per year with its interval
dot_trend <- function(d, xlab, xmin = 0, xmax, file, height, width = 11) {
  # years run top-to-bottom within each item, oldest first, so the reading order
  # matches the time order; the top row is labelled directly so the reader never
  # has to consult the legend to tell the waves apart.
  yrs <- sort(unique(as.character(d$year)))          # oldest first for the legend
  d$year <- factor(as.character(d$year), levels = rev(yrs))  # newest first so dodge reads oldest-on-top
  ord <- d %>% filter(year == "2026") %>% arrange(pct) %>% pull(item) %>% as.character()
  d$item <- factor(as.character(d$item), levels = ord)
  dodge <- position_dodge(0.66)
  top <- d %>% filter(item == tail(ord, 1))
  p <- ggplot(d, aes(pct, item, color = year)) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = 0.8, position = dodge) +
    geom_point(size = 3.4, position = dodge) +
    geom_text(data = top, aes(x = lo, label = year), position = dodge,
              hjust = 1.35, size = 5.2, fontface = "bold", show.legend = FALSE) +
    scale_color_manual(values = yr_pal, breaks = yrs) +
    scale_x_continuous(limits = c(xmin, xmax), breaks = seq(xmin, xmax, 10),
                       expand = expansion(mult = c(0.06, 0.02))) +
    labs(x = xlab, y = NULL) +
    theme_bw(base_size = 17) +
    theme(axis.title.x = element_text(size = 16, color = "black"),
          axis.text = element_text(size = 18, color = "black"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          plot.margin = margin(10, 18, 8, 8))
  ggsave(file.path(OUT, paste0(file, ".pdf")), p, width = width, height = height,
         device = pdf_dev)
  ggsave(file.path(OUT, paste0(file, ".png")), p, width = width, height = height,
         dpi = 300, bg = "white")
  p
}

#### Load the three waves ####

lt <- function(p) read_tsv(p, locale = locale(encoding = "UTF-16LE"), show_col_types = FALSE)[-c(1, 2), ]
prep <- function(d, drop_nonres) {
  d <- d %>% mutate(Progress = as.numeric(Progress)) %>% filter(Progress > 90) %>%
    mutate(across(where(is.character), trimws))
  if (drop_nonres) d <- d %>% filter(Q2 != "I’m not a researcher")
  d
}
D24 <- prep(lt("../../2024/Stats/Data/Expert_Survey_2024_ENG_July 1, 2024_03.52.tsv"), TRUE)
D25 <- prep(import("../../2025/Stats/Data.xlsx", which = 1), TRUE)
D26 <- prep(lt("../Data/IPIE_Expert_Survey_2026_June+8,+2026_08.58.tsv"), FALSE)
W <- list("2024" = D24, "2025" = D25, "2026" = D26)

share_by_year <- function(col, cats) {
  imap_dfr(W, function(d, y) {
    v <- d[[col]]
    bind_cols(tibble(year = as.integer(y)), wilson(sum(v %in% cats, na.rm = TRUE), sum(!is.na(v))))
  })
}

#### Figure 2b. Importance of each feature ####

imp <- c(Q15_3 = "Availability of accurate information", Q15_1 = "Diversity of voices",
         Q15_2 = "Diversity of media ownership", Q15_4 = "Absence of misinformation",
         Q15_5 = "Absence of hateful content", Q15_6 = "Absence of micro-targeted political ads",
         Q15_7 = "Absence of AI-generated content")
imp_tr <- imap_dfr(imp, ~bind_cols(tibble(item = .x), share_by_year(.y, "Absolutely essential")))
write_csv(imp_tr, "trend_importance.csv")
dot_trend(imp_tr, "% rating the feature absolutely essential (95% CI)",
          xmax = 75, file = "fig_trend_importance", height = 6.2)

#### Figure 4b. Generative AI expectations ####

ai <- c(Q21_4 = "Video", Q21_2 = "Images",
        Q21_3 = "Voices", Q21_1 = "Text")
ai_worsen <- c("Greatly worsen", "Somewhat worsen", "Slightly worsen")
ai_tr <- imap_dfr(ai, ~bind_cols(tibble(item = .x), share_by_year(.y, ai_worsen)))
write_csv(ai_tr, "trend_ai_future.csv")
dot_trend(ai_tr, "% expecting the format to worsen the information environment (95% CI)",
          xmin = 40, xmax = 80, file = "fig_trend_ai_future", height = 4.4, width = 9.5)

#### Figure 8b. Barriers, 2025 vs 2026 only ####
# 2024 is deliberately excluded. That wave offered five options; 2025 and 2026 offer
# ten. Respondents selected about the same number either way (2.19 in both 2024 and
# 2025), so every option mechanically loses share when the list doubles, which alone
# produces the apparent 2024-2025 collapse. 2025 and 2026 share an identical option
# set apart from a rewording of the privacy item, so those two waves are comparable
# across all eight substantive barriers.

bar_pat <- list(
  "Funding opportunities"               = "Funding opportunities",
  "Data access"                         = "Data access",
  "Pressure to align with a political agenda" = "Pressures to align current research",
  "Government control of research funding"    = "Government control over research funding",
  "Privacy or data-protection restrictions"   = "Privacy",
  "Government censorship"               = "Government censorship",
  "Government surveillance"             = "Government surveillance or monitoring",
  "Pressures from private companies"    = "Pressures from private companies")
bar_tr <- imap_dfr(W[c("2025", "2026")], function(d, y)
  imap_dfr(bar_pat, function(pat, lab)
    bind_cols(tibble(year = as.integer(y), item = lab),
              wilson(sum(grepl(pat, d$Q47, fixed = TRUE), na.rm = TRUE), nrow(d)))))
write_csv(bar_tr, "trend_barriers.csv")
dot_trend(bar_tr, "% of experts reporting the barrier (95% CI)",
          xmax = 80, file = "fig_trend_barriers", height = 6.0)

cat("\n--- Importance ---\n");    print(imp_tr %>% mutate(across(where(is.numeric), ~round(.x, 1))), n = Inf)
cat("\n--- Generative AI ---\n"); print(ai_tr  %>% mutate(across(where(is.numeric), ~round(.x, 1))), n = Inf)
cat("\n--- Barriers ---\n");      print(bar_tr %>% mutate(across(where(is.numeric), ~round(.x, 1))), n = Inf)
cat("\nTrend figures written to", OUT, "\n")
