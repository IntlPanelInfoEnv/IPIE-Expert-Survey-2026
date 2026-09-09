# ============================================================================
# IPIE 2026 Expert Survey — inferential support for the reviewed draft
# Adds confidence intervals and tests for every group difference and every
# year-over-year change narrated in the report. Output: review_stats.csv
# (full appendix table) + review_trend.csv (series for the trend figure).
# ============================================================================
suppressMessages({library(rio); library(tidyverse)})
options(width = 200)

#### Helpers ####

# Wilson score interval — same interval prop.test() reports, without continuity correction
wilson <- function(x, n, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2)
  p <- x / n
  d <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / d
  halfw <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / d
  tibble(pct = 100 * p, lo = 100 * pmax(0, centre - halfw), hi = 100 * pmin(1, centre + halfw), n = n)
}

# Two-proportion test, no continuity correction (matches the Wilson intervals)
prop2 <- function(x1, n1, x2, n2) {
  tt <- suppressWarnings(prop.test(c(x1, x2), c(n1, n2), correct = FALSE))
  tibble(pct1 = 100 * x1 / n1, n1 = n1, pct2 = 100 * x2 / n2, n2 = n2,
         diff = 100 * (x1 / n1 - x2 / n2),
         diff_lo = -100 * tt$conf.int[2], diff_hi = -100 * tt$conf.int[1],
         chisq = unname(tt$statistic), p = tt$p.value)
}

RES <- list()
add <- function(family, claim, tab) RES[[length(RES) + 1]] <<- bind_cols(tibble(family = family, claim = claim), tab)

#### Data ####

developed_countries <- c("Australia","Austria","Belgium","Canada","Czech Republic","Denmark",
  "France","Germany","Greece","Ireland","Israel","Italy","Japan","Netherlands","New Zealand",
  "Norway","Portugal","Singapore","Slovakia","South Korea","Spain","Sweden","Switzerland","UK",
  "USA","Croatia","San Marino","Andorra","Latvia","Lithuania","Estonia","Malta","Liechtenstein",
  "Monaco","Slovenia","Cyprus","Taiwan","Finland","Iceland","Luxembourg")

clean_country <- function(x) {
  x <- gsub("Ireland \\{Republic\\}", "Ireland", x)
  x[x == "Korea South"] <- "South Korea"; x[x == "United Kingdom"] <- "UK"
  x[x == "United States"] <- "USA"; x[x == "Russian Federation"] <- "Russia"; x
}

load_tsv <- function(path) {
  d <- read_tsv(path, locale = locale(encoding = "UTF-16LE"), show_col_types = FALSE)
  d[-c(1, 2), ]
}

d26 <- load_tsv("../Data/IPIE_Expert_Survey_2026_June+8,+2026_08.58.tsv")
d24 <- load_tsv("../../2024/Stats/Data/Expert_Survey_2024_ENG_July 1, 2024_03.52.tsv")
d25 <- import("../../2025/Stats/Data.xlsx", which = 1)

prep <- function(d, year, drop_nonres) {
  d <- d %>% mutate(Progress = as.numeric(Progress)) %>% filter(Progress > 90) %>%
    mutate(across(where(is.character), trimws))
  if (drop_nonres) d <- d %>% filter(Q2 != "I’m not a researcher")
  d %>% mutate(year = year, cc = clean_country(Q9),
               Developed = ifelse(cc %in% developed_countries, "Developed", "Developing"))
}

D <- prep(d26, "2026", FALSE)
cat("2026 valid N:", nrow(D), "\n")
D$Discipline3 <- case_when(D$Q12 == "Social Sciences" ~ "Social Sciences",
                           D$Q12 == "Science, Technology, Engineering, and Mathematics" ~ "STEM",
                           D$Q12 == "Humanities" ~ "Humanities", TRUE ~ NA_character_)
D$US <- ifelse(D$cc == "USA", "US", "Rest")

#### Item batteries and top-box definitions ####

items <- list(
  Q15 = c(Q15_1 = "Diversity of voices", Q15_2 = "Diversity of media ownership",
          Q15_3 = "Availability of accurate information", Q15_4 = "Absence of misinformation",
          Q15_5 = "Absence of hateful content", Q15_6 = "Absence of microtargeted political ads",
          Q15_7 = "Absence of AI-generated content"),
  Q36 = c(Q36_1 = "Polarization", Q36_2 = "Generative AI", Q36_3 = "Mis/disinformation",
          Q36_4 = "Dominance of a few social media platforms",
          Q36_5 = "Lack of platform accountability", Q36_6 = "Filter bubbles & echo chambers"),
  Q43 = c(Q43_1 = "Speed of finding relevant answers", Q43_2 = "Accuracy of information encountered",
          Q43_3 = "Diversity of viewpoints encountered", Q43_4 = "Accuracy of conclusions drawn",
          Q43_5 = "Number of visits to news websites"),
  Q46 = c(Q46_1 = "Search engines", Q46_2 = "Recommender systems",
          Q46_3 = "Generative AI", Q46_4 = "Social media"),
  Q21 = c(Q21_1 = "AI-generated text", Q21_2 = "AI-generated images",
          Q21_3 = "AI-generated voices", Q21_4 = "AI-generated videos")
)

topbox <- list(
  Q15 = "Absolutely essential",
  Q36 = c("Big threat", "An extreme threat"),
  Q43 = c("Improve a little", "Improve", "Improve a lot"),
  Q46 = c("Slightly positive", "Positive", "Very positive"),
  Q21 = c("Slightly worsen", "Somewhat worsen", "Greatly worsen")
)

# Denominator: valid responses to the item (DK counted as valid category where it exists,
# matching the report, which reports shares of all experts answering the battery).
hit <- function(col, cats) D[[col]] %in% cats
nvalid <- function(col) sum(!is.na(D[[col]]))

#### 1. Headline proportions with CIs ####

cat("\n\n########## 1. HEADLINE PROPORTIONS (Wilson 95% CI) ##########\n")
for (fam in names(items)) {
  cat("\n---", fam, "---\n")
  out <- imap_dfr(items[[fam]], function(lab, col) {
    n <- nvalid(col); x <- sum(hit(col, topbox[[fam]]), na.rm = TRUE)
    bind_cols(tibble(item = lab), wilson(x, n))
  })
  print(out %>% arrange(desc(pct)), n = Inf)
  out %>% rowwise() %>% do(add(paste0(fam, " overall"), .$item, tibble(pct = .$pct, lo = .$lo, hi = .$hi, n = .$n))) %>% invisible()
}

# Q25 outlook
worsen <- c("Very confident it will worsen", "Moderately confident it will worsen")
improve <- c("Very confident it will improve", "Moderately confident it will improve")
neither <- "Believe that it will neither improve nor worsen"
cat("\n--- Q25 outlook (denominator = all 470, incl. DK) ---\n")
q25 <- bind_rows(
  bind_cols(tibble(cat = "Worsen"), wilson(sum(D$Q25 %in% worsen), nrow(D))),
  bind_cols(tibble(cat = "Neither"), wilson(sum(D$Q25 == neither), nrow(D))),
  bind_cols(tibble(cat = "Improve"), wilson(sum(D$Q25 %in% improve), nrow(D))),
  bind_cols(tibble(cat = "DK/PNA"), wilson(sum(D$Q25 == "I don't know/Prefer not to say"), nrow(D))))
print(q25)

# Q47 barriers
barrier_opts <- c("Funding opportunities" = "Funding opportunities",
  "Data access" = "Data access",
  "Political pressures from government" = "Pressures to align current research",
  "Government control of funding" = "Government control over research funding",
  "Privacy restrictions" = "Privacy or data-protection restrictions",
  "Government censorship" = "Government censorship",
  "Government surveillance" = "Government surveillance or monitoring",
  "Pressures from private companies" = "Pressures from private companies")
cat("\n--- Q47 barriers (denominator = all 470) ---\n")
bar <- imap_dfr(barrier_opts, function(pat, lab)
  bind_cols(tibble(barrier = lab), wilson(sum(grepl(pat, D$Q47, fixed = TRUE)), nrow(D))))
print(bar %>% arrange(desc(pct)), n = Inf)

# Country-block items used in the report
cb <- list(
  "AI tools for political info (>half)"      = list("Q_4_1", c("Many, more than half of the population.", "The vast majority of the population.")),
  "Trusts main news outlets (>half)"         = list("Q_5_1", c("Many, more than half of the population.", "The vast majority of the population.")),
  "Can find diverse viewpoints (easily)"     = list("Q_6_1", c("Very easy. Online content includes many diverse viewpoints.", "Somewhat easy. A variety of viewpoints are available, though some are difficult to find.")),
  "Confident distinguishing fact (>half)"    = list("Q_8_1", c("Many, more than half of the population", "The vast majority of the population")),
  "Info supports civic understanding"        = list("Q_8_2", c("To a considerable extent", "To a very great extent")))
cat("\n--- Country-block items ---\n")
cbres <- imap_dfr(cb, function(spec, lab)
  bind_cols(tibble(item = lab), wilson(sum(D[[spec[[1]]]] %in% spec[[2]]), nrow(D))))
print(cbres, n = Inf)

#### 2. Group comparisons: developing vs developed ####

cat("\n\n########## 2. DEVELOPING vs DEVELOPED (Holm-corrected within battery) ##########\n")

grp_test <- function(col, cats, grpvar, g1, g2, denom_all = FALSE) {
  s <- D[[grpvar]]
  keep1 <- s == g1; keep2 <- s == g2
  if (!denom_all) { keep1 <- keep1 & !is.na(D[[col]]); keep2 <- keep2 & !is.na(D[[col]]) }
  prop2(sum(D[[col]][keep1] %in% cats), sum(keep1), sum(D[[col]][keep2] %in% cats), sum(keep2))
}

for (fam in names(items)) {
  out <- imap_dfr(items[[fam]], function(lab, col)
    bind_cols(tibble(item = lab), grp_test(col, topbox[[fam]], "Developed", "Developing", "Developed")))
  out$p_holm <- p.adjust(out$p, "holm")
  out$sig <- ifelse(out$p_holm < .05, "*", "")
  cat("\n---", fam, "(Developing - Developed) ---\n")
  print(out %>% select(item, pct1, pct2, diff, diff_lo, diff_hi, p, p_holm, sig) %>%
          mutate(across(where(is.numeric), ~round(.x, 3))), n = Inf)
  add(paste0(fam, " dev/developing"), out$item, out)
}

cat("\n--- Q25 outlook (Developing - Developed) ---\n")
o25 <- prop2(sum(D$Q25[D$Developed == "Developing"] %in% worsen), sum(D$Developed == "Developing"),
             sum(D$Q25[D$Developed == "Developed"] %in% worsen), sum(D$Developed == "Developed"))
print(o25 %>% mutate(across(where(is.numeric), ~round(.x, 3))))

cat("\n--- Q47 barriers (Developing - Developed) ---\n")
bar_g <- imap_dfr(barrier_opts, function(pat, lab) {
  h <- grepl(pat, D$Q47, fixed = TRUE)
  bind_cols(tibble(barrier = lab),
            prop2(sum(h & D$Developed == "Developing"), sum(D$Developed == "Developing"),
                  sum(h & D$Developed == "Developed"), sum(D$Developed == "Developed")))
})
bar_g$p_holm <- p.adjust(bar_g$p, "holm"); bar_g$sig <- ifelse(bar_g$p_holm < .05, "*", "")
print(bar_g %>% select(barrier, pct1, pct2, diff, diff_lo, diff_hi, p, p_holm, sig) %>%
        mutate(across(where(is.numeric), ~round(.x, 3))), n = Inf)

cat("\n--- Country-block items (Developing - Developed) ---\n")
cb_g <- imap_dfr(cb, function(spec, lab)
  bind_cols(tibble(item = lab), grp_test(spec[[1]], spec[[2]], "Developed", "Developing", "Developed", denom_all = TRUE)))
cb_g$p_holm <- p.adjust(cb_g$p, "holm"); cb_g$sig <- ifelse(cb_g$p_holm < .05, "*", "")
print(cb_g %>% select(item, pct1, pct2, diff, diff_lo, diff_hi, p, p_holm, sig) %>%
        mutate(across(where(is.numeric), ~round(.x, 3))), n = Inf)

#### 3. US vs rest, and US vs other developed ####

cat("\n\n########## 3. US COMPARISONS ##########\n")
cat("US n =", sum(D$US == "US"), "\n")

cat("\n--- Q47 barriers (US - Rest) ---\n")
bar_us <- imap_dfr(barrier_opts, function(pat, lab) {
  h <- grepl(pat, D$Q47, fixed = TRUE)
  bind_cols(tibble(barrier = lab),
            prop2(sum(h & D$US == "US"), sum(D$US == "US"), sum(h & D$US == "Rest"), sum(D$US == "Rest")))
})
bar_us$p_holm <- p.adjust(bar_us$p, "holm"); bar_us$sig <- ifelse(bar_us$p_holm < .05, "*", "")
print(bar_us %>% select(barrier, pct1, pct2, diff, diff_lo, diff_hi, p, p_holm, sig) %>%
        mutate(across(where(is.numeric), ~round(.x, 3))), n = Inf)

cat("\n--- Q25 outlook: US vs other developed economies ---\n")
us_dev <- D %>% filter(Developed == "Developed")
o_us <- prop2(sum(us_dev$Q25[us_dev$US == "US"] %in% worsen), sum(us_dev$US == "US"),
              sum(us_dev$Q25[us_dev$US == "Rest"] %in% worsen), sum(us_dev$US == "Rest"))
print(o_us %>% mutate(across(where(is.numeric), ~round(.x, 3))))

cat("\n--- Q36 threats: US vs rest (does the US really rate most threats higher?) ---\n")
thr_us <- imap_dfr(items$Q36, function(lab, col)
  bind_cols(tibble(item = lab), grp_test(col, topbox$Q36, "US", "US", "Rest")))
thr_us$p_holm <- p.adjust(thr_us$p, "holm"); thr_us$sig <- ifelse(thr_us$p_holm < .05, "*", "")
print(thr_us %>% select(item, pct1, pct2, diff, diff_lo, diff_hi, p, p_holm, sig) %>%
        mutate(across(where(is.numeric), ~round(.x, 3))), n = Inf)

cat("\n--- Political-agenda pressure: developed excluding US vs developing ---\n")
h <- grepl("Pressures to align current research", D$Q47, fixed = TRUE)
dev_nous <- D$Developed == "Developed" & D$US == "Rest"
devg <- D$Developed == "Developing"
print(prop2(sum(h & dev_nous), sum(dev_nous), sum(h & devg), sum(devg)) %>%
        mutate(across(where(is.numeric), ~round(.x, 3))))

#### 4. Discipline differences ####

cat("\n\n########## 4. DISCIPLINE (Social Sciences vs STEM vs Humanities) ##########\n")
disc_test <- function(col, cats) {
  sub <- D %>% filter(!is.na(Discipline3), !is.na(.data[[col]]))
  tab <- table(sub$Discipline3, sub[[col]] %in% cats)
  ct <- suppressWarnings(chisq.test(tab))
  props <- sub %>% group_by(Discipline3) %>%
    summarise(pct = 100 * mean(.data[[col]] %in% cats), n = n(), .groups = "drop")
  list(props = props, p = ct$p.value, chisq = unname(ct$statistic))
}
for (spec in list(list("Q36_2", topbox$Q36, "Q36 Generative AI as threat"),
                  list("Q21_4", topbox$Q21, "Q21 AI video will worsen"),
                  list("Q21_1", topbox$Q21, "Q21 AI text will worsen"))) {
  r <- disc_test(spec[[1]], spec[[2]])
  cat("\n---", spec[[3]], "---\n"); print(r$props)
  cat(sprintf("chi2 = %.2f, p = %.4f\n", r$chisq, r$p))
}

cat("\n--- STEM vs Humanities on AI video (report pools them: is that justified?) ---\n")
sub <- D %>% filter(Discipline3 %in% c("STEM", "Humanities"), !is.na(Q21_4))
print(prop2(sum(sub$Q21_4[sub$Discipline3 == "STEM"] %in% topbox$Q21), sum(sub$Discipline3 == "STEM"),
            sum(sub$Q21_4[sub$Discipline3 == "Humanities"] %in% topbox$Q21), sum(sub$Discipline3 == "Humanities")) %>%
        mutate(across(where(is.numeric), ~round(.x, 3))))

#### 5. Trend 2023-2026 with CIs ####

cat("\n\n########## 5. TREND IN OUTLOOK ##########\n")

# 2023 from the raw export (2023/Expert Survey 2023_June 2, 2023_12.23.csv), following the
# 2023 team's own filter (rows with a country, previews dropped) but without their inner
# merge on the naturalearth shapefile, which silently dropped Hong Kong (5) and Singapore (2).
# Hence n = 296 rather than the published 289; worsening = 157/296 = 53% (published 54%).
# Non-response on the item stays in the denominator, as for the later waves.
d23 <- read_csv("../../2023/Expert Survey 2023_June 2, 2023_12.23.csv", show_col_types = FALSE) %>%
  slice(-c(1, 2)) %>% filter(!is.na(COUNTRY), Status != "Survey Preview")
x23 <- sum(d23$THREATFUTURE %in% worsen); n23 <- nrow(d23)
# 2023 country names harmonised to the cc codes used for the later waves
d23cc <- d23 %>% mutate(cc = recode(COUNTRY, "United States of America" = "USA",
                                    "United Kingdom of Great Britain and Northern Ireland" = "UK"),
                        Q25 = THREATFUTURE)
cat(sprintf("2023 from microdata: %d / %d = %.1f%%\n", x23, n23, 100 * x23 / n23))
D24 <- prep(d24, "2024", TRUE); D25 <- prep(d25, "2025", TRUE)

pw <- function(d) sum(trimws(d$Q25) %in% worsen)
trend <- bind_rows(
  bind_cols(tibble(year = 2023), wilson(x23, n23)),
  bind_cols(tibble(year = 2024), wilson(pw(D24), nrow(D24))),
  bind_cols(tibble(year = 2025), wilson(pw(D25), nrow(D25))),
  bind_cols(tibble(year = 2026), wilson(pw(D), nrow(D))))
cat("\n--- Overall series (Wilson 95% CI) ---\n"); print(trend %>% mutate(across(where(is.numeric), ~round(.x, 2))))

cat("\n--- Year-over-year tests ---\n")
yy <- bind_rows(
  bind_cols(tibble(comp = "2024 vs 2023"), prop2(pw(D24), nrow(D24), x23, n23)),
  bind_cols(tibble(comp = "2025 vs 2024"), prop2(pw(D25), nrow(D25), pw(D24), nrow(D24))),
  bind_cols(tibble(comp = "2026 vs 2025"), prop2(pw(D), nrow(D), pw(D25), nrow(D25))),
  bind_cols(tibble(comp = "2026 vs 2023"), prop2(pw(D), nrow(D), x23, n23)))
print(yy %>% select(comp, pct1, pct2, diff, diff_lo, diff_hi, p) %>%
        mutate(across(where(is.numeric), ~round(.x, 3))))

cat("\n--- Comparability check: 2026 with non-researchers dropped (as 2024/2025 did) ---\n")
D26r <- D %>% filter(Q2 != "I’m not a researcher")
cat(sprintf("2026 all: %.1f%% (n=%d); 2026 researchers only: %.1f%% (n=%d)\n",
            100 * pw(D) / nrow(D), nrow(D), 100 * pw(D26r) / nrow(D26r), nrow(D26r)))

cat("\n--- By economy, by year ---\n")
econ_n <- bind_rows(
  D24 %>% group_by(Developed) %>% summarise(x = sum(trimws(Q25) %in% worsen), n = n(), .groups = "drop") %>% mutate(year = 2024),
  D25 %>% group_by(Developed) %>% summarise(x = sum(trimws(Q25) %in% worsen), n = n(), .groups = "drop") %>% mutate(year = 2025),
  D   %>% group_by(Developed) %>% summarise(x = sum(trimws(Q25) %in% worsen), n = n(), .groups = "drop") %>% mutate(year = 2026))
econ <- bind_cols(econ_n, wilson(econ_n$x, econ_n$n) %>% select(pct, lo, hi))
print(econ %>% select(year, Developed, pct, lo, hi, n) %>% mutate(across(where(is.numeric), ~round(.x, 2))), n = Inf)

cat("\n--- US series by year (Brendan: non-monotonic) ---\n")
usyr <- bind_rows(
  d23cc %>% filter(cc == "USA") %>% summarise(x = sum(trimws(Q25) %in% worsen), n = n()) %>% mutate(year = 2023),
  D24 %>% filter(cc == "USA") %>% summarise(x = sum(trimws(Q25) %in% worsen), n = n()) %>% mutate(year = 2024),
  D25 %>% filter(cc == "USA") %>% summarise(x = sum(trimws(Q25) %in% worsen), n = n()) %>% mutate(year = 2025),
  D   %>% filter(cc == "USA") %>% summarise(x = sum(trimws(Q25) %in% worsen), n = n()) %>% mutate(year = 2026))
usyr <- bind_cols(usyr, wilson(usyr$x, usyr$n) %>% select(pct, lo, hi))
print(usyr %>% select(year, pct, lo, hi, n) %>% mutate(across(where(is.numeric), ~round(.x, 2))))
cat("\n2026 vs 2025 within the US:\n")
u25 <- D25 %>% filter(cc == "USA"); u26 <- D %>% filter(cc == "USA")
print(prop2(pw(u26), nrow(u26), pw(u25), nrow(u25)) %>% mutate(across(where(is.numeric), ~round(.x, 3))))

cat("\n--- Composition-adjusted series (direct standardization to the 2024 economy mix) ---\n")
mix24 <- D24 %>% count(Developed) %>% mutate(w = n / sum(n)) %>% select(Developed, w)
adj <- econ %>% left_join(mix24, by = "Developed") %>% group_by(year) %>%
  summarise(pct_adj = sum(pct * w), pct_raw = 100 * sum(x) / sum(n), .groups = "drop")
print(adj %>% mutate(across(where(is.numeric), ~round(.x, 2))))

cat("\n--- Balanced country panel (countries present in all four waves, n>=5 each) ---\n")
cnt <- bind_rows(d23cc %>% count(cc) %>% mutate(year = 2023),
                 D24 %>% count(cc) %>% mutate(year = 2024),
                 D25 %>% count(cc) %>% mutate(year = 2025),
                 D %>% count(cc) %>% mutate(year = 2026))
common <- cnt %>% filter(n >= 5) %>% count(cc) %>% filter(n == 4) %>% pull(cc)
cat("Countries in the balanced panel:", paste(sort(common), collapse = ", "), "\n")
bal <- bind_rows(d23cc %>% filter(cc %in% common) %>% summarise(x = sum(trimws(Q25) %in% worsen), n = n()) %>% mutate(year = 2023),
                 D24 %>% filter(cc %in% common) %>% summarise(x = sum(trimws(Q25) %in% worsen), n = n()) %>% mutate(year = 2024),
                 D25 %>% filter(cc %in% common) %>% summarise(x = sum(trimws(Q25) %in% worsen), n = n()) %>% mutate(year = 2025),
                 D   %>% filter(cc %in% common) %>% summarise(x = sum(trimws(Q25) %in% worsen), n = n()) %>% mutate(year = 2026))
bal <- bind_cols(bal, wilson(bal$x, bal$n) %>% select(pct, lo, hi))
print(bal %>% select(year, pct, lo, hi, n) %>% mutate(across(where(is.numeric), ~round(.x, 2))))

cat("\n--- Headroom: full distribution of Q25 by year ---\n")
q25dist <- bind_rows(D24 %>% count(Q25 = trimws(Q25)) %>% mutate(year = 2024),
                     D25 %>% count(Q25 = trimws(Q25)) %>% mutate(year = 2025),
                     D   %>% count(Q25 = trimws(Q25)) %>% mutate(year = 2026)) %>%
  group_by(year) %>% mutate(pct = round(100 * n / sum(n), 1)) %>% ungroup() %>%
  select(year, Q25, n, pct) %>% pivot_wider(names_from = year, values_from = c(n, pct))
print(q25dist, n = Inf)

cat("\n--- 'Very confident it will worsen' share by year (the true ceiling measure) ---\n")
vc <- bind_rows(
  bind_cols(tibble(year = 2024), wilson(sum(trimws(D24$Q25) == "Very confident it will worsen"), nrow(D24))),
  bind_cols(tibble(year = 2025), wilson(sum(trimws(D25$Q25) == "Very confident it will worsen"), nrow(D25))),
  bind_cols(tibble(year = 2026), wilson(sum(trimws(D$Q25) == "Very confident it will worsen"), nrow(D))))
print(vc %>% mutate(across(where(is.numeric), ~round(.x, 2))))

#### 6. Other trend claims ####

cat("\n\n########## 6. OTHER TREND CLAIMS ##########\n")
cat("\n--- Importance: 'Absolutely essential' by year, with CIs ---\n")
imp_yr <- bind_rows(
  imap_dfr(items$Q15[c("Q15_1", "Q15_3")], function(lab, col)
    bind_cols(tibble(year = 2024, item = lab), wilson(sum(trimws(D24[[col]]) == "Absolutely essential", na.rm = TRUE), sum(!is.na(D24[[col]]))))),
  imap_dfr(items$Q15[c("Q15_1", "Q15_3")], function(lab, col)
    bind_cols(tibble(year = 2025, item = lab), wilson(sum(trimws(D25[[col]]) == "Absolutely essential", na.rm = TRUE), sum(!is.na(D25[[col]]))))),
  imap_dfr(items$Q15[c("Q15_1", "Q15_3")], function(lab, col)
    bind_cols(tibble(year = 2026, item = lab), wilson(sum(trimws(D[[col]]) == "Absolutely essential", na.rm = TRUE), sum(!is.na(D[[col]]))))))
print(imp_yr %>% arrange(item, year) %>% mutate(across(where(is.numeric), ~round(.x, 2))), n = Inf)

cat("\n--- Barriers: funding vs data access, 2026 (is funding really ahead?) ---\n")
fund <- grepl("Funding opportunities", D$Q47, fixed = TRUE)
dat <- grepl("Data access", D$Q47, fixed = TRUE)
cat(sprintf("Funding %.1f%%, Data access %.1f%%; McNemar p = %.4f\n",
            100 * mean(fund), 100 * mean(dat), mcnemar.test(table(fund, dat))$p.value))

cat("\n--- Pressure and pessimism: are pressured experts more pessimistic? ---\n")
pressured <- grepl("Pressures to align current research|Government control over research funding", D$Q47)
print(prop2(sum(D$Q25[pressured] %in% worsen), sum(pressured),
            sum(D$Q25[!pressured] %in% worsen), sum(!pressured)) %>%
        mutate(across(where(is.numeric), ~round(.x, 3))))

#### 7. LCA reconciliation (the 51% vs 62% contradiction) ####

cat("\n\n########## 7. LCA CLASS-CONDITIONAL PROBABILITIES ##########\n")
suppressMessages(library(poLCA))

recode_ai <- function(x) case_when(x %in% c("Greatly worsen", "Somewhat worsen", "Slightly worsen") ~ 1,
                                   x == "Neither" ~ 2,
                                   x %in% c("Slightly improve", "Somewhat improve", "Greatly improve") ~ 3)
Data_LCA_ai <- D %>% transmute(Q21_1 = recode_ai(Q21_1), Q21_2 = recode_ai(Q21_2),
                               Q21_3 = recode_ai(Q21_3), Q21_4 = recode_ai(Q21_4)) %>%
  drop_na()
cat("LCA N (listwise):", nrow(Data_LCA_ai), "of", nrow(D), "\n")
set.seed(42)
lca_ai <- poLCA(cbind(Q21_1, Q21_2, Q21_3, Q21_4) ~ 1, data = Data_LCA_ai, nclass = 3, nrep = 20, verbose = FALSE)
cat("\nClass shares (%):", paste(round(lca_ai$P * 100), collapse = " / "), "\n")
cat("\nP(worsen) within each class, by format:\n")
probs <- sapply(lca_ai$probs, function(m) round(m[, 1] * 100))
rownames(probs) <- paste("Class", seq_len(nrow(probs)))
colnames(probs) <- items$Q21
print(probs)

cat("\nMarginal % expecting worsening, by format (for comparison):\n")
print(round(sapply(names(items$Q21), function(col) 100 * mean(D[[col]] %in% topbox$Q21, na.rm = TRUE))))

cat("\nObserved: among members of the largest class, actual % saying 'worsen' per format:\n")
Data_LCA_ai$Class <- lca_ai$predclass
print(Data_LCA_ai %>% group_by(Class) %>%
        summarise(across(Q21_1:Q21_4, ~round(100 * mean(.x == 1))), n = n()) %>%
        rename(!!!setNames(names(items$Q21), items$Q21)))

#### Write out ####

allres <- bind_rows(RES)
write_csv(allres, "review_stats.csv")
write_csv(trend, "review_trend.csv")
write_csv(econ %>% dplyr::select(year, Developed, pct, lo, hi, n), "review_trend_economy.csv")
write_csv(usyr %>% dplyr::select(year, pct, lo, hi, n), "review_trend_us.csv")
write_csv(bal %>% dplyr::select(year, pct, lo, hi, n), "review_trend_balanced.csv")
write_csv(adj, "review_trend_adjusted.csv")
cat("\n\nWrote review_stats.csv, review_trend.csv, review_trend_economy.csv, review_trend_us.csv\n")
writeLines(capture.output(sessionInfo()), "sessionInfo_review.txt")
