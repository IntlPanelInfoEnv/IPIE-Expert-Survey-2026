# =============================================================================
# Expert typologies: class selection for the two latent class analyses
#
# The Findings describe three groups of experts on threats (52 / 37 / 11) and
# three camps on generative AI (62 / 27 / 11). Both models are fitted in
# Script.R; this script is the evidence for retaining three classes rather than
# two, which the Methods reports as a bootstrapped likelihood-ratio test.
#
# The report quotes poLCA's estimated class proportions ($P), not the share of
# respondents assigned to each class by modal posterior probability. The two
# differ (52/37/11 against 55/33/12 for threats), so the distinction matters
# when comparing this output to the text. Who falls in each class is in Script.R.
#
# Input : Data.xlsx        Output: sessionInfo_lca.txt
# =============================================================================

suppressMessages({library(rio); library(tidyverse); library(poLCA)})
set.seed(42)
B <- 500   # bootstrap replicates for the likelihood-ratio test; the script takes ~8 min

D <- import("Data.xlsx") %>%
  filter(as.numeric(Progress) > 90) %>%
  mutate(across(where(is.character), trimws))

recode_threat <- function(x) {
  case_when(x %in% c("Not a threat at all", "Very small threat", "Small threat") ~ 1,
            x == "Moderate threat" ~ 2,
            x %in% c("Big threat", "An extreme threat") ~ 3)
}
recode_ai <- function(x) {
  case_when(x %in% c("Greatly worsen", "Somewhat worsen", "Slightly worsen") ~ 1,
            x == "Neither" ~ 2,
            x %in% c("Slightly improve", "Somewhat improve", "Greatly improve") ~ 3)
}

# Normalised entropy, 1 = perfectly separated classes
entropy <- function(m) {
  p <- m$posterior
  1 - (-sum(p * log(pmax(p, 1e-12)))) / (nrow(p) * log(ncol(p)))
}

# Bootstrapped likelihood-ratio test of k classes against k - 1.
# Data are simulated from the fitted (k-1)-class model, both models are refitted
# on each replicate, and the observed 2*(logLik_k - logLik_k-1) is compared with
# that null distribution. poLCA has no BLRT of its own.
blrt <- function(f, dat, k, B) {
  m0 <- poLCA(f, dat, nclass = k - 1, nrep = 10, verbose = FALSE)
  m1 <- poLCA(f, dat, nclass = k,     nrep = 10, verbose = FALSE)
  obs <- 2 * (m1$llik - m0$llik)
  null <- vapply(seq_len(B), function(i) {
    s <- poLCA.simdata(N = nrow(dat), probs = m0$probs, P = m0$P)$dat
    names(s) <- all.vars(f)   # simdata returns Y1..Yk, the formula needs the item names
    b0 <- poLCA(f, s, nclass = k - 1, nrep = 5, verbose = FALSE)
    b1 <- poLCA(f, s, nclass = k,     nrep = 5, verbose = FALSE)
    2 * (b1$llik - b0$llik)
  }, numeric(1))
  list(obs = obs, p = (1 + sum(null >= obs)) / (B + 1),
       m0 = m0, m1 = m1, null = null)
}

report <- function(label, f, dat) {
  r <- blrt(f, dat, 3, B)
  cat(sprintf("\n=== %s (n = %d) ===\n", label, nrow(dat)))
  cat(sprintf("2 classes   logLik %9.1f   BIC %8.1f   entropy %.2f\n",
              r$m0$llik, r$m0$bic, entropy(r$m0)))
  cat(sprintf("3 classes   logLik %9.1f   BIC %8.1f   entropy %.2f\n",
              r$m1$llik, r$m1$bic, entropy(r$m1)))
  cat(sprintf("BLRT 3 vs 2: LR = %.1f, p = %.4f (%d bootstrap replicates)\n",
              r$obs, r$p, B))
  cat("shares as reported (estimated class proportions): ",
      paste0(round(sort(r$m1$P, decreasing = TRUE) * 100), "%", collapse = " "), "\n")
  cat("for comparison, modal assignment:                 ",
      paste0(round(sort(prop.table(table(r$m1$predclass)), decreasing = TRUE) * 100),
             "%", collapse = " "), "\n")
  invisible(r)
}

report("Threat typology", cbind(Q36_1, Q36_2, Q36_3, Q36_4, Q36_5, Q36_6) ~ 1,
       D %>% transmute(across(paste0("Q36_", 1:6), recode_threat)) %>% drop_na())

report("Generative-AI typology", cbind(Q21_1, Q21_2, Q21_3, Q21_4) ~ 1,
       D %>% transmute(across(paste0("Q21_", 1:4), recode_ai)) %>% drop_na())

writeLines(capture.output(sessionInfo()), "sessionInfo_lca.txt")
