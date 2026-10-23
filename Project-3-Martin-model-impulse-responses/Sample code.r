# ECF 5421, sample code to help with assignment 3 | Alexander Haas, 5 October 2026
# Impulse responses in the MARTIN model. The worked example shocks the cash rate (NCR);
# set `shockEq` and `shockSize` at the top to your own assigned shock.

# The first time you use a package in R, you need to install it with install.packages()

# Load necessary libraries
library(bimets) # For loading, estimating and simulating the MARTIN model
library(ggplot2) # For plotting
library(zoo) # For working with time series data (year-quarter dates)

# vignette("bimets") # Opens the package documentation for bimets

# Clear the workspace
rm(list = ls()) # Remove all objects from the workspace

# Set the working directory
# setwd("C:\Users\Jacob\OneDrive\AAMonash Economics\A_ECF5421 - Applied macroeconomics - S2 2026\Martin model projects") or create a project  # <- point this at the Assignment 3 folder

# (1) Estimate the whole model and look up your equation  [assignment step 1]

shockEq <- "NCR" # the equation whose residual you shock (your assigned shock)
shockSize <- 0.25 # in the units of that equation's residual (NCR: 0.25 = 25bp)
extraVars <- c("Y", "RC") # step 4: your two additional variables
SIM <- c(2010, 1, 2018, 2) # shock in 2010Q1; do not simulate past 2018Q2

load("Project-3-Martin-model-impulse-responses/MARTINDATA_A2.RData") # the same file as in Assignment 2

# "Estimating the whole model" is Assignment 2 done for every equation, one after another.
# LE is estimated by two-stage least squares, so it gets a second pass with estTech = "IV".
# For the updated vintage, every TSRANGE end moves from 2018 2 to 2026 1 (as in A2, step 4).
estimate_martin <- function(data, end = NULL) {
  txt <- readLines(
    "Project-3-Martin-model-impulse-responses/MARTINMODEL.txt",
    warn = FALSE
  )
  if (!is.null(end)) {
    txt <- sub("^(TSRANGE .*) 2018 2\\s*$", paste("\\1", end), txt)
  }
  m <- LOAD_MODEL(modelText = paste(txt, collapse = "\n"), quietly = TRUE)
  m <- LOAD_MODEL_DATA(m, as.list(data), showWarnings = FALSE, quietly = TRUE)
  m <- ESTIMATE(m, eqList = setdiff(names(m$behaviorals), "LE"), quietly = TRUE)
  ESTIMATE(m, eqList = "LE", estTech = "IV", quietly = TRUE)
}

# (One "NaNs produced" warning per estimation is expected: TDLLHPP is constant by design.)
modelOrig <- estimate_martin(MARTINDATA) # original data, to 2018Q2
modelUpd <- estimate_martin(MARTINDATA_UPDATED, "2026 1") # updated data, to 2026Q1

# Your equation as written in MARTINMODEL.txt; its estimates are in
# modelOrig$behaviorals[[shockEq]] and modelUpd$behaviorals[[shockEq]].
# Print your equation's block from MARTINMODEL.txt (from BEHAVIORAL> to the next blank line)
txt <- readLines(
  "Project-3-Martin-model-impulse-responses/MARTINMODEL.txt",
  warn = FALSE
)
at <- which(txt == paste0("BEHAVIORAL> ", shockEq))
cat(txt[at:(at + which(txt[-(1:at)] == "")[1] - 1)], sep = "\n")


# (2) Compute the impulse responses  [assignment step 2]

# Baseline: add the estimation residuals back as constant adjustments, so the simulation
# reproduces history. Shock: the same, with one residual raised by shockSize in 2010Q1.
# The impulse response is the difference.
# simConvergence: each quarter SIMULATE solves the model by iterating until the variables
# change by less than this percentage. The default (0.01%) leaves numerical errors about as
# large as the response to a small shock, so the IRFs come out noisy; 1e-7 removes that.
simulate_martin <- function(model, adj, exo = NULL) {
  SIMULATE(
    model,
    TSRANGE = SIM,
    ConstantAdjustment = adj,
    Exogenize = exo,
    simConvergence = 1e-7,
    simIterLimit = 1000,
    quietly = TRUE
  )$simulation
}

# Quarterly trimmed-mean inflation: % change in PTM (not annualised)
inflation <- function(ptm, model) {
  p <- c(
    as.numeric(window(
      model$modelData$PTM,
      start = c(2009, 4),
      end = c(2009, 4)
    )),
    as.numeric(ptm)
  )
  100 * (p[-1] / p[-length(p)] - 1)
}

# Rates in percentage points, everything else in % deviation from the baseline
RATES <- c("NCR", "LUR", "N2R", "N10R", "NMR", "RCR", "RMR")

# compute_irfs runs two simulations of the same model -- the baseline (residuals added back,
# so it reproduces history) and the shocked run (plus shockSize in 2010Q1) -- and returns
# shocked minus baseline for each variable, in pp for rates and % for everything else.
compute_irfs <- function(model, vars, label) {
  adj <- lapply(model$behaviorals, `[[`, "residuals")
  base <- simulate_martin(model, adj)
  adj[[shockEq]][[SIM[1], SIM[2]]] <- adj[[shockEq]][[SIM[1], SIM[2]]] +
    shockSize
  shock <- simulate_martin(model, adj)
  dev <- lapply(vars, function(v) {
    if (v == "Inflation") {
      inflation(shock$PTM, model) - inflation(base$PTM, model)
    } else if (v %in% RATES) {
      as.numeric(shock[[v]] - base[[v]])
    } else {
      100 * as.numeric(shock[[v]] / base[[v]] - 1)
    }
  })
  names(dev) <- trimws(paste(
    names(vars),
    vars,
    ifelse(vars %in% c(RATES, "Inflation"), "(pp)", "(%)")
  ))
  out <- do.call(
    rbind,
    lapply(names(dev), function(n) {
      data.frame(
        Quarter = seq_along(dev[[n]]) - 1,
        Deviation = dev[[n]],
        Variable = n,
        Vintage = label
      )
    })
  )
  list(irf = out, base = base, shock = shock)
}

# plot_irfs draws these deviations from baseline: one panel per variable, one line per
# vintage (original vs updated data), quarters after the shock on the horizontal axis.
plot_irfs <- function(d, title) {
  ggplot(d, aes(x = Quarter, y = Deviation, color = Vintage)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_line(linewidth = 1) +
    facet_wrap(
      ~ factor(Variable, levels = unique(Variable)),
      scales = "free_y",
      ncol = 2
    ) +
    labs(
      title = title,
      x = "Quarters after the shock",
      y = "Deviation from baseline"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# (i) your equation's dependent variable, (ii) LUR, (iii) inflation, (iv) NCR -- as in the task.
# In this example the dependent variable is NCR itself, so panels (i) and (iv) are the same series.
mainVars <- c(
  "(i)" = shockEq,
  "(ii)" = "LUR",
  "(iii)" = "Inflation",
  "(iv)" = "NCR"
)
runOrig <- compute_irfs(modelOrig, mainVars, "Original data, to 2018Q2")
runUpd <- compute_irfs(modelUpd, mainVars, "Updated data, to 2026Q1")
irfs <- rbind(runOrig$irf, runUpd$irf)

p_main <- plot_irfs(
  irfs,
  paste0(
    "Impulse responses to a ",
    shockSize,
    " shock to ",
    shockEq,
    " in 2010Q1"
  )
)
print(p_main)
ggsave(
  paste0("IRF_", shockEq, "_main.png"),
  plot = p_main,
  width = 9,
  height = 7,
  dpi = 300
)


# ------------------------------------------------------------------------------
# Side note | check against the RBA: its own monetary policy experiment (Figure 12 of the MARTIN paper)
# holds the cash rate 1pp above baseline for four quarters, with RSTAR, GC and GI held at baseline ("exogenised").
# The *_RBA columns are the RBA's published values (rdp-2019-07-graph-data.xlsx, Figure 12).
base <- runOrig$base
fig12 <- modelOrig
for (k in 1:4) {
  fig12$modelData$NCR[[2010, k]] <- fig12$modelData$NCR[[2010, k]] + 1
}
shockRBA <- simulate_martin(
  fig12,
  lapply(modelOrig$behaviorals, `[[`, "residuals"),
  list(NCR = c(2010, 1, 2010, 4), RSTAR = TRUE, GC = TRUE, GI = TRUE)
)
check12 <- data.frame(
  Quarter = 0:12,
  GDP = round(100 * as.numeric(shockRBA$Y / base$Y - 1), 2)[1:13],
  GDP_RBA = c(
    -0.00,
    -0.18,
    -0.35,
    -0.53,
    -0.72,
    -0.80,
    -0.81,
    -0.76,
    -0.67,
    -0.57,
    -0.47,
    -0.38,
    -0.30
  ),
  LUR = round(as.numeric(shockRBA$LUR - base$LUR), 2)[1:13],
  LUR_RBA = c(
    0.00,
    0.02,
    0.06,
    0.12,
    0.19,
    0.25,
    0.28,
    0.29,
    0.28,
    0.24,
    0.20,
    0.15,
    0.11
  ),
  NCR = round(as.numeric(shockRBA$NCR - base$NCR), 2)[1:13],
  NCR_RBA = c(
    1,
    1,
    1,
    1,
    0.44,
    0.00,
    -0.32,
    -0.50,
    -0.58,
    -0.57,
    -0.51,
    -0.43,
    -0.34
  )
)
print(check12)

# Close but not identical. The RBA started from a forecast (2019Q1 onwards) rather than history,
# but the starting point hardly matters because MARTIN is close to linear. The gap comes mainly
# from the RBA's internal data and the few RBA equations missing from the open-source version.
# ------------------------------------------------------------------------------

# (3) Interpret the responses  [assignment step 3]

# Both vintages use the same shock and simulation window; only the estimated coefficients
# differ. If the responses change, ask which equations' coefficients moved, and why.

# (4) Two additional variables  [assignment step 4]

# The same two simulations, now for the two variables in extraVars (set at the top). In this NCR
# example these could be real GDP (Y) and real household consumption (RC): the cash rate affects the
# economy mainly through spending. Choose two that your own shock should move, and say why.
extra <- rbind(
  compute_irfs(modelOrig, extraVars, "Original data, to 2018Q2")$irf,
  compute_irfs(modelUpd, extraVars, "Updated data, to 2026Q1")$irf
)
p_extra <- plot_irfs(extra, paste0("Additional variables | shock to ", shockEq))
print(p_extra)
ggsave(
  paste0("IRF_", shockEq, "_extra.png"),
  plot = p_extra,
  width = 9,
  height = 4,
  dpi = 300
)


# (5) Monetary policy  [assignment step 5]

# Start from your step-2 responses: which way does your shock push inflation and unemployment?
# What would a central bank want to do about it? Argue it, then also check below if interested.

# ------------------------------------------------------------------------------
# Optional | a counterfactual: what would your shock do if the central bank did not react?
# Rerun it with the cash rate held at its baseline path; the gap to your step-2 response is
# what MARTIN's policy rule does. (Needs a shock other than NCR.)
if (shockEq != "NCR") {
  adj <- lapply(modelOrig$behaviorals, `[[`, "residuals")
  adj[[shockEq]][[SIM[1], SIM[2]]] <- adj[[shockEq]][[SIM[1], SIM[2]]] +
    shockSize
  base <- runOrig$base
  fixedNCR <- simulate_martin(modelOrig, adj, list(NCR = TRUE))
  flexNCR <- runOrig$shock
  n <- length(base$LUR)
  policy <- data.frame(
    Quarter = rep(0:(n - 1), 4),
    Deviation = c(
      as.numeric(flexNCR$LUR - base$LUR),
      as.numeric(fixedNCR$LUR - base$LUR),
      inflation(flexNCR$PTM, modelOrig) - inflation(base$PTM, modelOrig),
      inflation(fixedNCR$PTM, modelOrig) - inflation(base$PTM, modelOrig)
    ),
    Variable = rep(c("LUR (pp)", "Inflation (pp)"), each = 2 * n),
    Vintage = rep(
      rep(c("Cash rate follows its rule", "Cash rate held fixed"), each = n),
      2
    )
  )
  print(
    plot_irfs(policy, paste0("Shock to ", shockEq, ", original data")) +
      labs(color = NULL) +
      scale_color_manual(
        values = c(
          "Cash rate follows its rule" = "darkgreen",
          "Cash rate held fixed" = "darkorange"
        )
      )
  )
}

# Reading the plot: "follows its rule" is your impulse response from step 2; "held fixed" is
# the same shock with no monetary policy reaction. The gap between the two lines is what the
# cash-rate rule does. If the rule pulls both unemployment and inflation back towards zero,
# policy stabilises this shock at no cost. If it can only dampen one by worsening the other
# (inflation and unemployment moving in the same direction), the shock poses a trade-off --
# a central bank then has to decide which to prioritise ... (demand vs supply shocks)
# -----------------------------------------------------------------------------------------
