# ECF 5421, sample code to help with assignment 2 | Alexander Haas, 17 August 2026
# Re-estimate one MARTIN equation. The example below is LE (employment);
# set `eq` at the top and paste your own equation from MARTINMODEL.txt.

# The first time you use a package in R, you need to install it with install.packages()

install.packages("bitmets")
# Load necessary libraries
library(bimets) # For loading and estimating the MARTIN model
library(ggplot2) # For plotting data
library(zoo) # For working with time series data (year-quarter dates)

# vignette("bimets") # Opens the package documentation for bimets

# Clear the workspace
rm(list = ls()) # Remove all objects from the workspace

# Set the working directory
# setwd("") or create a project  # <- point this at the Assignment 2 folder

# (1) Load the data and work out what your equation uses  [assignment step 1]

eq <- "NHC" # MARTIN equation name = its dependent variable
appendix <- "(19)" # equation number in the RBA appendix, to compare against
END_ORIG <- "2018 2" # the RBA's own estimation sample ends 2018Q2
END_UPD <- "2026 1" # the updated sample ends 2026Q1

# NOTE the file name: MARTINDATA_A2.RData, not the MARTINDATA.RData from Assignment 1.
# The two are different. A1's file holds only MARTINDATA; this one also holds
# MARTINDATA_UPDATED, every series brought forward to 2026Q1. If you saved your own
# MARTINDATA.RData in Assignment 1, keep it well away from this folder.
# LE is the ONLY equation in MARTIN estimated by two-stage least squares -- it is the
# worked example precisely because it is the awkward one. EVERY equation you could be
# allocated is ordinary least squares. This line reads the right estimator off `eq`, so
# changing eq above is enough; you do not have to remember to change the ESTIMATE calls.
# Leave it as 'IV' on an equation with no IV> lines and bimets stops with
#   ESTIMATE(): please provide instrumental variables "IV".
estTech <- if (eq == "LE") "IV" else "OLS"

load("MARTINDATA_A2.RData") # gives MARTINDATA (to 2019Q3) and MARTINDATA_UPDATED (to 2026Q1)

# Fail loudly rather than silently estimating on the wrong vintage
if (!all(c("MARTINDATA", "MARTINDATA_UPDATED") %in% ls())) {
  stop(
    "Loaded the wrong file -- this looks like Assignment 1's MARTINDATA.RData, ",
    "which has no MARTINDATA_UPDATED. Use MARTINDATA_A2.RData."
  )
}
stopifnot(eq %in% names(MARTINDATA))

# The series your equation needs. Read these off your own equation in MARTINMODEL.txt --
# step 1 asks you to say what each one is and what form it enters in (level, log, difference).
myVars <- c(
  "NHC",
  "PH",
  "KID",
  "NMR",
  "RMR",
  "PI_E"
)

stopifnot(all(myVars %in% names(MARTINDATA))) # a typo here is easy to make
stopifnot(all(myVars %in% names(MARTINDATA_UPDATED)))

# Variables and their definition: LE = employment; Y = output; RLC = real labour cost;
# TLLA = trend labour productivity; TDLLA = trend labour productivity growth (mean reverting);
# TLLHPP = trend average hours worked; TDLLHPP = trend average hours worked growth (downward sloping);
# TDLLPOP = trend population growth; LUR = unemployment rate; D_LE = dummy for 2003Q2.

# Inspect the data: the last 8 quarters of the dependent variable in each vintage
tail(MARTINDATA[[eq]], 8) # original -- stops in 2019Q3
tail(MARTINDATA_UPDATED[[eq]], 8) # updated  -- runs to 2026Q1

# The updated series must actually reach 2026Q1, or step 4 is not doing what it claims
last_obs <- as.yearqtr(time(MARTINDATA_UPDATED[[
  eq
]]))[length(MARTINDATA_UPDATED[[eq]])]
cat("Updated", eq, "ends:", format(last_obs), "\n")
stopifnot(last_obs == as.yearqtr("2026 Q1"))


# (2) Plot every series the equation uses  [assignment step 2]

# MARTIN series do not all begin in the same quarter -- LE starts 1964Q1, RLC 1965Q3,
# TDLLPOP only in 1978Q3 -- so every series is stored with leading NAs. Plotting those
# is what produces "Removed N rows containing missing values (geom_line())". The tidy fix
# is to drop the missing quarters before plotting, which this small helper does.
as_plot_df <- function(s) {
  df <- data.frame(date = as.Date(as.yearqtr(time(s))), Value = as.numeric(s))
  df[!is.na(df$Value), ] # drop the quarters before the series begins
}

# Note the print() -- inside a loop, ggplot objects have to be printed explicitly.
for (v in myVars) {
  df <- as_plot_df(MARTINDATA_UPDATED[[v]])
  cat(v, "starts", format(min(as.yearqtr(df$date))), "\n")
  print(
    ggplot(df, aes(x = date, y = Value)) +
      geom_line(color = "blue") +
      labs(
        title = paste0(v, " -- used in the ", eq, " equation"),
        x = "Date",
        y = v
      ) +
      theme_minimal()
  )
}

# The dependent variable, both vintages on one chart: blue solid = updated,
# red dashed = original. Where they differ, the data was revised or re-referenced.
df_upd <- as_plot_df(MARTINDATA_UPDATED[[eq]])
df_org <- as_plot_df(MARTINDATA[[eq]])

ggplot() +
  geom_line(data = df_upd, aes(x = date, y = Value), color = "blue") +
  geom_line(
    data = df_org,
    aes(x = date, y = Value),
    color = "red",
    linetype = "dashed"
  ) +
  labs(
    title = paste0(eq, " -- updated (blue) vs original (red dashed)"),
    x = "Date",
    y = eq
  ) +
  theme_minimal()


# (3) Estimate on the model's own sample, ending 2018Q2  [assignment step 3]

# The equation, copied VERBATIM from MARTINMODEL.txt. Replace this block with your own:
# keep the TSRANGE, COEFF>, RESTRICT> and IV> lines that belong to your equation.
# (restriction: in the long run a stable employment-to-population ratio)
myModelDefinition <-
  "MODEL

COMMENT> Nominal household credit
BEHAVIORAL> NHC
TSRANGE 1993 1 2018 2
EQ> TSDELTALOG(NHC,1)=c1+c2*(LOG(TSLAG(NHC,1))-LOG(TSLAG(PH,1))-LOG(TSLAG(KID,2)))
            +c3*TSLAG(RMR,1)
            +c4*TSDELTALOG(TSLAG(NHC,1),1)
            +c5*TSDELTALOG(TSLAG(PH,1),1)
            +c6*TSDELTALOG(TSLAG(KID,2),1)
            +c7*TSDELTA(TSLAG(NMR,1),1)
            +c8*(PI_E/400)
COEFF> c1 c2 c3 c4 c5 c6 c7 c8
RESTRICT> c4+c6=1
RESTRICT> c4+c5+c8=1



END"

# The data the equation needs -- ORIGINAL vintage
myModelData <- list(
  NHC = MARTINDATA$NHC,
  PH = MARTINDATA$PH,
  KID = MARTINDATA$KID,
  NMR = MARTINDATA$NMR,
  RMR = MARTINDATA$RMR,
  PI_E = MARTINDATA$PI_E
)

# myModelData <- as.list(MARTINDATA)   # simpler, but then you never state which series you use

myModel <- LOAD_MODEL(modelText = myModelDefinition)
myModel <- LOAD_MODEL_DATA(myModel, myModelData, showWarnings = TRUE)
myModelEstimated <- ESTIMATE(myModel, estTech = estTech) # 'IV' for LE, 'OLS' for every other equation

# EXPECTED, not a problem: "warning, there are non-finite values in time series ..." for
# the six series that begin after 1959Q3. bimets is telling you the series has NAs; it
# estimates over TSRANGE only, so they are never used. Leaving showWarnings = TRUE is
# deliberate -- if a series were missing observations INSIDE your TSRANGE, bimets would
# stop with an error instead, and that one you do need to see.

# Fail loudly if the estimation did not actually produce coefficients
stopifnot(!is.null(myModelEstimated$behaviorals[[eq]]$coefficients))
stopifnot(!any(is.na(myModelEstimated$behaviorals[[eq]]$coefficients)))

# Display the estimation results, then compare them with appendix equation `appendix`
summary(myModelEstimated)

# Plot the residuals
res_org <- myModelEstimated$behaviorals[[eq]]$residuals
df_res <- as_plot_df(res_org)
names(df_res)[2] <- "Residual"

ggplot(df_res, aes(x = date, y = Residual)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_line(color = "blue") +
  labs(
    title = paste0("Residuals | ", eq, ", original data to 2018Q2"),
    x = "Date",
    y = "Residual"
  ) +
  theme_minimal()

# READING YOUR OUTPUT AGAINST THE APPENDIX -- three things that trip people up:
#  * bimets numbers coefficients c1, c2, ... by ORDER OF APPEARANCE, so c1 is not
#    necessarily the appendix's alpha0. Match them term by term, not by position.
#  * the RBA reports HAC (Newey-West) standard errors; ESTIMATE() reports ordinary
#    ones. Coefficients are comparable, standard errors are only roughly so.
#  * if your equation has a *_TREND term sitting OUTSIDE the error-correction bracket,
#    the appendix's long-run coefficient is -c_trend / c_ec, not the trend coefficient
#    itself. Reading it straight off can look like a tenfold error when nothing is wrong.

# (4) Re-estimate on the updated data, extended to 2026Q1  [assignment step 4]

# Same equation, only the TSRANGE end date changes: 2018 2 becomes 2026 1.
myModelDefinition <-
  "MODEL

COMMENT> Nominal household credit
BEHAVIORAL> NHC
TSRANGE 1993 1 2026 1
EQ> TSDELTALOG(NHC,1)=c1+c2*(LOG(TSLAG(NHC,1))-LOG(TSLAG(PH,1))-LOG(TSLAG(KID,2)))
            +c3*TSLAG(RMR,1)
            +c4*TSDELTALOG(TSLAG(NHC,1),1)
            +c5*TSDELTALOG(TSLAG(PH,1),1)
            +c6*TSDELTALOG(TSLAG(KID,2),1)
            +c7*TSDELTA(TSLAG(NMR,1),1)
            +c8*(PI_E/400)
COEFF> c1 c2 c3 c4 c5 c6 c7 c8
RESTRICT> c4+c6=1
RESTRICT> c4+c5+c8=1



END"

# The same series -- UPDATED vintage
myModelData <- list(
  NHC = MARTINDATA_UPDATED$NHC,
  PH = MARTINDATA_UPDATED$PH,
  KID = MARTINDATA_UPDATED$KID,
  NMR = MARTINDATA_UPDATED$NMR,
  RMR = MARTINDATA_UPDATED$RMR,
  PI_E = MARTINDATA_UPDATED$PI_E
)


myModel <- LOAD_MODEL(modelText = myModelDefinition)
myModel <- LOAD_MODEL_DATA(myModel, myModelData, showWarnings = TRUE)
myModelEstimated <- ESTIMATE(myModel, estTech = estTech) # 'IV' for LE, 'OLS' for every other equation

stopifnot(!any(is.na(myModelEstimated$behaviorals[[eq]]$coefficients)))

summary(myModelEstimated)

ser_upd <- myModelEstimated$behaviorals[[eq]]$statistics$StandardErrorRegression
res_upd <- myModelEstimated$behaviorals[[eq]]$residuals
df_res <- as_plot_df(res_upd)
names(df_res)[2] <- "Residual"

ggplot(df_res, aes(x = date, y = Residual)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_line(color = "blue") +
  labs(
    title = paste0("Residuals | ", eq, ", updated data to 2026Q1"),
    x = "Date",
    y = "Residual"
  ) +
  theme_minimal()


# Saved here for KV (Stock of inventries)

# Two things change at once between (3) and (4): the sample is longer AND the data has
# been revised and re-referenced. A large change in the CONSTANT is usually the
# re-referencing, not economics -- compare slopes and the error-correction term.
# "Nothing changed significantly" is a legitimate finding. Say so if that is what you find.

# (5) Propose a change and re-estimate  [assignment step 5]

# Example: add a dummy for 2020Q2, the quartesr of the first lockdown. Your own change
# should follow from what you saw in the residuals above -- argue for it, do not just add terms.
#
# NOTE the extra `IV> D_2020Q2` line at the end of the instrument list. LE is estimated by
# two-stage least squares, and in 2SLS every exogenous regressor is also its own instrument
# (which is why D_LE is already in the list). Add a term to the equation and forget the IV>
# line and the fit gets WORSE instead of better -- a sure sign something is wrong, not a
# result. Equations other than LE have no IV> block, so this does not apply to them.
myModelDefinition <-
  "MODEL

COMMENT> Nominal household credit
BEHAVIORAL> NHC
TSRANGE 1993 1 2026 1
EQ> TSDELTALOG(NHC,1)=c1+c2*(LOG(TSLAG(NHC,1))-LOG(TSLAG(PH,1))-LOG(TSLAG(KID,2)))
            +c3*TSLAG(RMR,1)
            +c4*TSDELTALOG(TSLAG(NHC,1),1)
            +c5*TSDELTALOG(TSLAG(PH,1),1)
            +c6*TSDELTALOG(TSLAG(KID,2),1)
            +c7*TSDELTA(TSLAG(NMR,1),1)
            +c8*(PI_E/400)
COEFF> c1 c2 c3 c4 c5 c6 c7 c8
RESTRICT> c4+c6=1
RESTRICT> c4+c5+c8=1



END"


# This Dummy section is the last graph and has not been done yet

# The dummy is c9 here because LE has eight coefficients. YOUR equation will have a
# different number -- use the one after your own COEFF> list (if yours ends c6, add
# c7), and add it to the COEFF> line as well as to the equation.

# Build the dummy: 1 in 2020Q2, 0 in every other quarter.
D_2020Q2 <- MARTINDATA_UPDATED$Y * 0
window(D_2020Q2, start = c(2020, 1), end = c(2020, 1)) <- 1
stopifnot(sum(D_2020Q2) == 1) # exactly one quarter switched on

myModelData <- list(
  KV = MARTINDATA_UPDATED$LE,
  Y = MARTINDATA_UPDATED$Y,
  TDLLA = MARTINDATA_UPDATED$TDLLA,
  TDLLHPP = MARTINDATA_UPDATED$TDLLHPP,
  TDLLPOP = MARTINDATA_UPDATED$TDLLPOP,
  D_2020Q2 = D_2020Q2
)

myModel <- LOAD_MODEL(modelText = myModelDefinition)
myModel <- LOAD_MODEL_DATA(myModel, myModelData, showWarnings = TRUE)
myModelEstimated <- ESTIMATE(myModel, estTech = estTech) # 'IV' for LE, 'OLS' for every other equation

stopifnot(!any(is.na(myModelEstimated$behaviorals[[eq]]$coefficients)))

summary(myModelEstimated)

ser_new <- myModelEstimated$behaviorals[[eq]]$statistics$StandardErrorRegression
res_new <- myModelEstimated$behaviorals[[eq]]$residuals
df_res <- as_plot_df(res_new)
names(df_res)[2] <- "Residual"

ggplot(df_res, aes(x = date, y = Residual)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_line(color = "blue") +
  labs(
    title = paste0(
      "Residuals | ",
      eq,
      ", updated data to 2026Q1 + 2020Q2 dummy"
    ),
    x = "Date",
    y = "Residual"
  ) +
  theme_minimal()

# Did it help? Compare the standard error of the regression in (4) and (5), and say
# whether the improvement is worth the extra parameter.
# Take SER from the model itself -- sd(residuals) is NOT the same thing (it divides by
# n-1, the SER by n-k) and would not match the figure summary() prints above.
cat("SER without the dummy:", ser_upd, "\n")
cat("SER with the dummy   :", ser_new, "\n")
cat(
  "change in SER        :",
  sprintf("%+.2f%%", 100 * (ser_new / ser_upd - 1)),
  "\n"
)
