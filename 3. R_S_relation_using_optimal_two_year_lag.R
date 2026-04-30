setwd("D:/OneDrive/Postdoctor/Project_5_yellow_perch_lake_erie")
#setwd("C:/Users/user/Desktop/OneDrive/Postdoctor/Project_5_yellow_perch_lake_erie")
library(dplyr)
library(TMB)
library(devtools)
library(reshape2)
#library(TMBhelper)
library(rstan)
library(ggplot2)
library(gridExtra)
library(rstan)
library(plotly)
library(reshape2)
library(akima)
library(mgcv)

# read data #
# env data
env_df <-  read.csv("Data/Final/E_scale.csv")

# read the four stock assessment results scale(365-input_df$ice_off, scale = T)
mu1_df = read.table("Env_dependent_model/Base/mu1_23_PR_YOY_HCR_OHTR.rep",header = TRUE, fill = TRUE, comment.char = "#")
mu2_df = read.table("Env_dependent_model/Base/mu2_23_PR_YOY_HCR.rep",header = TRUE, fill = TRUE, comment.char = "#")
mu3_df = read.table("Env_dependent_model/Base/mu3_23_PR_YOY_HCR.rep",header = TRUE, fill = TRUE, comment.char = "#")
mu4_df = read.table("Env_dependent_model/Base/mu4_23_PR_YOY_HCR.rep",header = TRUE, fill = TRUE, comment.char = "#")

# read the recruitment data 
rec_df = readxl::read_xlsx("Data/Great Lakes Datasets for GLFC-YP Project (27Jan2025) (1).xlsx",2) 

#### mu1 ####
mu1_rs_df = data.frame(Year=1975:2023)
mu1_rs_df$ssb = as.numeric(mu1_df$ncount[(which(mu1_df=="spawning_stock_biomass")+1):(which(mu1_df=="N_proj")-1)])

mu1_rs_df = left_join(mu1_rs_df, rec_df, "Year")
mu1_rs_df = left_join(mu1_rs_df,env_df, c("Year"="year"))

rs_model_df = data.frame(
  # Remove the first two recruitment records (Age 2 fish)
  #R = mu1_rs_df$MU1YP2[-(1:2)] * 1000000,
  R = mu1_rs_df$MU1YP2[-(1:2)]* 1000000 ,
  
  # Remove the last two SSB records to align the length
  S = mu1_rs_df$ssb[-( (nrow(mu1_rs_df)-1) : nrow(mu1_rs_df) )]
)

rs_model_df$E_ice_on = mu1_rs_df$ice_on[-( (nrow(mu1_rs_df)-1) : nrow(mu1_rs_df) )]
rs_model_df$E_scale_ice_on = scale(rs_model_df$E_ice_on)[,1]

rs_model_df$E_hypoxia = mu1_rs_df$hypoxia[-( (nrow(mu1_rs_df)-1) : nrow(mu1_rs_df) )]
rs_model_df$E_scale_hypoxia = scale(rs_model_df$E_hypoxia)[,1]

rs_model_df$E_nutrition = mu1_rs_df$c_lold[-( (nrow(mu1_rs_df)-1) : nrow(mu1_rs_df) )]
rs_model_df$E_scale_nutrition = scale(rs_model_df$E_nutrition)[,1]

rs_model_df = rs_model_df[(1:44),]
## 
## 1. Prepare data
R <- rs_model_df$R
S <- rs_model_df$S

E_ice <- rs_model_df$E_scale_ice_on
E_hyp <- rs_model_df$E_scale_hypoxia
E_nut <- rs_model_df$E_scale_nutrition

## 
## 2. Objective functions
# No-environment Beverton-Holt model on log scale
obj_fun0 <- function(par, R, S) {
  log_a <- par[1]
  log_b <- par[2]
  
  pred <- log_a + log(S) - log(1 + exp(log_b) * S)
  
  if (any(!is.finite(pred))) return(1e20)
  
  sum((log(R) - pred)^2)
}

# Environment-dependent Beverton-Holt model on log scale
obj_fun_env <- function(par, R, S, E) {
  log_a <- par[1]
  log_b <- par[2]
  c     <- par[3]
  
  pred <- log_a + c * E + log(S) - log(1 + exp(log_b) * S)
  
  if (any(!is.finite(pred))) return(1e20)
  
  sum((log(R) - pred)^2)
}

## 
## 3. Fit no-environment model

start0 <- c(
  log_a = log(max(R, na.rm = TRUE)),
  log_b = log(1 / max(S, na.rm = TRUE))
)

fit0 <- optim(
  par = start0,
  fn = obj_fun0,
  R = R,
  S = S,
  method = "BFGS",
  hessian = TRUE,
  control = list(maxit = 10000, reltol = 1e-12)
)

# fitted values
pred0_log <- fit0$par["log_a"] + log(S) - log(1 + exp(fit0$par["log_b"]) * S)
pred0_R   <- exp(pred0_log)

# AIC
n <- length(R)
k0 <- 2
rss0 <- fit0$value
AIC0 <- n * log(rss0 / n) + 2 * k0

## 
## 4. Function to fit one environmental model
fit_bh_env <- function(E, R, S, fit0_par, env_name = "env") {
  
  start_env <- c(
    log_a = fit0_par["log_a"],
    log_b = fit0_par["log_b"],
    c = 0
  )
  
  fit <- optim(
    par = start_env,
    fn = obj_fun_env,
    R = R,
    S = S,
    E = E,
    method = "BFGS",
    hessian = TRUE,
    control = list(maxit = 10000, reltol = 1e-12)
  )
  
  # fitted values
  pred_log <- fit$par["log_a"] + fit$par["c"] * E + log(S) - log(1 + exp(fit$par["log_b"]) * S)
  pred_R   <- exp(pred_log)
  
  # AIC
  n <- length(R)
  k <- 3
  rss <- fit$value
  AIC_val <- n * log(rss / n) + 2 * k
  
  # variance-covariance and CI for c
  vcov_mat <- try(solve(fit$hessian), silent = TRUE)
  
  if (inherits(vcov_mat, "try-error")) {
    se_c <- NA
    c_low <- NA
    c_up <- NA
    exp_c_low <- NA
    exp_c_up <- NA
  } else {
    se_c <- sqrt(diag(vcov_mat))["c"]
    c_low <- fit$par["c"] - 1.96 * se_c
    c_up  <- fit$par["c"] + 1.96 * se_c
    exp_c_low <- exp(c_low)
    exp_c_up  <- exp(c_up)
  }
  
  result_row <- data.frame(
    model = env_name,
    convergence = fit$convergence,
    log_a = unname(fit$par["log_a"]),
    log_b = unname(fit$par["log_b"]),
    c = unname(fit$par["c"]),
    c_low_95 = unname(c_low),
    c_up_95 = unname(c_up),
    exp_c = unname(exp(fit$par["c"])),
    exp_c_low_95 = unname(exp_c_low),
    exp_c_up_95 = unname(exp_c_up),
    RSS = rss,
    AIC = AIC_val
  )
  
  list(
    fit = fit,
    result = result_row,
    pred_log = pred_log,
    pred_R = pred_R
  )
}

## 
## 5. Fit all three environmental models
fit_ice <- fit_bh_env(
  E = E_ice,
  R = R,
  S = S,
  fit0_par = fit0$par,
  env_name = "ice_on"
)

fit_hyp <- fit_bh_env(
  E = E_hyp,
  R = R,
  S = S,
  fit0_par = fit0$par,
  env_name = "hypoxia"
)

fit_nut <- fit_bh_env(
  E = E_nut,
  R = R,
  S = S,
  fit0_par = fit0$par,
  env_name = "nutrition"
)


## 
## 6. Combine results
result_table <- rbind(
  data.frame(
    model = "no_env",
    convergence = fit0$convergence,
    log_a = unname(fit0$par["log_a"]),
    log_b = unname(fit0$par["log_b"]),
    c = NA,
    c_low_95 = NA,
    c_up_95 = NA,
    exp_c = NA,
    exp_c_low_95 = NA,
    exp_c_up_95 = NA,
    RSS = rss0,
    AIC = AIC0
  ),
  fit_ice$result,
  fit_hyp$result,
  fit_nut$result
)

print(result_table)


## =========================
## 7. Optional: delta AIC
## =========================

result_table$delta_AIC <- result_table$AIC - min(result_table$AIC)
print(result_table)


## =========================
## 8. Optional: store fitted values back to data frame
## =========================

rs_model_df$pred_no_env_log <- pred0_log
rs_model_df$pred_no_env_R   <- pred0_R

rs_model_df$pred_ice_log <- fit_ice$pred_log
rs_model_df$pred_ice_R   <- fit_ice$pred_R

rs_model_df$pred_hyp_log <- fit_hyp$pred_log
rs_model_df$pred_hyp_R   <- fit_hyp$pred_R

rs_model_df$pred_nut_log <- fit_nut$pred_log
rs_model_df$pred_nut_R   <- fit_nut$pred_R


## =========================
## 9. Optional: only keep c results
## =========================

c_table <- result_table[, c(
  "model", "convergence", "c", "c_low_95", "c_up_95",
  "exp_c", "exp_c_low_95", "exp_c_up_95", "AIC", "delta_AIC"
)]

print(c_table)


#### mu2 ####
mu2_rs_df = data.frame(Year=1975:2023)
mu2_rs_df$ssb = as.numeric(mu2_df$Test[(which(mu2_df=="spawning_stock_biomass")+1):(which(mu2_df=="N_proj")-1)])

mu2_rs_df = left_join(mu2_rs_df, rec_df, "Year")
mu2_rs_df = left_join(mu2_rs_df,env_df, c("Year"="year"))

rs_model_df = data.frame(
  # Remove the first two recruitment records (Age 2 fish)
  R = mu2_rs_df$MU2YP2[-(1:2)]* 1000000 ,
  
  # Remove the last two SSB records to align the length
  S = mu2_rs_df$ssb[-( (nrow(mu2_rs_df)-1) : nrow(mu2_rs_df) )]
)

rs_model_df$E_ice_on  = mu2_rs_df$ice_on[-( (nrow(mu2_rs_df)-1) : nrow(mu2_rs_df) )]
rs_model_df$E_scale_ice_on = scale(rs_model_df$E_ice_on)[,1]

rs_model_df$E_hypoxia = mu2_rs_df$hypoxia[-( (nrow(mu2_rs_df)-1) : nrow(mu2_rs_df) )]
rs_model_df$E_scale_hypoxia = scale(rs_model_df$E_hypoxia)[,1]

rs_model_df$E_nutrition = mu2_rs_df$c_lold[-( (nrow(mu2_rs_df)-1) : nrow(mu2_rs_df) )]
rs_model_df$E_scale_nutrition = scale(rs_model_df$E_nutrition)[,1]


rs_model_df = rs_model_df[(1:44),]

## =========================
## 1. Prepare data
## =========================

R <- rs_model_df$R
S <- rs_model_df$S

E_ice <- rs_model_df$E_scale_ice_on
E_hyp <- rs_model_df$E_scale_hypoxia
E_nut <- rs_model_df$E_scale_nutrition


## =========================
## 2. Objective functions
## =========================

# No-environment Beverton-Holt model on log scale
obj_fun0 <- function(par, R, S) {
  log_a <- par[1]
  log_b <- par[2]
  
  pred <- log_a + log(S) - log(1 + exp(log_b) * S)
  
  if (any(!is.finite(pred))) return(1e20)
  
  sum((log(R) - pred)^2)
}

# Environment-dependent Beverton-Holt model on log scale
obj_fun_env <- function(par, R, S, E) {
  log_a <- par[1]
  log_b <- par[2]
  c     <- par[3]
  
  pred <- log_a + c * E + log(S) - log(1 + exp(log_b) * S)
  
  if (any(!is.finite(pred))) return(1e20)
  
  sum((log(R) - pred)^2)
}


## =========================
## 3. Fit no-environment model
## =========================

start0 <- c(
  log_a = log(max(R, na.rm = TRUE)),
  log_b = log(1 / max(S, na.rm = TRUE))
)

fit0 <- optim(
  par = start0,
  fn = obj_fun0,
  R = R,
  S = S,
  method = "BFGS",
  hessian = TRUE,
  control = list(maxit = 10000, reltol = 1e-12)
)

# fitted values
pred0_log <- fit0$par["log_a"] + log(S) - log(1 + exp(fit0$par["log_b"]) * S)
pred0_R   <- exp(pred0_log)

# AIC
n <- length(R)
k0 <- 2
rss0 <- fit0$value
AIC0 <- n * log(rss0 / n) + 2 * k0


## =========================
## 4. Function to fit one environmental model
## =========================

fit_bh_env <- function(E, R, S, fit0_par, env_name = "env") {
  
  start_env <- c(
    log_a = fit0_par["log_a"],
    log_b = fit0_par["log_b"],
    c = 0
  )
  
  fit <- optim(
    par = start_env,
    fn = obj_fun_env,
    R = R,
    S = S,
    E = E,
    method = "BFGS",
    hessian = TRUE,
    control = list(maxit = 10000, reltol = 1e-12)
  )
  
  # fitted values
  pred_log <- fit$par["log_a"] + fit$par["c"] * E + log(S) - log(1 + exp(fit$par["log_b"]) * S)
  pred_R   <- exp(pred_log)
  
  # AIC
  n <- length(R)
  k <- 3
  rss <- fit$value
  AIC_val <- n * log(rss / n) + 2 * k
  
  # variance-covariance and CI for c
  vcov_mat <- try(solve(fit$hessian), silent = TRUE)
  
  if (inherits(vcov_mat, "try-error")) {
    se_c <- NA
    c_low <- NA
    c_up <- NA
    exp_c_low <- NA
    exp_c_up <- NA
  } else {
    se_c <- sqrt(diag(vcov_mat))["c"]
    c_low <- fit$par["c"] - 1.96 * se_c
    c_up  <- fit$par["c"] + 1.96 * se_c
    exp_c_low <- exp(c_low)
    exp_c_up  <- exp(c_up)
  }
  
  result_row <- data.frame(
    model = env_name,
    convergence = fit$convergence,
    log_a = unname(fit$par["log_a"]),
    log_b = unname(fit$par["log_b"]),
    c = unname(fit$par["c"]),
    c_low_95 = unname(c_low),
    c_up_95 = unname(c_up),
    exp_c = unname(exp(fit$par["c"])),
    exp_c_low_95 = unname(exp_c_low),
    exp_c_up_95 = unname(exp_c_up),
    RSS = rss,
    AIC = AIC_val
  )
  
  list(
    fit = fit,
    result = result_row,
    pred_log = pred_log,
    pred_R = pred_R
  )
}


## =========================
## 5. Fit all three environmental models
## =========================

fit_ice <- fit_bh_env(
  E = E_ice,
  R = R,
  S = S,
  fit0_par = fit0$par,
  env_name = "ice_on"
)

fit_hyp <- fit_bh_env(
  E = E_hyp,
  R = R,
  S = S,
  fit0_par = fit0$par,
  env_name = "hypoxia"
)

fit_nut <- fit_bh_env(
  E = E_nut,
  R = R,
  S = S,
  fit0_par = fit0$par,
  env_name = "nutrition"
)


## =========================
## 6. Combine results
## =========================

result_table <- rbind(
  data.frame(
    model = "no_env",
    convergence = fit0$convergence,
    log_a = unname(fit0$par["log_a"]),
    log_b = unname(fit0$par["log_b"]),
    c = NA,
    c_low_95 = NA,
    c_up_95 = NA,
    exp_c = NA,
    exp_c_low_95 = NA,
    exp_c_up_95 = NA,
    RSS = rss0,
    AIC = AIC0
  ),
  fit_ice$result,
  fit_hyp$result,
  fit_nut$result
)

print(result_table)


## =========================
## 7. Optional: delta AIC
## =========================

result_table$delta_AIC <- result_table$AIC - min(result_table$AIC)
print(result_table)


## =========================
## 8. Optional: store fitted values back to data frame
## =========================

rs_model_df$pred_no_env_log <- pred0_log
rs_model_df$pred_no_env_R   <- pred0_R

rs_model_df$pred_ice_log <- fit_ice$pred_log
rs_model_df$pred_ice_R   <- fit_ice$pred_R

rs_model_df$pred_hyp_log <- fit_hyp$pred_log
rs_model_df$pred_hyp_R   <- fit_hyp$pred_R

rs_model_df$pred_nut_log <- fit_nut$pred_log
rs_model_df$pred_nut_R   <- fit_nut$pred_R


## =========================
## 9. Optional: only keep c results
## =========================

c_table_M2 <- result_table[, c(
  "model", "convergence", "c", "c_low_95", "c_up_95",
  "exp_c", "exp_c_low_95", "exp_c_up_95", "AIC", "delta_AIC"
)]

print(c_table_M2)

#### mu3 ####
mu3_rs_df = data.frame(Year=1975:2023)
mu3_rs_df$ssb = as.numeric(mu3_df$Test[(which(mu3_df=="spawning_stock_biomass")+1):(which(mu3_df=="N_proj")-1)])

mu3_rs_df = left_join(mu3_rs_df, rec_df, "Year")
mu3_rs_df = left_join(mu3_rs_df, env_df, c("Year"="year"))

rs_model_df = data.frame(
  # Remove the first two recruitment records (Age 2 fish)
  R = mu3_rs_df$MU3YP2[-(1:2)]* 1000000 ,
  
  # Remove the last two SSB records to align the length
  S = mu3_rs_df$ssb[-( (nrow(mu3_rs_df)-1) : nrow(mu3_rs_df) )]
)
rs_model_df$E_ice_on  = mu3_rs_df$ice_on[-( (nrow(mu3_rs_df)-1) : nrow(mu3_rs_df) )]
rs_model_df$E_scale_ice_on = scale(rs_model_df$E_ice_on)[,1]

rs_model_df$E_hypoxia = mu3_rs_df$hypoxia[-( (nrow(mu3_rs_df)-1) : nrow(mu3_rs_df) )]
rs_model_df$E_scale_hypoxia = scale(rs_model_df$E_hypoxia)[,1]

rs_model_df$E_nutrition = mu3_rs_df$c_lold[-( (nrow(mu3_rs_df)-1) : nrow(mu3_rs_df) )]
rs_model_df$E_scale_nutrition = scale(rs_model_df$E_nutrition)[,1]


rs_model_df = rs_model_df[(1:44),]

## =========================
## 1. Prepare data
## =========================

R <- rs_model_df$R
S <- rs_model_df$S

E_ice <- rs_model_df$E_scale_ice_on
E_hyp <- rs_model_df$E_scale_hypoxia
E_nut <- rs_model_df$E_scale_nutrition


## =========================
## 2. Objective functions
## =========================

# No-environment Beverton-Holt model on log scale
obj_fun0 <- function(par, R, S) {
  log_a <- par[1]
  log_b <- par[2]
  
  pred <- log_a + log(S) - log(1 + exp(log_b) * S)
  
  if (any(!is.finite(pred))) return(1e20)
  
  sum((log(R) - pred)^2)
}

# Environment-dependent Beverton-Holt model on log scale
obj_fun_env <- function(par, R, S, E) {
  log_a <- par[1]
  log_b <- par[2]
  c     <- par[3]
  
  pred <- log_a + c * E + log(S) - log(1 + exp(log_b) * S)
  
  if (any(!is.finite(pred))) return(1e20)
  
  sum((log(R) - pred)^2)
}


## =========================
## 3. Fit no-environment model
## =========================

start0 <- c(
  log_a = log(max(R, na.rm = TRUE)),
  log_b = log(1 / max(S, na.rm = TRUE))
)

fit0 <- optim(
  par = start0,
  fn = obj_fun0,
  R = R,
  S = S,
  method = "BFGS",
  hessian = TRUE,
  control = list(maxit = 10000, reltol = 1e-12)
)

# fitted values
pred0_log <- fit0$par["log_a"] + log(S) - log(1 + exp(fit0$par["log_b"]) * S)
pred0_R   <- exp(pred0_log)

# AIC
n <- length(R)
k0 <- 2
rss0 <- fit0$value
AIC0 <- n * log(rss0 / n) + 2 * k0


## =========================
## 4. Function to fit one environmental model
## =========================

fit_bh_env <- function(E, R, S, fit0_par, env_name = "env") {
  
  start_env <- c(
    log_a = fit0_par["log_a"],
    log_b = fit0_par["log_b"],
    c = 0
  )
  
  fit <- optim(
    par = start_env,
    fn = obj_fun_env,
    R = R,
    S = S,
    E = E,
    method = "BFGS",
    hessian = TRUE,
    control = list(maxit = 10000, reltol = 1e-12)
  )
  
  # fitted values
  pred_log <- fit$par["log_a"] + fit$par["c"] * E + log(S) - log(1 + exp(fit$par["log_b"]) * S)
  pred_R   <- exp(pred_log)
  
  # AIC
  n <- length(R)
  k <- 3
  rss <- fit$value
  AIC_val <- n * log(rss / n) + 2 * k
  
  # variance-covariance and CI for c
  vcov_mat <- try(solve(fit$hessian), silent = TRUE)
  
  if (inherits(vcov_mat, "try-error")) {
    se_c <- NA
    c_low <- NA
    c_up <- NA
    exp_c_low <- NA
    exp_c_up <- NA
  } else {
    se_c <- sqrt(diag(vcov_mat))["c"]
    c_low <- fit$par["c"] - 1.96 * se_c
    c_up  <- fit$par["c"] + 1.96 * se_c
    exp_c_low <- exp(c_low)
    exp_c_up  <- exp(c_up)
  }
  
  result_row <- data.frame(
    model = env_name,
    convergence = fit$convergence,
    log_a = unname(fit$par["log_a"]),
    log_b = unname(fit$par["log_b"]),
    c = unname(fit$par["c"]),
    c_low_95 = unname(c_low),
    c_up_95 = unname(c_up),
    exp_c = unname(exp(fit$par["c"])),
    exp_c_low_95 = unname(exp_c_low),
    exp_c_up_95 = unname(exp_c_up),
    RSS = rss,
    AIC = AIC_val
  )
  
  list(
    fit = fit,
    result = result_row,
    pred_log = pred_log,
    pred_R = pred_R
  )
}


## =========================
## 5. Fit all three environmental models
## =========================

fit_ice <- fit_bh_env(
  E = E_ice,
  R = R,
  S = S,
  fit0_par = fit0$par,
  env_name = "ice_on"
)

fit_hyp <- fit_bh_env(
  E = E_hyp,
  R = R,
  S = S,
  fit0_par = fit0$par,
  env_name = "hypoxia"
)

fit_nut <- fit_bh_env(
  E = E_nut,
  R = R,
  S = S,
  fit0_par = fit0$par,
  env_name = "nutrition"
)


## =========================
## 6. Combine results
## =========================

result_table <- rbind(
  data.frame(
    model = "no_env",
    convergence = fit0$convergence,
    log_a = unname(fit0$par["log_a"]),
    log_b = unname(fit0$par["log_b"]),
    c = NA,
    c_low_95 = NA,
    c_up_95 = NA,
    exp_c = NA,
    exp_c_low_95 = NA,
    exp_c_up_95 = NA,
    RSS = rss0,
    AIC = AIC0
  ),
  fit_ice$result,
  fit_hyp$result,
  fit_nut$result
)

print(result_table)


## =========================
## 7. Optional: delta AIC
## =========================

result_table$delta_AIC <- result_table$AIC - min(result_table$AIC)
print(result_table)


## =========================
## 8. Optional: store fitted values back to data frame
## =========================

rs_model_df$pred_no_env_log <- pred0_log
rs_model_df$pred_no_env_R   <- pred0_R

rs_model_df$pred_ice_log <- fit_ice$pred_log
rs_model_df$pred_ice_R   <- fit_ice$pred_R

rs_model_df$pred_hyp_log <- fit_hyp$pred_log
rs_model_df$pred_hyp_R   <- fit_hyp$pred_R

rs_model_df$pred_nut_log <- fit_nut$pred_log
rs_model_df$pred_nut_R   <- fit_nut$pred_R


## =========================
## 9. Optional: only keep c results
## =========================

c_table_M3 <- result_table[, c(
  "model", "convergence", "c", "c_low_95", "c_up_95",
  "exp_c", "exp_c_low_95", "exp_c_up_95", "AIC", "delta_AIC"
)]

print(c_table_M3)

#### mu4 ####
mu4_rs_df = data.frame(Year=1975:2023)
mu4_rs_df$ssb = as.numeric(mu4_df$Test[(which(mu4_df=="spawning_stock_biomass")+1):(which(mu4_df=="N_proj")-1)])

mu4_rs_df = left_join(mu4_rs_df, rec_df, "Year")
mu4_rs_df = left_join(mu4_rs_df,env_df, c("Year"="year"))

rs_model_df = data.frame(
  # Remove the first two recruitment records (Age 2 fish)
  R = mu4_rs_df$MU4YP2[-(1:2)]* 1000000 ,
  
  # Remove the last two SSB records to align the length
  S = mu4_rs_df$ssb[-( (nrow(mu4_rs_df)-1) : nrow(mu4_rs_df) )]
)

rs_model_df$E_ice_on  = mu4_rs_df$ice_on[-( (nrow(mu4_rs_df)-1) : nrow(mu4_rs_df) ) ]
rs_model_df$E_scale_ice_on = scale(rs_model_df$E_ice_on)[,1]

rs_model_df$E_hypoxia = mu4_rs_df$hypoxia[-( (nrow(mu4_rs_df)-1) : nrow(mu4_rs_df) )]
rs_model_df$E_scale_hypoxia = scale(rs_model_df$E_hypoxia)[,1]

rs_model_df$E_nutrition = mu4_rs_df$c_lold[-( (nrow(mu4_rs_df)-1) : nrow(mu4_rs_df) )]
rs_model_df$E_scale_nutrition = scale(rs_model_df$E_nutrition)[,1]


rs_model_df = rs_model_df[(1:44),]

## =========================
## 1. Prepare data
## =========================

R <- rs_model_df$R
S <- rs_model_df$S

E_ice <- rs_model_df$E_scale_ice_on
E_hyp <- rs_model_df$E_scale_hypoxia
E_nut <- rs_model_df$E_scale_nutrition


## =========================
## 2. Objective functions
## =========================

# No-environment Beverton-Holt model on log scale
obj_fun0 <- function(par, R, S) {
  log_a <- par[1]
  log_b <- par[2]
  
  pred <- log_a + log(S) - log(1 + exp(log_b) * S)
  
  if (any(!is.finite(pred))) return(1e20)
  
  sum((log(R) - pred)^2)
}

# Environment-dependent Beverton-Holt model on log scale
obj_fun_env <- function(par, R, S, E) {
  log_a <- par[1]
  log_b <- par[2]
  c     <- par[3]
  
  pred <- log_a + c * E + log(S) - log(1 + exp(log_b) * S)
  
  if (any(!is.finite(pred))) return(1e20)
  
  sum((log(R) - pred)^2)
}


## =========================
## 3. Fit no-environment model
## =========================

start0 <- c(
  log_a = log(max(R, na.rm = TRUE)),
  log_b = log(1 / max(S, na.rm = TRUE))
)

fit0 <- optim(
  par = start0,
  fn = obj_fun0,
  R = R,
  S = S,
  method = "BFGS",
  hessian = TRUE,
  control = list(maxit = 10000, reltol = 1e-12)
)

# fitted values
pred0_log <- fit0$par["log_a"] + log(S) - log(1 + exp(fit0$par["log_b"]) * S)
pred0_R   <- exp(pred0_log)

# AIC
n <- length(R)
k0 <- 2
rss0 <- fit0$value
AIC0 <- n * log(rss0 / n) + 2 * k0


## =========================
## 4. Function to fit one environmental model
## =========================

fit_bh_env <- function(E, R, S, fit0_par, env_name = "env") {
  
  start_env <- c(
    log_a = fit0_par["log_a"],
    log_b = fit0_par["log_b"],
    c = 0
  )
  
  fit <- optim(
    par = start_env,
    fn = obj_fun_env,
    R = R,
    S = S,
    E = E,
    method = "BFGS",
    hessian = TRUE,
    control = list(maxit = 10000, reltol = 1e-12)
  )
  
  # fitted values
  pred_log <- fit$par["log_a"] + fit$par["c"] * E + log(S) - log(1 + exp(fit$par["log_b"]) * S)
  pred_R   <- exp(pred_log)
  
  # AIC
  n <- length(R)
  k <- 3
  rss <- fit$value
  AIC_val <- n * log(rss / n) + 2 * k
  
  # variance-covariance and CI for c
  vcov_mat <- try(solve(fit$hessian), silent = TRUE)
  
  if (inherits(vcov_mat, "try-error")) {
    se_c <- NA
    c_low <- NA
    c_up <- NA
    exp_c_low <- NA
    exp_c_up <- NA
  } else {
    se_c <- sqrt(diag(vcov_mat))["c"]
    c_low <- fit$par["c"] - 1.96 * se_c
    c_up  <- fit$par["c"] + 1.96 * se_c
    exp_c_low <- exp(c_low)
    exp_c_up  <- exp(c_up)
  }
  
  result_row <- data.frame(
    model = env_name,
    convergence = fit$convergence,
    log_a = unname(fit$par["log_a"]),
    log_b = unname(fit$par["log_b"]),
    c = unname(fit$par["c"]),
    c_low_95 = unname(c_low),
    c_up_95 = unname(c_up),
    exp_c = unname(exp(fit$par["c"])),
    exp_c_low_95 = unname(exp_c_low),
    exp_c_up_95 = unname(exp_c_up),
    RSS = rss,
    AIC = AIC_val
  )
  
  list(
    fit = fit,
    result = result_row,
    pred_log = pred_log,
    pred_R = pred_R
  )
}


## =========================
## 5. Fit all three environmental models
## =========================

fit_ice <- fit_bh_env(
  E = E_ice,
  R = R,
  S = S,
  fit0_par = fit0$par,
  env_name = "ice_on"
)

fit_hyp <- fit_bh_env(
  E = E_hyp,
  R = R,
  S = S,
  fit0_par = fit0$par,
  env_name = "hypoxia"
)

fit_nut <- fit_bh_env(
  E = E_nut,
  R = R,
  S = S,
  fit0_par = fit0$par,
  env_name = "nutrition"
)


## =========================
## 6. Combine results
## =========================

result_table <- rbind(
  data.frame(
    model = "no_env",
    convergence = fit0$convergence,
    log_a = unname(fit0$par["log_a"]),
    log_b = unname(fit0$par["log_b"]),
    c = NA,
    c_low_95 = NA,
    c_up_95 = NA,
    exp_c = NA,
    exp_c_low_95 = NA,
    exp_c_up_95 = NA,
    RSS = rss0,
    AIC = AIC0
  ),
  fit_ice$result,
  fit_hyp$result,
  fit_nut$result
)

print(result_table)


## =========================
## 7. Optional: delta AIC
## =========================

result_table$delta_AIC <- result_table$AIC - min(result_table$AIC)
print(result_table)


## =========================
## 8. Optional: store fitted values back to data frame
## =========================

rs_model_df$pred_no_env_log <- pred0_log
rs_model_df$pred_no_env_R   <- pred0_R

rs_model_df$pred_ice_log <- fit_ice$pred_log
rs_model_df$pred_ice_R   <- fit_ice$pred_R

rs_model_df$pred_hyp_log <- fit_hyp$pred_log
rs_model_df$pred_hyp_R   <- fit_hyp$pred_R

rs_model_df$pred_nut_log <- fit_nut$pred_log
rs_model_df$pred_nut_R   <- fit_nut$pred_R


## =========================
## 9. Optional: only keep c results
## =========================

c_table_M4 <- result_table[, c(
  "model", "convergence", "c", "c_low_95", "c_up_95",
  "exp_c", "exp_c_low_95", "exp_c_up_95", "AIC", "delta_AIC"
)]

print(c_table_M4)

### figure ####
c_table$mu = 1
c_table_M2$mu = 2
c_table_M3$mu = 3
c_table_M4$mu = 4 

### combine all results
mu_all_rs_result = rbind(c_table,c_table_M2,c_table_M3,c_table_M4)
write.csv(mu_all_rs_result,"Figure/Final/ssb_r_result_two_years_lag.csv",row.names = FALSE)

### plot env impact on r-s relationship 
# Set order and labels
mu_all_rs_result <- read.csv("Figure/Final/ssb_r_result_two_years_lag.csv")
mu_all_rs_result <- mu_all_rs_result[complete.cases(mu_all_rs_result), ]
mu_all_rs_result$model <- factor(
  mu_all_rs_result$model,
  levels = c("ice_on", "nutrition","hypoxia"),
  labels = c("Ice-on duration", "Total phosphorus loads","Hypoxia extent")
)

# Relabel mu panels
mu_all_rs_result$mu <- factor(
  mu_all_rs_result$mu,
  levels = c(1,2,3,4),
  labels = c("MU1","MU2","MU3","MU4")
)

main_figure_df <- mu_all_rs_result[mu_all_rs_result$model%in%c("Ice-on duration","Total phosphorus loads"),]

# Colors
env_colors <- c(
  "Ice-on duration" = "#4C78A8",
  "Total phosphorus loads" = "#B22222"
)

ggplot(main_figure_df,
       aes(x = model, y = c, color = model)) +
  geom_hline(yintercept = 0,
             linetype = "dotted",
             color = "black",
             linewidth = 0.8) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = c_low_95, ymax = c_up_95),
                width = 0.2) +
  facet_wrap(~mu, nrow = 1) +
  scale_color_manual(values = env_colors) +
  theme_bw(base_size = 16) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),      # remove crowded labels
    axis.ticks.x = element_blank()
  ) +
  labs(
    x = "Environmental factors",
    y = "Effect on recruitment",
    color = NULL
  )

ggsave(
  "Figure/Final/environment_effects_R_S_two_years_lag.png",
  width = 7,
  height = 4,
  units = "in",
  dpi = 600
)

sm_figure_df <- mu_all_rs_result[mu_all_rs_result$model%in%c("Hypoxia extent","Total phosphorus loads"),]

# Colors
env_colors <- c(
  "Hypoxia extent" = "#B22222",
  "Total phosphorus loads" = "#B22222"
)
env_shapes <- c(
  "Hypoxia extent" = 17,        # Triangle
  "Total phosphorus loads" = 16 # Circle
)

ggplot(sm_figure_df,
       aes(x = model, y = c, color = model, shape = model)) + # Map shape to model
  geom_hline(yintercept = 0,
             linetype = "dotted",
             color = "black",
             linewidth = 0.8) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = c_low_95, ymax = c_up_95),
                width = 0.2) +
  facet_wrap(~mu, nrow = 1) +
  scale_color_manual(values = env_colors) +
  scale_shape_manual(values = env_shapes) + # Set the manual shapes
  theme_bw(base_size = 16) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    x = "Environmental factors",
    y = "Effect on recruitment",
    color = NULL,
    shape = NULL # Remove legend title for shapes too
  )

ggsave(
  "Figure/Final/sm_environment_effects_R_S_two_years_lag.png",
  width = 7,
  height = 4,
  units = "in",
  dpi = 600
)


