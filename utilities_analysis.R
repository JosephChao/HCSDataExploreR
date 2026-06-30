#### Definitions of function inputs ####
# DAT = dataframe eg. data_merged()
# DS$compare = input$compare
# DS$reference = input$reference
# CTRL = ctrl_data_selected() 
# CSD = ctrl_sd_data_selected()
# UDC = uninf_def_ctrl_data_selected()


#### % Cell Survival ####
X_Cell_Survival_fn <- function(DAT,DS,CTRL,CSD,UDC) {
  dat <- DAT[which(!is.na(DAT$Sample)), ]

  req(DS$compare)
  
  if (DS$compare == "Uninfected Control by definition") {

    dat <- dat |>
      left_join(UDC, by = c("plate", "definition", "Control_Region"))

    dat$Value <- 100 * dat$Value / dat$Uninfected_Value  # modules.R uninf_def_ctrl_data_selected VAL = "Uninfected_Value"
    dat$Uninfected_Value <- NULL

    return(dat)
  } else {
    col_ind <- match("Value", names(CTRL))
    c <- match("Value", names(dat))

    for (j in unique(na.omit(dat$Control_Region))) {
      for (i in unique(na.omit(dat$plate))) {
        r_ctrl <- ctrl_ind_fn(CTRL, i, j, DS$compare)

        if (length(r_ctrl) == 1) {
          u <- CTRL[[r_ctrl, col_ind]]
          r <- which(dat$plate == i & dat$Control_Region == j)

          dat[r, c] <- 100 * dat[r, c] / u
        }
      }
    }
    return(dat)
  }
}

#### % Growth ####
X_Growth_fn <- function(DAT,DS,CTRL,CSD,UDC) {
  dat <- DAT[which(!is.na(DAT$Sample)), ]
  col_ind <- match("Value", names(CTRL))

  pc <- "Positive Control" %in% CTRL$Sample

  c <- match("Value", names(dat))

  if (pc) {
    for (j in unique(na.omit(dat$Control_Region))) {
      for (i in unique(na.omit(dat$plate))) {
        p_ind <- ctrl_ind_fn(CTRL, i, j, "Positive Control")
        n_ind <- ctrl_ind_fn(CTRL, i, j, "Negative Control")

        if (length(p_ind) == 1 & length(n_ind) == 1) {
          p <- CTRL[[p_ind, col_ind]]
          n <- CTRL[[n_ind, col_ind]]
          r <- which(dat$plate == i & dat$Control_Region == j)
          dat[r, c] <- 100 * (dat[r, c] - p) / (n - p)
        }
      }
    }
    return(dat)

  } else {
    for (j in unique(na.omit(dat$Control_Region))) {
      for (i in unique(dat$plate)) {
        n_ind <- ctrl_ind_fn(CTRL, i, j, "Negative Control")

        if (length(n_ind) == 1) {
          n <- CTRL[[n_ind, col_ind]]
          r <- which(dat$plate == i & dat$Control_Region == j)

          dat[r, c] <- 100 * dat[r, c] / n
        }
      }
    }
    return(dat)
  }
}

X_Inhibition_fn <- function(DAT,DS,CTRL,CSD,UDC) {
  
  dat <- DAT[which(!is.na(DAT$Sample)), ]
  col_ind <- match("Value", names(CTRL))

  pc <- "Positive Control" %in% CTRL$Sample

  c <- match("Value", names(dat))

  if (pc) {
    for (j in unique(na.omit(dat$Control_Region))) {
      for (i in unique(na.omit(dat$plate))) {
        p_ind <- ctrl_ind_fn(CTRL, i, j, "Positive Control")
        n_ind <- ctrl_ind_fn(CTRL, i, j, "Negative Control")

        if (length(p_ind) == 1 & length(n_ind) == 1) {
          p <- CTRL[[p_ind, col_ind]]
          n <- CTRL[[n_ind, col_ind]]
          r <- which(dat$plate == i & dat$Control_Region == j)
          dat[r, c] <- 100 * (dat[r, c] - n) / (p - n)
        }
      }
    }
    return(dat)
  } else {
    for (j in unique(na.omit(dat$Control_Region))) {
      for (i in unique(dat$plate)) {
        n_ind <- ctrl_ind_fn(CTRL, i, j, "Negative Control")

        if (length(n_ind) == 1) {
          n <- CTRL[[n_ind, col_ind]]
          r <- which(dat$plate == i & dat$Control_Region == j)

          dat[r, c] <- 100 * (n - dat[r, c]) / n
        }
      }
    }
    return(dat)
  }
}

#### Z Factors ####
Z_Factors_fn <- function(DAT,DS,CTRL,CSD,UDC) {
  req(DS$compare)
  dat <- DAT[which(!is.na(DAT$Sample)), ] |>
    group_by(plate,Sample,definition,Control_Region) |>
    mutate(
      ZAVG = mean(Value, na.rm = TRUE),
      ZStDev = sd(Value, na.rm = TRUE)
    ) |>
    ungroup()
  
  col_ind <- match("Value", names(CTRL))
  
  c_val <- match("Value", names(dat))
  c_avg <- match("ZAVG", names(dat))
  c_sd <- match("ZStDev", names(dat))
  
  dat[[c_val]] <- as.numeric(dat[[c_val]])  # Convert Value to numeric in case they are integers (eg. ValidObjectCount)
  
  for (j in unique(na.omit(dat$Control_Region))) {
    for (i in unique(na.omit(dat$plate))) {
      ctrl_avg_ind <- ctrl_ind_fn(CTRL, i, j, DS$compare)
      ctrl_sd_ind <- ctrl_ind_fn(CSD, i, j, DS$compare)
      
      if (length(ctrl_avg_ind) == 1 & length(ctrl_sd_ind) == 1) {
        ctrl_avg <- CTRL[[ctrl_avg_ind, col_ind]]
        ctrl_sd <- CSD[[ctrl_sd_ind, col_ind]]
        
        r <- which(dat$plate == i & dat$Control_Region == j)
        
        dat[r, c_val] <- 1 - (3 * (dat[r, c_sd] + ctrl_sd)) / abs(dat[r, c_avg] - ctrl_avg)
      }
    }
  }
  return(dat)
}

#### Z Score ####
Z_Score_fn <- function(DAT,DS,CTRL,CSD,UDC) {
  dat <- DAT[which(!is.na(DAT$Sample)), ] |>
    group_by(plate, Sample, definition, Control_Region) |>
    mutate(
      Value = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE)
    ) |>
    ungroup()
  
  return(dat)
}

#### Total Fluorescence ####
Total_Fluorescence_fn <- function(DAT,DS,CTRL,CSD,UDC) {
  dat <- DAT
  
  dat$Value <- dat$Value * dat$Ref_Val
  
  return(dat)
}

#### %LOW to %HIGH ####
X_HIGH_fn <- function(DAT,DS,CTRL,CSD,UDC) {
  dat <- DAT
  
  dat$Value <- 100 - dat$Value
  
  return(dat)
}

#### Normalized % Infected ####
Normalized_X_Infected_fn <- function(DAT,DS,CTRL,CSD,UDC) {
  dat <- DAT[which(!is.na(DAT$Sample)), ] |>
  # dat <- DAT |>
    group_by(plate, definition, Sample, Control_Region) |>
    mutate(
      Value = Value / mean(Value) * Ref_Val
    ) |>
    ungroup()
  return(dat)
}

#### Infected Count ####
Infected_Count_fn <- function(DAT,DS,CTRL,CSD,UDC) {
  dat <- DAT
  dat$Value <- dat$Value * dat$Ref_Val / 100
  return(dat)
}

#### MEAN Infected ####
MEAN_Infected_fn <- function(DAT,DS,CTRL,CSD,UDC) {
  dat <- DAT
  dat$Value <- dat$Value * (100/dat$Ref_Val)
  return(dat)
}

#### MEAN % Growth ####
X_Growth_Infected_fn <- function(DAT,DS,CTRL,CSD,UDC) {
  # Calculate the MEAN_ of infected control values prior to taking average of control values
  infected_ctrl_mean <- MEAN_Infected_fn(CTRL[which(!CTRL$control_avgs), ]) |>
    group_by(plate, Sample, Control_Region) |>
    dplyr::summarize(across(where(is.numeric),
                     ~ mean(., na.rm = TRUE)), .groups = 'drop')
  infected_ctrl_mean$control_avgs <- TRUE
  
  dat <- DAT[which(!is.na(DAT$Sample)), ] |>
    MEAN_Infected_fn() |>
    X_Growth_fn(CTRL =  infected_ctrl_mean)
  return(dat)
}

#### MEAN % Inhibition ####
X_Inhibition_Infected_fn <- function(DAT,DS,CTRL,CSD,UDC) {
  infected_ctrl_mean <- MEAN_Infected_fn(CTRL[which(!CTRL$control_avgs), ]) |>
    group_by(plate, Sample, Control_Region) |>
    dplyr::summarize(across(where(is.numeric),
                     ~ mean(., na.rm = TRUE)), .groups = 'drop')
  infected_ctrl_mean$control_avgs <- TRUE
  
  dat <- DAT[which(!is.na(DAT$Sample)), ] |>
    MEAN_Infected_fn() |>
    X_Inhibition_fn(CTRL= infected_ctrl_mean)
  return(dat)
}

#### Transform ####

Transform_fn <- function(DAT,DS,CTRL,CSD,UDC) {
  dat <- DAT
  
  DS_transform <- req(DS$transform)
  
  operation <- switch(req(DS$operation),
                      "Y = Y * K" = c("*", TRUE),
                      "Y = Y + K" = c("+", TRUE),
                      "Y = Y / K" = c("/", TRUE),
                      "Y = Y - K" = c("-", TRUE),
                      "Y = K / Y" = c("/", FALSE),
                      "Y = K - Y" = c("-", FALSE),
                      "Y = K ^ Y" = c("^", FALSE),
                      "Y = Y ^ K" = c("^", TRUE),
                      "Y = (Y - K) / K * 100" = c("pct_change_fn", TRUE),
                      "Y = (K - Y) / Y * 100" = c("pct_change_fn", FALSE))
  
  fn <- get(operation[1])
  
  if (DS_transform == "definition" & length(DS$definition_group) > 0) {
    
    dat <- dat |>
      group_by(across(all_of(DS$definition_group))) |>
      mutate(
        Value = {
          m <- mean(Value[.data[[DS$definition_col]] %in% DS$definition], na.rm = TRUE)
          if (operation[2]) {
            fn(Value, m)
          } else {
            fn(m, Value)
          }
        }
      ) |>
      ungroup()
    
  } else {
    K <- switch (DS_transform,
                 "factor" = req(DS$factor),
                 "reference" = dat$Ref_Val,
                 "definition" = mean(dat[which(dat[[DS$definition_col]] %in% DS$definition), "Value"], na.rm = TRUE)
    )
    req(K)
    
    dat <- dat |>
      mutate(
        Value = {
          if (operation[2]) {
            fn(Value, K)
          } else {
            fn(K, Value)
          }
        }
      )
  }
  
  return(dat)
}


#### List of analysis functions ####
analysis_fns <- c("% Cell Survival" = X_Cell_Survival_fn,
                  "Normalized % Infected" = Normalized_X_Infected_fn,
                  "Infected Count" = Infected_Count_fn,
                  "% Growth" = X_Growth_fn,
                  "% Inhibition" = X_Inhibition_fn,
                  "Z Factors" = Z_Factors_fn,
                  "Z Score" = Z_Score_fn,
                  "Total Fluorescence" = Total_Fluorescence_fn,
                  "% Growth Infected" = X_Growth_Infected_fn,
                  "% Inhibition Infected" = X_Inhibition_Infected_fn,
                  "MEAN Infected" = MEAN_Infected_fn,
                  "% Spread" = X_Growth_fn,
                  "% Clearance" = X_Inhibition_fn,
                  "% HIGH" = X_HIGH_fn,
                  "Transform" = Transform_fn)

#### Outlier calculations ####
# Calculate z-score outliers
z_score_outliers <- function(x,y=3) {
  z <- (x - mean(x)) / sd(x)

  outliers <- rep(FALSE, length(x))
  outliers[abs(z) > y] <- TRUE
  outliers
}

# Calculate interquartile outliers
interquartile_outliers <- function(x,y=NULL) {
  lower_fence <- quantile(x, .25) - (1.5 * IQR(x))
  upper_fence <- quantile(x, .75) + (1.5 * IQR(x))

  outliers <- rep(FALSE, length(x))
  outliers[x < lower_fence | x > upper_fence] <- TRUE
  outliers
}

# Calculate Hampel filter outliers
hampel_filter_outliers <- function(x,y=3) {
  low <- median(x) - y * mad(x, constant=1)
  high <- median(x) + y * mad(x, constant=1)

  outliers <- rep(FALSE, length(x))
  outliers[x < low | x > high] <- TRUE
  outliers
}

# List outlier functions
outlier_fns <- c("Z-Score" = z_score_outliers,
                 "Interquartile Range" = interquartile_outliers,
                 "Hampel Filter" = hampel_filter_outliers)

