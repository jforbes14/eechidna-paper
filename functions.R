# Function to standardize variables
standardise_vars <- function(df) {
  hold <- df
  num_cols <- sapply(df, class) == "numeric"
  df[, num_cols] <- lapply(df[, num_cols], scale)
  names(df) <- names(hold)
  df$LNP_Percent <- hold$LNP_Percent
  return(df)
}

# Compute spatial weights matrix
sp_weights_matrix <- function(sF) {
  dist_matrix <- function(shapefile) {
    dist_mat <- matrix(NA, nrow = 150, ncol = 150)
    rownames(dist_mat) <- sort(shapefile$elect_div)
    colnames(dist_mat) <- sort(shapefile$elect_div)

    for (i in 1:(nrow(dist_mat) - 1)) {
      rname <- rownames(dist_mat)[i]
      row_poly <- shapefile %>% subset(elect_div == rname)

      for (j in (i + 1):ncol(dist_mat)) {
        cname <- rownames(dist_mat)[j]
        col_poly <- shapefile %>% subset(elect_div == cname)
        dist <- gDistance(row_poly, col_poly)
        dist_mat[i, j] <- dist
      }
      print(i)
    }

    # Now copy to lower triange
    for (i in 2:nrow(dist_mat)) {
      for (j in 1:(i - 1)) {
        dist_mat[i, j] <- dist_mat[j, i]
      }
    }

    # Check it is symmetric
    if (!isSymmetric(dist_mat)) {
      print("Warning! Matrix is not symmetric. Error has occured.")
    }

    return(dist_mat)
  }

  my_dist_mat <- dist_matrix(sF)

  # S matrix

  s_mat <- function(dist_mat) {
    s_mat <- dist_mat

    for (i in 1:nrow(dist_mat)) {
      for (j in 1:nrow(dist_mat)) {
        a <- dist_mat[i, j]

        if (is.na(a)) {
          b <- 0
        } else {
          b <- ifelse(a == 0, 1, 0)
        }

        s_mat[i, j] <- b
      }
    }

    return(s_mat)
  }

  my_smat <- s_mat(my_dist_mat)

  # Turn into W matrix

  w_mat <- function(s_mat) {
    w_mat <- s_mat
    rsums <- rowSums(s_mat)

    for (i in 1:nrow(s_mat)) {
      w_mat[i, ] <- s_mat[i, ] / rsums[i]
    }

    return(w_mat)
  }

  my_wmat <- w_mat(my_smat)

  # Turn into listw

  my_listw <- mat2listw(my_wmat)

  return(my_listw)
}

# Function for FGLS
my_fgls <- function(my_formula, my_data, sp_weights) {
  # DivisionNm
  model_data <- my_data %>% dplyr::select(-c(DivisionNm, year))

  # Spatial weights matrix
  w_mat <- listw2mat(sp_weights)

  # Get OLS residuals
  ols_model <- lm(my_formula, model_data)
  my_res <- ols_model$residuals

  # Solve for rho
  res_model <- lm(my_res ~ w_mat %*% my_res)
  rho <- res_model$coefficients[2]
  rho_df <- data.frame(estimate = rho, se = summary(res_model)$coefficients[2, 2], p = summary(res_model)$coefficients[2, 4])

  # Transform data for GLS
  trans_mat <- diag(nrow(model_data)) - rho * w_mat
  gls_data <- data.frame(
    LNP_Percent = trans_mat %*% model_data$LNP_Percent,
    Intercept = trans_mat %*% rep(1, nrow(model_data))
  ) %>%
    bind_cols(as.data.frame(trans_mat %*% as.matrix(model_data %>% dplyr::select(-LNP_Percent))))

  # GLS model
  my_formula <- formula(paste0(my_formula, " - 1"))
  gls_model <- gls(my_formula, gls_data)

  # Cooks distance
  # gls_model$cooksd <- unname(predictmeans::CookD(gls_model, plot = FALSE))

  # Call to function with stargazer
  gls_model$call$model <- formula(paste0("LNP_Percent ~ ", paste0(names(gls_data)[-1], collapse = " + ")))

  # Rho and data
  gls_model$rho_df <- rho_df
  gls_model$gls_data <- gls_data
  gls_model$my_data <- my_data
  gls_model$actual_residuals <- solve(trans_mat) %*% gls_model$residuals

  return(gls_model)
}

# Function to produce visreg style conditional plots
my_visreg <- function(my_model, sp_weights, varname,
  plot = FALSE, nolabs = FALSE, xlimits = NULL, ylimits = NULL, year = "") {

  # Extract fitted parameters
  rho <- my_model$rho_df$estimate
  sigma <- sqrt(sum(my_model$residuals^2)/(my_model$dims$N-my_model$dims$p))

  # Spatial weights
  w_mat <- listw2mat(sp_weights)

  # Q - where u = Qe, Q = (I - pW)^-1
  q_mat <- solve(diag(my_model$dims$N) - rho*w_mat)

  # Omega - QQ'
  omega_mat <- q_mat%*%t(q_mat)

  # X
  x_mat <- my_model$my_data %>%
    dplyr::select(-c(LNP_Percent, year, DivisionNm)) %>%
    mutate(Intercept = 1) %>%
    dplyr::select(Intercept, everything()) %>%
    as.matrix()

  # Beta
  beta_mat <- my_model$coefficients

  # T value
  t = qt(0.975, nrow(my_model$gls_data)-ncol(my_model$gls_data))

  # Lambda matrix (FGLS)
  x <- round(seq(min(as.numeric(x_mat[, varname])), max(as.numeric(x_mat[, varname])), 0.025), 3)
  lambda_mat <- data.frame(matrix(0, nrow = length(x), ncol = ncol(x_mat)))
  names(lambda_mat) <- dimnames(x_mat)[[2]]
  lambda_mat[, varname] <- x
  lambda_mat$Intercept <- 1
  lambda_mat <- as.matrix(lambda_mat)

  # Confidence interval
  plot_df <- data.frame(variable = x, fitted = lambda_mat%*%beta_mat, variance = 0)

  for (i in 1:nrow(lambda_mat)) {
    lambda <- lambda_mat[i, ]
    plot_df$variance[i] = sigma^2 * t(lambda) %*%
      solve(t(x_mat) %*% solve(omega_mat) %*% x_mat) %*%
      lambda
  }

  plot_df <- plot_df %>%
    mutate(upper95 = fitted + t*sqrt(variance), lower95 = fitted - t*sqrt(variance))

  # Partial residuals
  points_df <- data.frame(
    variable = my_model$my_data[, varname] %>% unname,
    part_res = (my_model$my_data$LNP_Percent - x_mat%*%my_model$coefficients) + my_model$coefficients[varname]*x_mat[, varname] + my_model$coefficients[1]
  )

  # Plot
  if (plot == TRUE) {
      myplot <- ggplot(data = plot_df) +
    geom_ribbon(aes(x = variable, ymin = lower95, ymax = upper95), fill = "grey80") +
    geom_point(aes(x = variable, y = part_res), data = points_df, size = 0.75, col = "grey50") +
    geom_line(aes(x = variable, y = fitted), col = "blue", size = 1) +
    #geom_hline(aes(yintercept = min(upper95)), col = "red") +
    #geom_hline(aes(yintercept = max(lower95)), col = "blue") +
    theme_bw() +
    labs(x = varname, y = "Response") +
    ggtitle(year) +
    theme(plot.title = element_text(face = "bold", size = 10, hjust = 0.5))

  if (nolabs == TRUE) {
    myplot <- myplot + labs(x = "", y = "")
  }

  if (!is.null(xlimits) & !is.null(ylimits)) {
    myplot <- myplot + coord_cartesian(xlim = xlimits, ylim = ylimits)
  }
      return(myplot)
  }

  # Points
  if (plot == FALSE) {
    return_ls <- list(bands = plot_df, points = points_df)

    return(return_ls)
  }


}

# Grid visreg
grid_visreg <- function(varname, plot = TRUE, myscale = "free_y", top = FALSE) {

    p16 <- my_visreg(glsmod16, sp_weights_16, varname = varname, plot = FALSE, year = "2016")
    p13 <- my_visreg(glsmod13, sp_weights_13, varname = varname, plot = FALSE, year = "2013")
    p10 <- my_visreg(glsmod10, sp_weights_10, varname = varname, plot = FALSE, year = "2010")
    p07 <- my_visreg(glsmod07, sp_weights_07, varname = varname, plot = FALSE, year = "2007")
    p04 <- my_visreg(glsmod04, sp_weights_04, varname = varname, plot = FALSE, year = "2004")
    p01 <- my_visreg(glsmod01, sp_weights_01, varname = varname, plot = FALSE, year = "2001")

    bands_df <- bind_rows(
      p16$bands %>% mutate(year = "2016"),
      p13$bands %>% mutate(year = "2013"),
      p10$bands %>% mutate(year = "2010"),
      p07$bands %>% mutate(year = "2007"),
      p04$bands %>% mutate(year = "2004"),
      p01$bands %>% mutate(year = "2001")
    ) %>% mutate(varname = varname)

    points_df <- bind_rows(
      p16$points %>% mutate(year = "2016"),
      p13$points %>% mutate(year = "2013"),
      p10$points %>% mutate(year = "2010"),
      p07$points %>% mutate(year = "2007"),
      p04$points %>% mutate(year = "2004"),
      p01$points %>% mutate(year = "2001")
    ) %>% mutate(varname = varname)

    if (plot == T) {
      return_object <- ggplot(data = bands_df) +
        geom_ribbon(aes(x = variable, ymin = lower95, ymax = upper95), fill = "grey80") +
        geom_point(aes(x = variable, y = part_res), data = points_df, size = 0.5, shape = 1, alpha = 0.5) +
        geom_line(aes(x = variable, y = fitted), col = "blue", size = 1) +
        theme_bw() +
        labs(x = "", y = "") +
        facet_grid(varname ~ year, scales = "free_y") +
        theme(plot.margin=unit(c(b = 0.05, l = 0, t = -0.2, r = 0),"cm"))

      if (top == FALSE) {
        return_object <- return_object + theme(strip.text.x = element_blank())
      }

      if (myscale == "free") {
        return_object <- return_object + lims(x = range(model_df %>% select(varname)),
          y = range(c(bands_df$upper95, bands_df$lower95, points_df$part_res)))
      }

      if (myscale == "free_x") {
        return_object <- return_object + lims(x = range(model_df %>% select(varname)))
      }

      if (myscale == "free_y") {
        return_object <- return_object + lims(y = range(c(bands_df$upper95, bands_df$lower95, points_df$part_res)))
      }

      if (myscale == "none") {
        return_object <- return_object
      }
    }

    if (plot == F) {
      return_object <- list(bands = bands_df, points = points_df)
    }

    return(return_object)

}
