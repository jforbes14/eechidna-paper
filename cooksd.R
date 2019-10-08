## @knitr GetCookDistance

# 2016

my_formula = "LNP_Percent ~ ."
my_data = model_df %>% filter(year == "2016")
sp_weights = sp_weights_16

# DivisionNm
model_data <- my_data %>% dplyr::select(-c(DivisionNm, year))

# Spatial weights matrix
w_mat <- listw2mat(sp_weights)

# Get OLS residuals
ols_model <- lm(my_formula, model_data)
my_res <- ols_model$residuals

# Solve for rho
res_model <- lm(my_res ~ w_mat%*%my_res)
rho <- res_model$coefficients[2]
rho_df <- data.frame(estimate = rho, se = summary(res_model)$coefficients[2,2], p = summary(res_model)$coefficients[2,4])

# Transform data for GLS
trans_mat <- diag(nrow(model_data)) - rho*w_mat
gls_data <- data.frame(LNP_Percent = trans_mat %*% model_data$LNP_Percent,
  Intercept = trans_mat %*% rep(1,nrow(model_data))) %>% 
  bind_cols(as.data.frame(trans_mat %*% as.matrix(model_data %>% dplyr::select(-LNP_Percent))))

# GLS model
my_formula <- formula(paste0(my_formula,  " - 1"))
gls_model <- gls(my_formula, gls_data)

# Cooks distance
glsmod16$cooksd <- unname(predictmeans::CookD(gls_model, plot = FALSE))

# Hat values
glsmod16$hatvalues <- hat(model_data %>% dplyr::select(-LNP_Percent))

# REMOVE

remove(gls_model)
remove(my_formula)
remove(gls_data)
remove(trans_mat)
remove(rho)
remove(rho_df)
remove(res_model)
remove(my_res)
remove(ols_model)
remove(w_mat)
remove(model_data)
remove(sp_weights)
remove(my_data)


# 2013

my_formula = "LNP_Percent ~ ."
my_data = model_df %>% filter(year == "2013")
sp_weights = sp_weights_13

# DivisionNm
model_data <- my_data %>% dplyr::select(-c(DivisionNm, year))

# Spatial weights matrix
w_mat <- listw2mat(sp_weights)

# Get OLS residuals
ols_model <- lm(my_formula, model_data)
my_res <- ols_model$residuals

# Solve for rho
res_model <- lm(my_res ~ w_mat%*%my_res)
rho <- res_model$coefficients[2]
rho_df <- data.frame(estimate = rho, se = summary(res_model)$coefficients[2,2], p = summary(res_model)$coefficients[2,4])

# Transform data for GLS
trans_mat <- diag(nrow(model_data)) - rho*w_mat
gls_data <- data.frame(LNP_Percent = trans_mat %*% model_data$LNP_Percent,
  Intercept = trans_mat %*% rep(1,nrow(model_data))) %>% 
  bind_cols(as.data.frame(trans_mat %*% as.matrix(model_data %>% dplyr::select(-LNP_Percent))))

# GLS model
my_formula <- formula(paste0(my_formula,  " - 1"))
gls_model <- gls(my_formula, gls_data)

# Cooks distance
glsmod13$cooksd <- unname(predictmeans::CookD(gls_model, plot = FALSE))

# Hat values
glsmod13$hatvalues <- hat(model_data %>% dplyr::select(-LNP_Percent))

# REMOVE

remove(gls_model)
remove(my_formula)
remove(gls_data)
remove(trans_mat)
remove(rho)
remove(rho_df)
remove(res_model)
remove(my_res)
remove(ols_model)
remove(w_mat)
remove(model_data)
remove(sp_weights)
remove(my_data)


# 2010

my_formula = "LNP_Percent ~ ."
my_data = model_df %>% filter(year == "2010")
sp_weights = sp_weights_10

# DivisionNm
model_data <- my_data %>% dplyr::select(-c(DivisionNm, year))

# Spatial weights matrix
w_mat <- listw2mat(sp_weights)

# Get OLS residuals
ols_model <- lm(my_formula, model_data)
my_res <- ols_model$residuals

# Solve for rho
res_model <- lm(my_res ~ w_mat%*%my_res)
rho <- res_model$coefficients[2]
rho_df <- data.frame(estimate = rho, se = summary(res_model)$coefficients[2,2], p = summary(res_model)$coefficients[2,4])

# Transform data for GLS
trans_mat <- diag(nrow(model_data)) - rho*w_mat
gls_data <- data.frame(LNP_Percent = trans_mat %*% model_data$LNP_Percent,
  Intercept = trans_mat %*% rep(1,nrow(model_data))) %>% 
  bind_cols(as.data.frame(trans_mat %*% as.matrix(model_data %>% dplyr::select(-LNP_Percent))))

# GLS model
my_formula <- formula(paste0(my_formula,  " - 1"))
gls_model <- gls(my_formula, gls_data)

# Cooks distance
glsmod10$cooksd <- unname(predictmeans::CookD(gls_model, plot = FALSE))

# Hat values
glsmod10$hatvalues <- hat(model_data %>% dplyr::select(-LNP_Percent))

# REMOVE

remove(gls_model)
remove(my_formula)
remove(gls_data)
remove(trans_mat)
remove(rho)
remove(rho_df)
remove(res_model)
remove(my_res)
remove(ols_model)
remove(w_mat)
remove(model_data)
remove(sp_weights)
remove(my_data)


# 2007

my_formula = "LNP_Percent ~ ."
my_data = model_df %>% filter(year == "2007")
sp_weights = sp_weights_07

# DivisionNm
model_data <- my_data %>% dplyr::select(-c(DivisionNm, year))

# Spatial weights matrix
w_mat <- listw2mat(sp_weights)

# Get OLS residuals
ols_model <- lm(my_formula, model_data)
my_res <- ols_model$residuals

# Solve for rho
res_model <- lm(my_res ~ w_mat%*%my_res)
rho <- res_model$coefficients[2]
rho_df <- data.frame(estimate = rho, se = summary(res_model)$coefficients[2,2], p = summary(res_model)$coefficients[2,4])

# Transform data for GLS
trans_mat <- diag(nrow(model_data)) - rho*w_mat
gls_data <- data.frame(LNP_Percent = trans_mat %*% model_data$LNP_Percent,
  Intercept = trans_mat %*% rep(1,nrow(model_data))) %>% 
  bind_cols(as.data.frame(trans_mat %*% as.matrix(model_data %>% dplyr::select(-LNP_Percent))))

# GLS model
my_formula <- formula(paste0(my_formula,  " - 1"))
gls_model <- gls(my_formula, gls_data)

# Cooks distance
glsmod07$cooksd <- unname(predictmeans::CookD(gls_model, plot = FALSE))

# Hat values
glsmod07$hatvalues <- hat(model_data %>% dplyr::select(-LNP_Percent))

# REMOVE

remove(gls_model)
remove(my_formula)
remove(gls_data)
remove(trans_mat)
remove(rho)
remove(rho_df)
remove(res_model)
remove(my_res)
remove(ols_model)
remove(w_mat)
remove(model_data)
remove(sp_weights)
remove(my_data)


# 2004

my_formula = "LNP_Percent ~ ."
my_data = model_df %>% filter(year == "2004")
sp_weights = sp_weights_04

# DivisionNm
model_data <- my_data %>% dplyr::select(-c(DivisionNm, year))

# Spatial weights matrix
w_mat <- listw2mat(sp_weights)

# Get OLS residuals
ols_model <- lm(my_formula, model_data)
my_res <- ols_model$residuals

# Solve for rho
res_model <- lm(my_res ~ w_mat%*%my_res)
rho <- res_model$coefficients[2]
rho_df <- data.frame(estimate = rho, se = summary(res_model)$coefficients[2,2], p = summary(res_model)$coefficients[2,4])

# Transform data for GLS
trans_mat <- diag(nrow(model_data)) - rho*w_mat
gls_data <- data.frame(LNP_Percent = trans_mat %*% model_data$LNP_Percent,
  Intercept = trans_mat %*% rep(1,nrow(model_data))) %>% 
  bind_cols(as.data.frame(trans_mat %*% as.matrix(model_data %>% dplyr::select(-LNP_Percent))))

# GLS model
my_formula <- formula(paste0(my_formula,  " - 1"))
gls_model <- gls(my_formula, gls_data)

# Cooks distance
glsmod04$cooksd <- unname(predictmeans::CookD(gls_model, plot = FALSE))

# Hat values
glsmod04$hatvalues <- hat(model_data %>% dplyr::select(-LNP_Percent))

# REMOVE

remove(gls_model)
remove(my_formula)
remove(gls_data)
remove(trans_mat)
remove(rho)
remove(rho_df)
remove(res_model)
remove(my_res)
remove(ols_model)
remove(w_mat)
remove(model_data)
remove(sp_weights)
remove(my_data)


# 2001

my_formula = "LNP_Percent ~ ."
my_data = model_df %>% filter(year == "2001")
sp_weights = sp_weights_01

# DivisionNm
model_data <- my_data %>% dplyr::select(-c(DivisionNm, year))

# Spatial weights matrix
w_mat <- listw2mat(sp_weights)

# Get OLS residuals
ols_model <- lm(my_formula, model_data)
my_res <- ols_model$residuals

# Solve for rho
res_model <- lm(my_res ~ w_mat%*%my_res)
rho <- res_model$coefficients[2]
rho_df <- data.frame(estimate = rho, se = summary(res_model)$coefficients[2,2], p = summary(res_model)$coefficients[2,4])

# Transform data for GLS
trans_mat <- diag(nrow(model_data)) - rho*w_mat
gls_data <- data.frame(LNP_Percent = trans_mat %*% model_data$LNP_Percent,
  Intercept = trans_mat %*% rep(1,nrow(model_data))) %>% 
  bind_cols(as.data.frame(trans_mat %*% as.matrix(model_data %>% dplyr::select(-LNP_Percent))))

# GLS model
my_formula <- formula(paste0(my_formula,  " - 1"))
gls_model <- gls(my_formula, gls_data)

# Cooks distance
glsmod01$cooksd <- unname(predictmeans::CookD(gls_model, plot = FALSE))

# Hat values
glsmod01$hatvalues <- hat(model_data %>% dplyr::select(-LNP_Percent))

# REMOVE

remove(gls_model)
remove(my_formula)
remove(gls_data)
remove(trans_mat)
remove(rho)
remove(rho_df)
remove(res_model)
remove(my_res)
remove(ols_model)
remove(w_mat)
remove(model_data)
remove(sp_weights)
remove(my_data)
