# Take log of indigenous, judaism, islam, buddhism
abs2016 <- eechidna::abs2016 %>%
  mutate(
    Indigenous = log(Indigenous),
    Judaism = log(Judaism),
    Islam = log(Islam),
    Buddhism = log(Buddhism)
  )
abs2013 <- eechidna::abs2013 %>%
  mutate(
    Indigenous = log(Indigenous),
    Judaism = log(Judaism),
    Islam = log(Islam),
    Buddhism = log(Buddhism)
  )
abs2010 <- eechidna::abs2010 %>%
  mutate(
    Indigenous = log(Indigenous),
    Judaism = log(Judaism),
    Islam = log(Islam),
    Buddhism = log(Buddhism)
  )
abs2007 <- eechidna::abs2007 %>%
  mutate(
    Indigenous = log(Indigenous),
    Judaism = log(Judaism),
    Islam = log(Islam),
    Buddhism = log(Buddhism)
  )
abs2004 <- eechidna::abs2004 %>%
  mutate(
    Indigenous = log(Indigenous),
    Judaism = log(Judaism),
    Islam = log(Islam),
    Buddhism = log(Buddhism)
  )
abs2001 <- eechidna::abs2001 %>%
  mutate(
    Indigenous = log(Indigenous),
    Judaism = log(Judaism),
    Islam = log(Islam),
    Buddhism = log(Buddhism)
  )

# Change abs2016 to data frame to prevent error below
abs2016 <- as.data.frame(abs2016)

# Combine and standardize
my_df <- bind_rows(
  left_join(eechidna::tpp01,
    standardise_vars(abs2001) %>%
      dplyr::select(-c(UniqueID, Area, ends_with("NS"), Population)),
    by = c("DivisionNm", "StateAb" = "State")
  ) %>% mutate(year = "2001"),
  left_join(eechidna::tpp04,
    standardise_vars(abs2004) %>%
      dplyr::select(-UniqueID),
    by = c("DivisionNm")
  ) %>% mutate(year = "2004"),
  left_join(eechidna::tpp07,
    standardise_vars(abs2007) %>%
      dplyr::select(-UniqueID),
    by = c("DivisionNm")
  ) %>% mutate(year = "2007"),
  left_join(eechidna::tpp10,
    standardise_vars(abs2010) %>%
      dplyr::select(-UniqueID),
    by = c("DivisionNm")
  ) %>% mutate(year = "2010"),
  left_join(eechidna::tpp13,
    standardise_vars(abs2013) %>%
      dplyr::select(-UniqueID),
    by = c("DivisionNm")
  ) %>% mutate(year = "2013"),
  left_join(eechidna::tpp16,
    standardise_vars(abs2016) %>%
      dplyr::select(-c(UniqueID, Area, ends_with("NS"), Population)),
    by = c("DivisionNm", "StateAb" = "State")
  ) %>% mutate(year = "2016")
) %>%
  mutate(year = factor(year)) %>%
  select(-c(
    starts_with("Age"), StateAb,
    LNP_Votes, ALP_Votes, ALP_Percent, TotalVotes, Swing,
    InternetUse, InternetAccess, EnglishOnly,
    Other_NonChrist, OtherChrist, Catholic, Anglican,
    Volunteer, EmuneratedElsewhere #, DivisionID
  ))

# Create final df for modelling

factors_df <- my_df %>%
  mutate(
    Education = BachelorAbv + HighSchool + Professional + Finance - Laborer - Tradesperson - DipCert,
    FamHouseSize = FamilyRatio + AverageHouseholdSize + Couple_WChild_House - Couple_NoChild_House - SP_House,
    PropertyOwned = Owned + Mortgage - Renting - PublicHousing,
    RentLoanPrice = MedianRent + MedianLoanPay,
    Incomes = MedianFamilyIncome + MedianHouseholdIncome + MedianPersonalIncome,
    Unemployment = Unemployed - LFParticipation
  ) %>%
  dplyr::select(-c(
    BachelorAbv, HighSchool, Professional, Finance, Laborer, Tradesperson, DipCert, FamilyRatio,
    AverageHouseholdSize, Couple_WChild_House, Couple_NoChild_House, SP_House, Owned, Mortgage, Renting,
    PublicHousing, MedianFamilyIncome, MedianHouseholdIncome, MedianPersonalIncome, MedianRent,
    MedianLoanPay, Unemployed, LFParticipation
  ))

# Now standardize factors

small_df <- bind_rows(
  factors_df %>% filter(year == "2001") %>% standardise_vars(),
  factors_df %>% filter(year == "2004") %>% standardise_vars(),
  factors_df %>% filter(year == "2007") %>% standardise_vars(),
  factors_df %>% filter(year == "2010") %>% standardise_vars(),
  factors_df %>% filter(year == "2013") %>% standardise_vars(),
  factors_df %>% filter(year == "2016") %>% standardise_vars()
)

# Order electorates in alphabetical order to match spatial matrix
model_df <- small_df %>%
  arrange(year, DivisionNm) %>%
  dplyr::select(order(colnames(.))) %>%
  rename(
    ManagerAdmin = ManagerAdminClericalSales,
    OtherLanguage = OtherLanguageHome,
    BornSEEuro = Born_SE_Europe,
    BornAsia = Born_Asia,
    OneParentHouse = OneParent_House
  )

# Get spatial weights
# This is not run, as weights are pulled from github, but this can be run to get them from scratch.
# sF_16 <- sF_download(2016)
# sF_13 <- sF_download(2013)
# sF_10 <- sF_download(2010)
# sF_07 <- sF_download(2007)
# sF_04 <- sF_download(2004)
# sF_01 <- sF_download(2001)

# sp_weights_16 <- sp_weights_matrix(sF_16)
# sp_weights_13 <- sp_weights_matrix(sF_13)
# sp_weights_10 <- sp_weights_matrix(sF_10)
# sp_weights_07 <- sp_weights_matrix(sF_07)
# sp_weights_04 <- sp_weights_matrix(sF_04)
# sp_weights_01 <- sp_weights_matrix(sF_01)


# Instead of running spatial weights, load from github
load("data/sp_weights_01.rda")
load("data/sp_weights_04.rda")
load("data/sp_weights_07.rda")
load("data/sp_weights_10.rda")
load("data/sp_weights_13.rda")
load("data/sp_weights_16.rda")

## Run full models for each year
full_formula <- "LNP_Percent ~ ."

# 2016
glsmod16 <- my_fgls(full_formula,
  my_data = model_df %>% filter(year == "2016"),
  sp_weights = sp_weights_16
)

# 2013
glsmod13 <- my_fgls(full_formula,
  my_data = model_df %>% filter(year == "2013"),
  sp_weights = sp_weights_13
)

# 2010
glsmod10 <- my_fgls(full_formula,
  my_data = model_df %>% filter(year == "2010"),
  sp_weights = sp_weights_10
)

# 2007
glsmod07 <- my_fgls(full_formula,
  my_data = model_df %>% filter(year == "2007"),
  sp_weights = sp_weights_07
)

# 2004
glsmod04 <- my_fgls(full_formula,
  my_data = model_df %>% filter(year == "2004"),
  sp_weights = sp_weights_04
)

# 2001
glsmod01 <- my_fgls(full_formula,
  my_data = model_df %>% filter(year == "2001"),
  sp_weights = sp_weights_01
)

## Visualise coefficients and significance

coef_df <- bind_rows(
  data.frame(
    variable = glsmod16$coefficients %>% names(),
    estimate = glsmod16$coefficients %>% unname(),
    se = summary(glsmod16)$tTable[, "Std.Error"] %>% unname(),
    p = summary(glsmod16)$tTable[, "p-value"] %>% unname(),
    year = 2016),
  data.frame(
    variable = glsmod13$coefficients %>% names(),
    estimate = glsmod13$coefficients %>% unname(),
    se = summary(glsmod13)$tTable[, "Std.Error"] %>% unname(),
    p = summary(glsmod13)$tTable[, "p-value"] %>% unname(),
    year = 2013),
  data.frame(
    variable = glsmod10$coefficients %>% names(),
    estimate = glsmod10$coefficients %>% unname(),
    se = summary(glsmod10)$tTable[, "Std.Error"] %>% unname(),
    p = summary(glsmod10)$tTable[, "p-value"] %>% unname(),
    year = 2010),
  data.frame(
    variable = glsmod07$coefficients %>% names(),
    estimate = glsmod07$coefficients %>% unname(),
    se = summary(glsmod07)$tTable[, "Std.Error"] %>% unname(),
    p = summary(glsmod07)$tTable[, "p-value"] %>% unname(),
    year = 2007),
  data.frame(
    variable = glsmod04$coefficients %>% names(),
    estimate = glsmod04$coefficients %>% unname(),
    se = summary(glsmod04)$tTable[, "Std.Error"] %>% unname(),
    p = summary(glsmod04)$tTable[, "p-value"] %>% unname(),
    year = 2004),
  data.frame(
    variable = glsmod01$coefficients %>% names(),
    estimate = glsmod01$coefficients %>% unname(),
    se = summary(glsmod01)$tTable[, "Std.Error"] %>% unname(),
    p = summary(glsmod01)$tTable[, "p-value"] %>% unname(),
    year = 2001)
)

