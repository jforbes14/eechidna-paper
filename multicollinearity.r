# Multicollinearity check with tour
library(tourr)
library(tidyverse)
library(eechidna)
source("functions.R")

load("data/abs2004_cd.rda")
load("data/abs2007_cd.rda")
load("data/abs2010_cd.rda")
load("data/abs2013_cd.rda")

# Take log of indigenous, judaism, islam, buddhism
abs2016 <- eechidna::abs2016 %>%
  mutate(
    Indigenous = log(Indigenous),
    Judaism = log(Judaism),
    Islam = log(Islam),
    Buddhism = log(Buddhism)
  )
abs2013 <- abs2013_cd %>% #eechidna::abs2013 %>%
  mutate(
    Indigenous = log(Indigenous),
    Judaism = log(Judaism),
    Islam = log(Islam),
    Buddhism = log(Buddhism)
  )
abs2010 <- abs2010_cd %>% #eechidna::abs2010 %>%
  mutate(
    Indigenous = log(Indigenous),
    Judaism = log(Judaism),
    Islam = log(Islam),
    Buddhism = log(Buddhism)
  )
abs2007 <- abs2007_cd %>% #eechidna::abs2007 %>%
  mutate(
    Indigenous = log(Indigenous),
    Judaism = log(Judaism),
    Islam = log(Islam),
    Buddhism = log(Buddhism)
  )
abs2004 <- abs2004_cd %>% #eechidna::abs2004 %>%
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
            standardise_vars(abs2004),
            by = c("DivisionNm")
  ) %>% mutate(year = "2004"),
  left_join(eechidna::tpp07,
            standardise_vars(abs2007),
            by = c("DivisionNm")
  ) %>% mutate(year = "2007"),
  left_join(eechidna::tpp10,
            standardise_vars(abs2010),
            by = c("DivisionNm")
  ) %>% mutate(year = "2010"),
  left_join(eechidna::tpp13,
            standardise_vars(abs2013),
            by = c("DivisionNm")
  ) %>% mutate(year = "2013"),
  left_join(eechidna::tpp16,
            standardise_vars(abs2016) %>%
              dplyr::select(-c(UniqueID, Area, ends_with("NS"), Population)),
            by = c("DivisionNm", "StateAb" = "State")
  ) %>% mutate(year = "2016")
) %>%
  mutate(year = factor(year), 
         Band_00_19 = Age00_04 + Age05_14 + Age15_19,
         Band_20_34 = Age20_24 + Age25_34,
         Band_35_54 = Age35_44 + Age45_54,
         Band_55plus = Age55_64 + Age65_74 + Age75_84 + Age85plus) %>% 
  select(-c(
    starts_with("Age"), MedianAge, StateAb,
    LNP_Votes, ALP_Votes, ALP_Percent, TotalVotes, Swing,
    InternetUse, InternetAccess, EnglishOnly,
    Other_NonChrist, OtherChrist, Catholic, Anglican,
    Volunteer, EmuneratedElsewhere, Population, UniqueID
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

d <- small_df %>% filter(year == "2016") %>%
  select(-DivisionNm, -LNP_Percent, -year)
quartz()
animate_xy(d, axes="bottomleft", half_range=1)

# For blog post
d1 <- small_df %>% filter(year == "2001") %>%
  select(-DivisionNm, -LNP_Percent, -year, -Band_55plus) %>%
  rename_all(function(.) paste0("V", 1:32))

d2 <- small_df %>% filter(year == "2016") %>%
  select(-DivisionNm, -LNP_Percent, -year, -Band_55plus) %>%
  rename_all(function(.) paste0("V", 1:32))

save(d1, file="d1.rda") 
save(d2, file="d2.rda") 

