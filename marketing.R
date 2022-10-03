library(plm)

marketing <- read.csv("Marketing Insight.csv")
summary(marketing)
str(marketing)

it_sector <- marketing[marketing$sector == "6", ]
summary(it_sector) #check data integrity
str(it_sector)

it_sector_clea <- it_sector[it_sector$mkt >= 0, ] #clean neg. mkt
it_sector_clean <- it_sector_clea[it_sector_clea$sales > 0, ] #clean zero sales
summary(it_sector_clean)

it_sector_scaled <- it_sector_clean
it_sector_scaled$ad <- it_sector_scaled$ad / it_sector_scaled$sales
it_sector_scaled$rd <- it_sector_scaled$rd / it_sector_scaled$sales

# lm(earnings ~ ad, ad^2) can also be used
# lags of variables?
# control variables: size (usually log(asset)),
# ad can be autocorrelated with earnings, i.e. ad budget set to a certain
# % of earnings
preliminary_regression <- lm(earnings ~ log(ad), data = it_sector_clean)
summary(preliminary_regression)

#interaction terms
inter_lm <- lm(earnings ~ ad * threat, data = it_sector_clean)
summary(inter_lm)

#gls?
prelim_gls <- glm(earnings ~ ad, data = it_sector_clean)
summary(prelim_gls)

#year dummies
# factor - 1?
fixed_dum <- lm(earnings ~ ad + factor(year) - 1, data = it_sector_clean)
summary(fixed_dum)

#fixed effect
fixed <- plm(earnings ~ ad,
             data = it_sector_clean, index = "year", model = "within")
summary(fixed)
fixef(fixed)

#random effect
random <- plm(earnings ~ ad,
              data = it_sector_clean, index = "year", model = "random")
summary(random)

#hausman test
phtest(fixed, random)

#lvl 1
dummy_lvl1 <- lm(earnings ~ ad + rd + threat * ad
                  + factor(year) - 1, data = it_sector_clean)
summary(dummy_lvl1)

#lvl 2
fe_lvl2 <- plm(earnings ~ ad + rd + threat * ad
               , index = c("id", "year"), model = "within"
               , effect = "twoways", data = it_sector_clean)
summary(fe_lvl2)
#lvl 3

# LOG
it_sector_log <- it_sector_clean
it_sector_log <- it_sector_log[it_sector_log$ad > 0, ]
it_sector_log <- it_sector_log[it_sector_log$rd > 0, ]
it_sector_log <- it_sector_log[it_sector_log$earnings > 0, ]

fe_log <- plm(log(earnings) ~ log(ad) + log(rd) + threat * log(ad),
    index = c("id", "year"), model = "within",
    effect = "twoways", data = it_sector_log
)
summary(fe_log)

#SCALED
it_sector_scaled <- it_sector_clean
it_sector_scaled$ad <- it_sector_scaled$ad / it_sector_scaled$sales
it_sector_scaled$rd <- it_sector_scaled$rd / it_sector_scaled$sales

fe_lvl2 <- plm(fv ~ ad + rd + threat * ad,
    index = c("id", "year"), model = "within",
    effect = "twoways", data = it_sector_scaled
)
summary(fe_lvl2)