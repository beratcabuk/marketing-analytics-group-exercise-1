it_sector_scaled <- it_sector_clean #remove
it_sector_scaled$ad <- it_sector_scaled$ad / it_sector_scaled$sales #remove
it_sector_scaled$rd <- it_sector_scaled$rd / it_sector_scaled$sales #remove

# lm(earnings ~ ad, ad^2) can also be used
# lags of variables?
# control variables: size (usually log(asset)),
# ad can be autocorrelated with earnings, i.e. ad budget set to a certain
# % of earnings
preliminary_regression <- lm(earnings ~ ad, data = it_sector_clean)
summary(preliminary_regression)

#interaction terms
inter_lm <- lm(earnings ~ ad * threat, data = it_sector_clean)
summary(inter_lm)

#gls?
prelim_gls <- glm(earnings ~ ad, data = it_sector_clean)
summary(prelim_gls)

#year dummies
# factor - 1?
fixed_dum <- lm(earnings ~ ad + factor(year) - 1, data = it_sector)
summary(fixed_dum)

#fixed effect
fixed <- plm(earnings ~ ad + rd + threat * ad,
             data = it_sector, index = c("id", "year"), effect = "twoways",
             model = "within")
summary(fixed)
fixef(fixed)

#random effect
random <- plm(earnings ~ ad + rd + threat * ad,
              data = it_sector, index = c("id", "year"), effect = "twoways",
              model = "random")
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


fe_log <- plm(earnings ~ log(1 + ad) + log(1 + rd) + threat * log(1 + ad),
    index = c("id", "year"), model = "within",
    effect = "twoways", data = it_sector
)
summary(fe_log)

#ROA
it_sector_roa <- it_sector_clean
it_sector_roa$roa <- it_sector_roa$earnings / it_sector_roa$assets
summary(it_sector_roa)
fe_roa <- plm(roa ~ log(1 + ad) + log(1 + rd) + threat * log(1 + ad),
    index = c("id", "year"), model = "within",
    effect = "twoways", data = it_sector_roa
)
summary(fe_roa)

#SCALED
it_sector_scaled <- it_sector_clean #remove
it_sector_scaled$ad <- it_sector_scaled$ad / it_sector_scaled$sales #remove
it_sector_scaled$rd <- it_sector_scaled$rd / it_sector_scaled$sales #remove
#remove
it_sector_scaled$earnings <- it_sector_scaled$earnings / it_sector_scaled$sales

fe_lvl2 <- plm(earnings ~ ad + rd + threat * ad,
    index = c("id", "year"), model = "within",
    effect = "twoways", data = it_sector_scaled
)
summary(fe_lvl2)

# AFTER CONSULT. SESSION
library(plm)
library(car)
library(ggplot2)
library(margins)
library(corrplot)

marketing <- read.csv("Marketing Insight.csv")
summary(marketing)

it_sector <- marketing[marketing$sector == "6", ]
summary(it_sector) # check data integrity

it_sector_clea <- it_sector[it_sector$mkt >= 0, ] #clean neg. mkt
it_sector_clean <- it_sector_clea[it_sector_clea$sales > 0, ] # clean zero sales
summary(it_sector_clean)

it_sector_final <- it_sector_clean
it_sector_final$ad_sales <- it_sector_final$ad / it_sector_final$sales
it_sector_final$rd_sales <- it_sector_final$rd / it_sector_final$sales
it_sector_final$roa <- it_sector_final$earnings / it_sector_final$assets

summary(it_sector_final)

corr_matrix_data <- it_sector_final[c("mv", "ad", "rd", "assets",
                                    "threat", "debt")]
summary(corr_matrix_data)
correlations <- cor(log1p(corr_matrix_data))
correlations <- cor(corr_matrix_data)
corrplot(correlations, "number")

fe_sug1 <- plm(roa ~ ad_sales + rd_sales + threat * ad_sales,
    index = c("id", "year"), model = "within",
    effect = "twoways", data = it_sector_final
)
rnd_sug1 <- plm(roa ~ ad_sales + rd_sales + threat * ad_sales,
    index = c("id", "year"), model = "random",
    effect = "twoways", data = it_sector_final
)
summary(fe_sug1)
summary(rnd_sug1)
phtest(fe_sug1, rnd_sug1)


fe_sug2 <- plm(roa ~ ad_sales + threat * ad_sales
    + log(assets),
    index = c("year"), model = "within", data = it_sector_final,
    effects = "time"
)
rnd_sug2 <- plm(roa ~ ad_sales + rd_sales + threat * ad_sales
    + log(assets),
    index = c("id", "year"), model = "random",
    effect = "twoways", data = it_sector_final
)
summary(fe_sug2)
fixef(fe_sug2)
summary(rnd_sug2)
phtest(fe_sug2, rnd_sug2)

fe_sug3 <- plm(roa ~ log(1 + ad) + log(1 + rd) + threat * log(1 + ad),
    index = c("id", "year"), model = "within",
    effect = "twoways", data = it_sector_final
)
summary(fe_sug3)

fe_sug4 <- plm(earnings ~ log(1 + ad) + log(1 + rd) + threat * log(1 + ad)
    + log(assets) + log(sales),
    index = c("id", "year"), model = "within",
    effect = "twoways", data = it_sector_final
)
summary(fe_sug4)

fe_alt <- plm(
    earnings ~ ad + rd + threat * ad,
    data = it_sector_final, index = c("id", "year"), model = "within",
    effect = "twoways"
)
rnd_alt <- plm(
    earnings ~ ad + rd + threat * ad,
    data = it_sector_final, index = c("id", "year"), model = "random",
    effect = "twoways"
)
fixed_dummy <- lm(log(mv) ~ log1p(rd) + log1p(ad) + log1p(assets) + log(threat)
    + log1p(debt) + log1p(ad) * log1p(rd)
    + factor(year) - 1,
    data = it_sector_final
)
summary(fixed_dummy)

fixed_dummy3 <- lm(log(mv) ~ log1p(rd) * log1p(debt) + log(threat)
    + log1p(debt)
    + factor(year) - 1,
data = it_sector_final
)
summary(fixed_dummy3)

fixed_dummy2 <- lm(log1p(fv) ~ log1p(rd) + log1p(ad) + log1p(assets) + threat
    + log1p(debt) + threat * log1p(rd) + factor(year) - 1,
data = it_sector_final
)

cor(log1p(it_sector_final))
summary(fe_alt)
summary(rnd_alt)
phtest(fe_alt, rnd_alt)

roa_dummy <- lm(roa ~ log(1 + ad) + log(1 + rd) + threat * log(1 + ad)
    + factor(year) - 1, data = it_sector_final
)
summary(roa_dummy)