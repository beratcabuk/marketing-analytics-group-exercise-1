library(corrplot)
library(plm)

marketing <- read.csv("Marketing Insight.csv")
summary(marketing)

it_sector <- marketing[marketing$sector == "6", ] # take the it sector

it_sector_clea <- it_sector[it_sector$mkt >= 0, ] # clean neg. mkt
it_sector_clean <- it_sector_clea[it_sector_clea$sales > 0, ] # clean zero sales
summary(it_sector_clean)

it_sector_final <- it_sector_clean
# some additional variables
it_sector_final$ad_sales <- it_sector_final$ad / it_sector_final$sales
it_sector_final$rd_sales <- it_sector_final$rd / it_sector_final$sales
it_sector_final$roa <- it_sector_final$earnings / it_sector_final$assets
summary(it_sector_final)

corr_matrix_data <- it_sector_final[
    c("mv", "ad", "rd", "assets",
    "threat", "debt")
]

summary(corr_matrix_data)

# look at the correlations matrices of our pre and post-transform variables
correlations_log <- cor(log1p(corr_matrix_data))
correlations <- cor(corr_matrix_data)
corrplot(correlations, "number") # pre
corrplot(correlations_log, "number") # post

# the model (LSDV)
fixed_dummy <- lm(log(mv) ~ log1p(ad) + log1p(rd) + log1p(assets)
                  + log(threat) + log1p(debt) + log1p(ad) * log1p(rd)
                  + factor(year) - 1, data = it_sector_final)
summary(fixed_dummy)

# ADDITIONAL MODELS AND A SAMPLE HAUSMAN TEST

# time random effects
rnd_model <- plm(log(mv) ~ log1p(ad) + log1p(rd) + log1p(assets)
                 + log(threat) + log1p(debt) + log1p(ad) * log1p(rd),
                 data = it_sector_final, effect = "time", model = "random",
                 index = c("id", "year"))
summary(rnd_model)

# time fixed effects, within estimator
fe_model <- plm(log(mv) ~ log1p(ad) + log1p(rd) + log1p(assets)
                + log(threat) + log1p(debt) + log1p(ad) * log1p(rd),
                data = it_sector_final, effect = "time", model = "within",
                index = c("id", "year"))
summary(fe_model)
fixef(fe_model)

# pooling OLS for testing purposes
pooling_model <- plm(log(mv) ~ log1p(ad) + log1p(rd) + log1p(assets)
                + log(threat) + log1p(debt) + log1p(ad) * log1p(rd),
                data = it_sector_final, model = "pooling",
                index = c("id", "year"))
summary(pooling_model)

# F test for time effects
pFtest(fe_model, pooling_model)

# Hausman Test
phtest(fe_model, rnd_model)