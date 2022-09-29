library(plm)

marketing <- read.csv("Marketing Insight.csv")
summary(marketing)
str(marketing)

it_sector <- marketing[marketing$sector == "6", ]
summary(it_sector) #check data integrity
str(it_sector)

# lm(earnings ~ ad, ad^2) can also be used
# lags of variables?
# control variables: size (usually log(asset)),
# ad can be autocorrelated with earnings, i.e. ad budget set to a certain
# % of earnings
preliminary_regression <- lm(earnings ~ ad, data = marketing)
summary(preliminary_regression)

#interaction terms
inter_lm <- lm(earnings ~ ad * threat, data = marketing)
summary(inter_lm)

#gls?
prelim_gls <- glm(earnings ~ ad, data = marketing)
summary(prelim_gls)

#year dummies
# factor - 1?
fixed_dum <- lm(earnings ~ ad + factor(year) - 1, data = marketing)
summary(fixed_dum)

#fixed effect
fixed <- plm(earnings ~ ad, data = marketing, index = "year", model = "within")
summary(fixed)
fixef(fixed)

#random effect
random <- plm(earnings ~ ad, data = marketing, index = "year", model = "random")
summary(random)

#hausman test
phtest(fixed, random)

#lecture examples
lecture <- lm(sales ~ ad * rd, data = marketing)
summary(lecture)
