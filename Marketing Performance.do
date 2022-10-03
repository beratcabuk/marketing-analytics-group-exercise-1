
* Open Marketing Insight.dta

* To check the summary statistics
sum sales ad rd mkt

/*
    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
       sales |     22,723    3125.406    16117.71          0     511729
          ad |     22,723    73.34095    328.5978          0      11000
          rd |     22,723    139.7114     900.056          0      35931
         mkt |     22,723    566.9216    2902.017      -3728     103457
*/

* surprisingly, there is negative value in mkt.
* to check the details
sum mkt, det
* sometimes in the data there are errors.
* in this case, you can eliminate the observations.
* because it is impossible for firms to have negative marketing expenditure.

/* Panel Data Regression */

*Declare Panel Data
xtset id year /*entity identifier comes first followed by time indicator*/

/* Hausman Test */

* Null hypothesis: Random Effect(RE) is preferred.
* Basically, it tests whether the assumption of RE holds or not.
* The assumption is the firm specific effect is not correlated with X variables.
* Fixed Effect (FE) does not impose this assumtion and allows the correlation between the firm specific unobservable factor is correlated with X variables.
* If this assumption holds, there should be no difference betweeen the estimates from FE and RE.
* If this assumption does not hold, then there should be difference between the estimates from FE and RE.
* If Hausman test gives a significant result, then we reject the null hypothesis.
* This means we are supposed to use FE model to take into account the panel.

*Fixed Effects Model
xtreg fv ad, fe
estimates store fe

*Random Effects Model
xtreg fv ad, re
estimates store re

// Hausman Test
hausman fe re, sigmamore

* variable construction 1
* usually we use the log-transformed sales and total assets in the model to address the skewness of the distribution.
gen lsales=log(sales)
* scaling the variables
gen ads=ad/sales
gen rds=rd/sales
gen mkts=mkt/sales
gen size=log(assets)
* debt to total asset ratio
gen dta=debt/assets
* ROA
gen roa=earnings/assets

* variable construction 2
* market share
* the following code means we generate total sales for each sector in each year.
* please recall using "egen" allows for built-in functions and we use sum to generate total sales for each sector in each year.
by sector year, sort: egen sales_sector=sum(sales)
gen ms=sales/sales_sector

* HHI
* sum of the squares of market shares for all firms in the sector.
* for the formula, please refer to the slides in session 2.
gen mssq=ms*ms
by sector year, sort: egen hhi=sum(mssq)

* Time operator
* Once you declare time series data or panel data, you are able to use time operator.
* if you use l.variable (i.e., time lag), then it indicates the variable at time t-1.
* similarly, l2.variable indicates the variable at time t-2.
* in this data, time is year.

* market growth
* After you use sort option, then you need to rearragne the data and need to declare the panel structure again.
xtset id year
gen mgrowth=log(sales_sector)-log(l.sales_sector)

*Fixed Effects Model
xtreg fv ads size dta hhi mgrowth, fe
estimates store fe

*Random Effects Model
xtreg fv ads size dta hhi mgrowth, re
estimates store re

// Hausman Test
hausman fe re

* if you have a problem non-positive definite matrix
* use the followiong
hausman fe re, sigmamore

* example of a model with the interaction term: a moderating effect
xtreg fv ads c.ads#c.size size dta hhi mgrowth, fe

sum size, det
* to draw the margins plot, use the values from the distribution of the moderating variable
/*
                            size
-------------------------------------------------------------
      Percentiles      Smallest
 1%     .9415686      -5.521461
 5%     2.083309      -4.961845
10%     2.756586       -2.60369       Obs              29,889
25%     3.982463       -1.80181       Sum of Wgt.      29,889

50%     5.439174                      Mean           5.585185
                        Largest       Std. Dev.      2.269874
75%     7.056844       13.05775
90%     8.614684       13.06835       Variance       5.152327
95%     9.616312       13.18414       Skewness       .2915737
99%     11.36342        13.2207       Kurtosis       2.932517
*/

* One way of extrapolating marginal effects
* you can use approximate values (round up/down)
margins, dydx(ads) at(size=(.941 2.083 2.756 3.982 5.439 7.056 8.614 9.616 11.363))

marginsplot
