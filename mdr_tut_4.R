library(faraway)
library(olsrr)
data("teengamb")
teengamb
model = lm(gamble~., data = teengamb)
summary(model)

#best subset selection

fit = ols_step_all_possible(model)
fit
result = fit[['result']]
head(result)
result

c_1 = result$cp - result$n
which(c_1 == min(c_1))
result$predictors[11]

a_1 = result$aic
which(a_1 == min(a_1))

#comment: best model has predictors "sex", "income", "verbal"

#forward subset selection

fit1 = ols_step_forward_adj_r2(model)
result1 = fit1[["metrics"]]
result1

#backward subset selection

fit2 = ols_step_backward_aic(model)
result2 = fit1[["metrics"]]
result2
fit2$model

#stepwise subset selection

fit3 = ols_step_both_aic(model)

result = fit3[["metrics"]]
fit3$model

#comment: Best model has predictor "income, sex, verbal"

#Best model fit


fit4 = lm(gamble ~ income + sex + verbal, data = teengamb)
summary(fit4)

# future data: income = 10,sex = 0, verbal = 5
# To predict the mean response

future_dat = data.frame(income = 10, sex = 0, verbal = 5)

predict(fit4, future_dat, interval = "confidence")
predict(fit4, future_dat, interval = "prediction")
