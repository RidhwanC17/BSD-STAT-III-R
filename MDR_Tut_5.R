library(faraway)
attach(coagulation)
summary(coagulation)
plot(coag~diet, data = coagulation)
#taking mu = 0
fit1 = lm(coag~diet-1, data = coagulation)
summary(fit1)

null_model = lm(coag~1, data=coagulation)
anova(null_model, fit1)

#second: sum_alpha = 0

fit2 = lm(coag~diet, data=coagulation)
round(fit2$coefficients,2)
anova(fit2)

#third: sum_alpha = 0 
options(contrasts = c("contr.sum", "contor.poly")) #changing the global setting

fit3 = lm(coag~diet, data = coagulation)
summary(fit3)

#restore the default global setting
options(contrasts = c("contr.treatment", "contr.poly"))

