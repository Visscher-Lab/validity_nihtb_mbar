#Multiple Regression 
# Total cognition composite score
sitemodel <- lm(tb_cogtotal_uncorr_ss ~ site, data=REG2)
fit_total <- lm(tb_cogtotal_uncorr_ss ~ gender + race + age_visit + highest_academic_degree + comp_freq + site, data=REG2)
summary(fit_total) # show results
#total R2 change
rsqdeltatotal <- summary(fit_total)$r.squared - summary(sitemodel)$r.squared
rsqdeltatotal
anova(sitemodel, fit_total, test="F")

#Fluid composite score
sitemodel <- lm(tb_cogfluid_uncorr_ss ~ site, data=REG2)
fit_fluid <- lm(tb_cogfluid_uncorr_ss ~ gender + race + age_visit + highest_academic_degree + comp_freq, data=REG2)
summary(fit_fluid) # show results
rsqdeltafluid <- summary(fit_fluid)$r.squared - summary(sitemodel)$r.squared
rsqdeltafluid
anova(sitemodel, fit_fluid, test="F")

#Crystallized composite score
sitemodel <- lm(tb_cogcrystal_uncorr_ss ~ site, data=REG2)
fit_crys <- lm(tb_cogcrystal_uncorr_ss ~ gender + race + age_visit + highest_academic_degree + comp_freq, data=REG2)
summary(fit_crys) # show results
rsqdeltacrys <- summary(fit_crys)$r.squared - summary(sitemodel)$r.squared
rsqdeltacrys
anova(sitemodel, fit_crys, test="F")


# Other useful functions
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table
vcov(fit) # covariance matrix for model parameters
influence(fit) # regression diagnostics