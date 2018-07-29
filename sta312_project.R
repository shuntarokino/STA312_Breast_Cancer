cancer = read.csv("C:/Users/Shuntaro/Documents/2015-16 files/STA312/data/breastcancerdata.csv")
cancer
attach(cancer)

### Binomial Model ###

survived = survived.yes
dead = survived.no
total = survived + dead
prop_death = dead / total
bin_sat = glm(dead / total ~ age * malignant, weight = total, family = binomial)
bin_noint = glm(dead / total ~ age + malignant, weight = total, family = binomial)
# Saturated Model
summary(bin_sat)
anova(bin_sat, test="Chisq")
# Additive Model
summary(bin_noint)
anova(bin_noint, test="Chisq")
# GOF of additive model with res.deviance = 0.49409, df =2
pchisq(0.49409, 2, lower.tail = FALSE)
# B1 v.s. B2
L <- matrix(c(0,1,-1,0), nrow = 1, byrow = T)
summary(glht(bin_noint, L), test=adjusted("bonf"))
# plots
plot(age, survived/total)
plot(malignant, survived/total)
plot(age,exp(bin_noint$fit)/(1+exp(bin_noint$fit)), xlab="Age", ylab="Estimated prob.", main ="Estimated Prob. v.s. Age")
plot(malignant,exp(bin_noint$fit)/(1+exp(bin_noint$fit)), xlab="Malignancy", ylab="Estimated prob.", main ="Estimated Prob. v.s. Malignancy")
interaction.plot(age, malignant, dead/total)
ggplot(cancer, aes(x = age, y = prop_death, colour = malignant)) +
  geom_line(data = cancer, aes(y = prop_death, group = malignant)) +
  theme_bw()
qplot(prop_death, age, data=cancer, geom=c("smooth"))
# Estimates
# 1. prob. of death for <50 and malignantY
odds1=exp(-2.0730+0.7328)
odds1/(1+odds1)
# 2. prob. of death for 50-69 and malignantY
odds2=exp(-2.0730+0.6318+0.7328)
odds2/(1+odds2)
# 3. prob. of death for 70+ and malignantY
odds3=exp(-2.0730+0.9282+0.7328)
odds3/(1+odds3)
# 4. prob. of death for <50 and malignantN
odds4=exp(-2.0730)
odds4/(1+odds4)
# 5. prob. of death for 50-69 and malignantN
odds5=exp(-2.0730+0.6318)
odds5/(1+odds5)
# 6. prob. of death for 70+ and malignantN
odds6=exp(-2.0730+0.9282)
odds6/(1+odds6)

### Pearson Chi Square Test of Independence ###

# Age v.s. Malignant
table1 = rbind(c(64,87),c(58,62),c(9,10))
table1
chisq.test(table1, correct = FALSE)

# Age v.s. Survived
table2 = rbind(c(128,23),c(89,31),c(13,6))
table2
chisq.test(table2, correct = FALSE)
# I got a warning message because one of the sample had small size (=6)

# Malignant v.s. Survived
table3 = rbind(c(95,36),c(135,24))
table3
chisq.test(table3, correct = FALSE)

### Multinomial Likehood Ratio Test ###
require(Deducer)

# Age v.s. Malignant
likelihood.test(table1)

# Malignant v.s. Survived
likelihood.test(table2)

# Age v.s. Survived
likelihood.test(table3)

### Poisson Model ###

# Create data with 12 counts and 3 explanatory variables 
# (ageP, malignantP, survivedP)

fiftyless = rep('<50', 4)
fiftysixtynine = rep('50-69', 4)
seventyplus = rep('70+', 4)
ageP = c(fiftyless, fiftysixtynine, seventyplus)

sur = c('yes', 'no')
survivedP = rep(sur, 6)

dea = c('no', 'yes')
deadP = rep(dea, 6)

mal = c('no', 'no', 'yes', 'yes')
malignantP = rep(mal, 3)

count = rep(0, 12)
for (i in 1:12) {
  if (i %% 2 == 1) {
    count[i] = survived.yes[(i+1)/2]} 
  else {
    count[i] = survived.no[i/2]}
}

# Additive Model
pois_add = glm(count ~ ageP+malignantP+survivedP, family="poisson")
summary(pois_add)

# Saturated Model
pois_sat = glm(count ~ ageP*malignantP*survivedP, family="poisson")
summary(pois_sat1)
anova(pois_sat, test="Chisq")

# GOF of additive model with res.deviance = 13.651, df = 7
pchisq(13.651, 7, lower.tail = FALSE)


# Interaction model 2: 3-way interactions are dropped because Wald test results
#                       are very insiginificant for fiftysixtynineI:malI:deadI 
#                       and seventyplusI:malI:deadI
pois_int2 = glm(count ~ fiftysixtynineI+seventyplusI+malI+deadI
                 +fiftysixtynineI:deadI+seventyplusI:deadI
                 +malI:deadI, family="poisson")
summary(pois_int2)

# GOF of interaction model 2 with res.deviance = 0.93591, df = 4
pchisq(0.93591, 4, lower.tail = FALSE)

# Interaction model 2 with character variables to perform Deviance GOF test
pois_int2a = glm(count ~ ageP+malignantP+deadP+ageP:deadP+malignantP:deadP, family="poisson")
summary(pois_int2a)

# Deviance GOF test
anova(pois_int2a, test="Chisq")

#Plots
plot(ageP,exp(pois_int2a$fit)/(1+exp(pois_int2a$fit)), xlab="Age", ylab="Estimated count", main ="Estimated Prob. v.s. Age")
