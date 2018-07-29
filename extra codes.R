#Stuff I wrote but useless for project

# I made numerical categorical variables because R gave me a weird bug
# when I test 3-way interaction model with character variables with ":".
malI = rep(0, 12)
surI = rep(0, 12)
deadI = rep(0,12) # dead = survivedNo
fiftysixtynineI = rep(0,12)
seventyplusI = rep(0,12)
for (i in 1:12) {
  if (malignantP[i] == 'yes') {
    malI[i] = 1}
  if (survivedP[i] == 'yes') {
    surI[i] = 1}
  if (ageP[i] == '50-69') {
    fiftysixtynineI[i] = 1}
  if (ageP[i] == '70+') {
    seventyplusI[i] = 1}
  if (survivedP[i] == 'no') {
    deadI[i] = 1}
}


# Interaction model 1 has all interactions with deadI = 1 (survivedP = "no")
pois_int1 = glm(count ~ fiftysixtynineI+seventyplusI+malI+deadI
                +fiftysixtynineI:deadI+seventyplusI:deadI
                +malI:deadI
                +fiftysixtynineI:malI:deadI+seventyplusI:malI:deadI, family="poisson")
summary(pois_int1)

# GOF of interaction model 1 with res.deviance = 0.30885, df = 2
pchisq(0.30885, 2, lower.tail = FALSE)



# Interaction model 1 has all interactions with surI = 1 (survivedP = "yes")
pois_int1 = glm(count ~ fiftysixtynineI+seventyplusI+malI+surI
                +fiftysixtynineI:surI+seventyplusI:surI
                +malI:surI
                +fiftysixtynineI:malI:surI+seventyplusI:malI:surI, family="poisson")
summary(pois_int1)
anova(pois_int1, test="Chisq")

# GOF of interaction model 1 with res.deviance = 0.62707, df = 2
pchisq(0.62707, 2, lower.tail = FALSE)

# Interaction model 2: 3-way interactions are dropped because Wald test results
#                       are insiginificant for fiftysixtynineI:malI:surI 
#                       and seventyplusI:malI:surI
pois_int2 = glm(count ~ fiftysixtynineI+seventyplusI+malI+surI
                +fiftysixtynineI:surI+seventyplusI:surI
                +malI:surI, family="poisson")
summary(pois_int2)
anova(pois_int2, test="Chisq")

# Multiple contrasts for H0: Does age matter to survival?
L1 <- matrix(c(0,1,0,0,0,1,0,0,
               0,-1,1,0,0,-1,1,0,
               0,1,0,0,0,0,0,0,
               0,-1,1,0,0,0,0,0), nrow = 4, byrow = T)
require(multcomp)
summary(glht(pois_int2, L1), test=adjusted("bonf"))

# Multiple contrasts for H0: Does malignant matter to survival?
L2 <- matrix(c(0,0,0,1,0,0,0,1,
               0,0,0,1,0,0,0,0), nrow = 2, byrow = T)
summary(glht(pois_int2, L2), test=adjusted("bonf"))

pois_full = glm(count ~ fiftysixtynineI+seventyplusI+malI+surI
           +fiftysixtynineI:malI+seventyplusI:malI
           +fiftysixtynineI:surI+seventyplusI:surI
           +malI:surI
           +fiftysixtynineI:malI:surI+seventyplusI:malI:surI, family="poisson")
summary(pois_full)