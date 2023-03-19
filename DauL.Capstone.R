#Tweet count aggregated by issue and county
df<- read.csv("C:/Users/dauho/Downloads/0315_county_issues_transpose.csv")
df[is.na(df.t)] <- 0
df['total.count'] = df$abortion+df$inflation+df$unemployment+df$wage
ga = df[df$state=='Georgia',]

#Georgia voter turnout
vt <- read.csv("C:/Users/dauho/OneDrive/Desktop/GA_voter_turnout_2022.csv")
vt$county = paste(vt$County, 'County')


#Georgia population
pop <- read.csv("C:/Users/dauho/Downloads/county_pop_char.csv")
ga.pop = pop[pop$state== 'Georgia',]

#Merged dataset
ga_vt <- merge(ga,vt.df,by="county")
ga.vt <-merge(ga_vt,ga.pop,by="county")
attach(ga.vt)

#Selecting best predictors
results2 = regsubsets(Voter.Turnout~p_f+p_h+X20.29y+p_wa+p_ba+p_na+log(total.count)+X15.19y+p_ia+p_aa, method="exhaustive", data=ga.vt)
bics =summary(results2)$bic
rsqs = summary(results2)$rsq
id_bic =which.min(bics)
id_rsqs= whic.max(rsqs)

summary(results2)$which[id_bic,]
summary(results2)$which[id_rsqs,]

b1 = lm(Voter.Turnout~X20.29y+p_ia)
BIC(b1)
summary(b1)

#Check for assumptions
#equal variance
plot(resid(based)~fitted(based), main = "Residual Plot for Best Model",
     ylab = "Residuals", xlab = "Fitted Values")
abline(h = 0)

#linearity
qqnorm(resid(based))
qqline(resid(based), col = "red")



