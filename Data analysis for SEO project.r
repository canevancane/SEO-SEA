# data analysis for SEO project
# load data
dt_full <- read.csv("/Users/vincentbecker/VSCode/SEO Project/dt_full_analysis.csv")

# remove rows with a 0 to 100 in sumRE
dt_full <- dt_full[dt_full$sumRE > 100,]

# remove rows with a 0 to 100 in nChar
dt_full <- dt_full[dt_full$nChar > 100,]

# only keep rows with 0 in nError
dt_full <- dt_full[dt_full$nError == 0,]

# transform nChar to pixles
dt_full$nChar <- dt_full$nChar * 1.333 * 12

# Ratio Function
calculate_ratio <- function(a, b) {
  if (a == b) {
    return(0)
  } else if (a > b) {
    return(((a) / (a + b) - 0.5))
  } else {
    return((-((b) / (a + b)) + 0.5))
  }
}

#View(dt_full)

# define vector ratio
ratio <- c()

# loop through dt_full and calculate ratio
for (i in 1:nrow(dt_full)) {
    rt <- calculate_ratio(dt_full$sumRE[i], dt_full$nChar[i])
    # add rt to verctor ratio
    ratio <- c(ratio, rt)
}

# add ratio to dt_full
dt_full$ratio <- ratio

library(psych)
describe(dt_full)

#################### DATA ANALYSIS ####################
model1 <- lm(rank ~ ratio + I(ratio^2) + loadTime + keyword, data = dt_full)
summary(model1)

### Testing Assumptions
## cut standardized residuals above 3
dt_full <- dt_full[abs(rstandard(model1)) < 3,]

## coocks distance
dt_full$CooksD <- round(cooks.distance(model1), 5)
hist(dt_full$CooksD,
     breaks = 10,
     main = "Cook's Distance",
     xlab = "Cook's Distance")

## cut above 0.006
dt_full <- dt_full[dt_full$CooksD < 0.006,]

### Rerun Model
model2 <- lm(rank ~ ratio + I(ratio^2) + loadTime + keyword, data = dt_full)
summary(model2)

### Linear Model Alternative
model3 <- lm(rank ~ ratio + loadTime + keyword, data = dt_full)
summary(model3)

### compare model 2 and 3
anova(model2, model3)

plot(dt_full$ratio, dt_full$rank)
abline(lm(rank ~ ratio, data = dt_full), col = "red")
