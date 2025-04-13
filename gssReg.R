library(haven)
library(tidyverse)

# load data from previous course
gss <- read_sav("data_assignment_6.sav") %>% 
  filter(YEAR == 2004)

# remove all columns that only have NA (which is over 5000)
gss <- gss[,colSums(is.na(gss)) < nrow(gss)]

# logistic regression: regress whether or not the first alter on social network list is family on age and education

# make the columns kin1 to kin5 and put them into gss data subset
kin_prefix <- c("SPOUSE", "PARENT", "SIBLING", "CHILD", "OTHFAM")

for (i in 1:5) {
  cols <- paste0(kin_prefix, i)
  
  gss[[paste0("kin", i)]] <- apply(gss[, cols], 1, function(values) {
    # 1 if any is yes
    if (any(values == 1, na.rm = TRUE)) {
      return(1)
      # 0 there is no yes kin and at least some are valid (=2)
    } else if (any(values == 2, na.rm = TRUE)) {
      return(0)
      # NA if all NA
    } else if (all(is.na(values))) {
      return(NA)
    }
  })
}

# making and viewing logistic regression
kinReg <- glm(kin1 ~ AGE + EDUC, family = "binomial", data = gss)
summary(kinReg)

# covariance matrix
(kinCov <- vcov(kinReg) %>% round(3))
