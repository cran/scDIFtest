## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----instalation, eval = FALSE------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("ddebeer/scDIFtest")

## ----get-data, eval = FALSE---------------------------------------------------
#  install.packages("psychotree", quiet = TRUE)
#  data("SPISA", package = "psychotree")

## ----get-data_2, eval = TRUE--------------------------------------------------
data("SPISA", package = "psychotree")

## ----get-responses------------------------------------------------------------
resp <- as.data.frame(SPISA$spisa[,37:45])

## ----get-summary--------------------------------------------------------------
summary(SPISA[,2:6])

## ----get-mirt-----------------------------------------------------------------
library(mirt, quietly = TRUE)

## ----fit-models---------------------------------------------------------------
fit_2PL <- mirt(data = resp, 
                model = 1, 
                itemtype = "2PL", 
                verbose = FALSE)
fit_multiGroup <- multipleGroup(
  data = resp, model = 1,  
  group = SPISA$gender,
  invariance = c("free_means", 
                 "slopes", 
                 "intercepts", 
                 "free_var"),
  verbose = FALSE)

## ----anova--------------------------------------------------------------------
anova(fit_2PL, fit_multiGroup)

## ----dif-gender---------------------------------------------------------------
library(scDIFtest)
DIF_gender <- scDIFtest(fit_multiGroup, DIF_covariate = SPISA$gender) 

## ----print-dif-gender---------------------------------------------------------
DIF_gender

## ----print-dif-gender-selection-----------------------------------------------
print(DIF_gender, item_selection = c("V4", "V7"))

## ----dif-age------------------------------------------------------------------
DIF_age <- scDIFtest(fit_multiGroup, DIF_covariate = SPISA$age)
summary_age <- summary(DIF_age)
summary_age

## ----dif-spon-----------------------------------------------------------------
DIF_spon <- scDIFtest(fit_multiGroup, DIF_covariate = SPISA$spon)
DIF_spon

