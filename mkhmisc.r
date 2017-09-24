sub1 <- help.spss %>%
  select(age,female,racegrp,homeless,a15a,a15b,
         d1,e2b,g1b,i1,i2,pcs,mcs,cesd,treat,pss_fr)

summary(sub1)

table(as_factor(help.spss$female))
table(as_factor(help.spss$racegrp))
table(as_factor(help.spss$homeless))
table(help.spss$a15a)
table(help.spss$a15b)
table(help.spss$d1)
table(help.spss$e2b)
table(as_factor(help.spss$g1b))

attributes(sub1$age)$label

# create a function to get the label
# label output from the attributes() function
getlabel <- function(x) attributes(x)$label
getlabel(sub1$age)

library(purrr)
ldf <- purrr::map_df(sub1, getlabel) # this is a 1x15 tibble data.frame
t(ldf) # transpose for easier reading to a 15x1 single column list

# using knitr to get a table of these
# variable names for Rmarkdown
library(knitr)
knitr::kable(t(ldf),
             col.names = c("Variable Label"))