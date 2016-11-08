## ---- echo=FALSE---------------------------------------------------------
suppressPackageStartupMessages(library(tidytopics))
suppressPackageStartupMessages(library(tidytext))
suppressPackageStartupMessages(library(dplyr))

## ------------------------------------------------------------------------
library(tidytopics)
library(tidytext)
library(dplyr)

data("sotu50")
sotu50
is.tidy_topic_state(sotu50)

## ------------------------------------------------------------------------
top_terms(sotu50, "type_probability")

## ------------------------------------------------------------------------
nw <- topic_type_matrix(sotu50)
nw
nd <- doc_topic_matrix(sotu50)
nd

## ------------------------------------------------------------------------
is.tidy_topic_matrix(nw)
is.tidy_topic_array(nw)

data("sotu")
nd <- left_join(nd, sotu[,c("doc" )], by = "doc")

is.tidy_topic_matrix(nd)
is.tidy_topic_array(nd)

## ------------------------------------------------------------------------
nw <- as.sparseMatrix(nw)
nw[1:10, 1:5]

## ---- echo=FALSE---------------------------------------------------------
data("sotu50")
my_sotu <- sotu50
alpha <- c(0.02136692 ,0.01317826 ,0.03399012 ,0.04877238 ,0.05565182 ,0.02634909 ,0.07547259 ,0.1090051 ,0.0343327 ,0.1987157 ,0.01995965 ,0.1032003 ,0.0476333 ,0.02253602 ,0.1115289 ,0.01482319 ,0.01683189 ,0.04114055 ,0.0393674 ,0.01632598 ,0.02016105 ,0.03419158 ,0.2109707 ,0.02525466 ,0.06465434 ,0.02042515 ,0.04062194 ,0.04762343 ,0.04169344 ,0.05274674 ,0.02347339 ,0.02688192 ,0.01971497 ,0.08863154 ,0.02162647 ,0.08386104 ,0.01468114 ,0.01798629 ,0.04029905 ,0.05446011 ,0.02651762 ,0.03284185 ,0.02257509 ,0.06026494 ,0.07507902 ,0.07178521 ,0.03777182 ,0.0702841 ,0.02130745 ,0.0345557)
beta <- 0.01228582

## ---- eval=FALSE---------------------------------------------------------
#  my_sotu <- read_mallet_statefile("sotu50.txt.gz")
#  alpha <- read_mallet_statefile("sotu50.txt.gz", type = "alpha")
#  beta <- read_mallet_statefile("sotu50.txt.gz", type = "beta")

## ---- eval=TRUE----------------------------------------------------------
my_sotu
alpha[1:5]
beta

## ---- eval=FALSE---------------------------------------------------------
#  write_mallet_statefile(state = my_sotu,
#                         alpha = alpha,
#                         beta = beta,
#                         state_file = "my_mallet_state.txt.gz")

## ----sessioninfo, message=FALSE, warning=FALSE---------------------------
sessionInfo()

