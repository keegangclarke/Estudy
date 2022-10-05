library(magrittr)
library(LambertW)

df <- read.csv("C:/Users/Keegan/Desktop/staging/df_t.csv")

samp1 <- df$Open
samp2 <- df$intra_t

mod1 <- IGMM(samp1, "hh") %>% get_input
mod2 <- IGMM(samp2, "hh") %>% get_input

test_normality(samp1)
test_normality(mod1)
