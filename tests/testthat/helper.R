rm(list = ls())


### simple example of a DCE with 3 attributes and 3 levels each

df <- expand.grid(a = c(1,2,3), b = c(1,2,3),c = c(1,2,3))

df <- df[rep(seq_len(nrow(df)), times = 20), ]

df$ID <- rep(1:20, each = 27)

u=list( list(
  V1 = V.1~ b_a*a + b_b*b + b_c*c,
  V2 = V.2~ 0
))

b = list(
  b_a = 0.4,
  b_b = -0.3,
  b_c = 0.2
)

df3att3lev <- simulateDCE::simulate_choices(data = df,utility = u,bcoeff = b, setspp = 20) 
                 

# same data but with latent class structure



