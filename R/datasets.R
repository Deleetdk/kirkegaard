### This file contains some example datasets used by the SAC functions
#they are from "Some methods for analyzing and correcting for spatial autocorrelation" (Kirkegaard, 2015)

# Ex 5 --------------------------------------------------------------------
#false cause scenario, no clusters
n=500
set.seed(2)
d_ex5 = data.frame(x = runif(n, 1, 100),
                   y = runif(n, 1, 100),
                   SACV = rnorm(n))

d_ex5 = add_SAC(d_ex5, iter = 20, k=10, vars = c("SACV"), lat_var = "x", lon_var = "y", distance_method = "euclidean")

set.seed(3)
d_ex5$predictor = rnorm(n) * .73 + scale(d_ex5$SACV) %>% as.vector
set.seed(4)
d_ex5$outcome = rnorm(n) * .73 + scale(d_ex5$SACV) %>% as.vector
cor(d_ex5)["predictor", "outcome"]

# Ex 6 --------------------------------------------------------------------
#true cause scenario, no clusters
n=500
set.seed(2)
d_ex6 = data.frame(x = runif(n, 1, 100),
                   y = runif(n, 1, 100),
                   SACV = rnorm(n))

d_ex6 = add_SAC(d_ex6, iter = 20, k=10, vars = c("SACV"), lat_var = "x", lon_var = "y", distance_method = "euclidean")

set.seed(3)
d_ex6$predictor = + rnorm(n) + scale(d_ex6$SACV) %>% as.vector
set.seed(4)
d_ex6$outcome = (rnorm(n) + scale(d_ex6$predictor) * .82) %>% as.vector
cor(d_ex6)["predictor", "outcome"]


# Ex 7 --------------------------------------------------------------------
#this is a variant of ex6, but where SAC is also induced into the other main vars afterwards
n=500
set.seed(2)
d_ex7 = data.frame(x = runif(n, 1, 100),
                   y = runif(n, 1, 100),
                   SACV = rnorm(n))

d_ex7 = add_SAC(d_ex7, iter = 10, k=10, vars = c("SACV"), lat_var = "x", lon_var = "y", distance_method = "euclidean")

set.seed(3)
d_ex7$predictor = + rnorm(n)  + scale(d_ex7$SACV) %>% as.vector
set.seed(4)
d_ex7$outcome = (rnorm(n) * 3 + scale(d_ex7$predictor)) %>% as.vector

tmp = d_ex7$SACV

d_ex7 = add_SAC(d_ex7, iter = 10, k=10, vars = c("outcome", "predictor"), lat_var = "x", lon_var = "y", distance_method = "euclidean")
d_ex7$SACV = tmp

cor(d_ex7) %>% round(2)


# Ex 8 --------------------------------------------------------------------
#the point of this is to try a case where the outcome is determined by distance to the center
n=500
set.seed(2)
d_ex8 = data.frame(x = runif(n, 1, 100),
                   y = runif(n, 1, 100))
#make one point the central node @ 50,50
d_ex8[n+1, "x"] = 50;d_ex8[n+1, "y"] = 50;
d_ex8$predictor = get_euclidean_dists(d_ex8, output = "matrix")[n+1, ] %>% scale %>% as.vector
d_ex8$outcome = d_ex8$predictor + rnorm(n+1) #with noise
d_ex8 = d_ex8[-(n+1), ] #remove excess case
cor(d_ex8)

# Ex 9 --------------------------------------------------------------------
#the point of this is to try a case where the outcome is determined by distance to the center of the topright quadrant
n=500
set.seed(2)
d_ex9 = data.frame(x = runif(n, 1, 100),
                   y = runif(n, 1, 100))
#make one point the central node @ 50,50
d_ex9[n+1, "x"] = 75;d_ex9[n+1, "y"] = 75;
d_ex9$predictor = get_euclidean_dists(d_ex9, output = "matrix")[n+1, ] %>% scale %>% as.vector
d_ex9$outcome = d_ex9$predictor + rnorm(n+1) #with noise
d_ex9 = d_ex9[-(n+1), ] #remove excess case
cor(d_ex9)

# Ex 10 --------------------------------------------------------------------
#the point of this is to try a case where the outcome is determined by distance to topright corner
n=500
set.seed(2)
d_ex10 = data.frame(x = runif(n, 1, 100),
                    y = runif(n, 1, 100))
#make one point the central node @ 50,50
d_ex10[n+1, "x"] = 100;d_ex10[n+1, "y"] = 100;
d_ex10$predictor = get_euclidean_dists(d_ex10, output = "matrix")[n+1, ] %>% scale %>% as.vector
d_ex10$outcome = d_ex10$predictor + rnorm(n+1) #with noise
d_ex10 = d_ex10[-(n+1), ] #remove excess case
cor(d_ex10)

# Ex 11 --------------------------------------------------------------------
#no SAC induced, random values
n=500
set.seed(2)
d_ex11 = data.frame(x = runif(n, 1, 100),
                    y = runif(n, 1, 100))
d_ex11$predictor = rnorm(n)
d_ex11$outcome = rnorm(n)
cor(d_ex11)

# Ex 12 --------------------------------------------------------------------
#perfect anti SAC in grid form
d_ex12 = data.frame(expand.grid(seq(1, 100, length.out = 25), seq(1, 100, length.out = 25)))
n=nrow(d_ex12)
colnames(d_ex12) = c("x", "y")
d_ex12$predictor = rep(c(rep(0:1, length.out=25), rep(1:0, length.out=25)), length.out=n)
d_ex12$outcome = rep(0:4, length.out=n)
cor(d_ex12)
