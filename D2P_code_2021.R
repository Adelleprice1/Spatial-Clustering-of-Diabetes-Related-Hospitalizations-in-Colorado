#load diabetes hospitalization rate data
shape <- sf::st_read("Diabetes_Hospitalization_Rate_(Census_Tracts)/Diabetes_Hospitalization_Rate_(Census_Tracts).shp")
suppressPackageStartupMessages(library(GISTools))
population <- sf::st_read("Population_Density_(Census_Tracts)/Population_Density_(Census_Tracts).shp")
plot(population$geometry, border="grey60")

select_shape <- na.omit(shape)
choropleth(select_shape$geometry, select_shape$DIABETES_A, axes = TRUE)
shades = auto.shading(select_shape$DIABETES_A)
choro.legend(px='bottomleft', sh=shades, fmt="%4.1f")
title(main = "Diabetes-Related Hospitalization Rate in Colorado Census Tracts")
      
choropleth(select_shape$geometry, select_shape$DIABETES_A, axes = TRUE, xlim = range(-104.5,-105.5), ylim = c(39.4,40.4))
shades = auto.shading(select_shape$DIABETES_A)

choropleth(select_shape$geometry, select_shape$DIABETES_A, axes = TRUE, xlim = range(-104.5,-105.5), ylim = c(38,39))
shades = auto.shading(select_shape$DIABETES_A)

choropleth(select_shape$geometry, select_shape$DIABETES_A, axes = TRUE, xlim = range(-108, -109), ylim = c(38,40))
shades = auto.shading(select_shape$DIABETES_A)

#merge diabetes data with population data
diabetes_pop <- merge(as.data.frame(select_shape), as.data.frame(population), by.x = "TRACT_FIPS", by.y = "Tract_FIPS")
diabetes_pop$geometry.y <- NULL

library(sf)
newshape <- sf::st_sf(diabetes_pop)
#make geography non-scalar
newshape <- st_transform(newshape, "+init=epsg:32613")
p2 <- st_point_on_surface(newshape)
newshape <- as.data.frame(newshape)
newshape$counts = (newshape$DIABETES_A/100000)*newshape$Population
p2 <- as.data.frame(p2)
newshape$centroids <- p2$geometry.x
library(tidyverse)
separated_coord <- newshape %>%
    mutate(latitude = unlist(map(newshape$centroids,1)),
           longitude = unlist(map(newshape$centroids,2)))
newshape2 <- sf::st_sf(separated_coord)
plot(newshape2$geometry.x, border="grey60")
points(newshape2$latitude, newshape2$longitude, col = 'red', pch = 19)


suppressPackageStartupMessages(library(smerc))
suppressPackageStartupMessages(library(spdep))

sum(newshape2$Population)



coords = as.data.frame(cbind("x" = newshape2$latitude, "y" = newshape2$longitude)) 
cases = (newshape2$counts)
cepp16000 = cepp.test(coords = coords, 
                     cases = cases, 
                     pop = newshape2$Population, 
                     nstar = 16000, 
                     alpha = 0.01) 
 
cepp20000 = cepp.test(coords = coords, 
                     cases = cases, 
                     pop = newshape2$Population,  
                     nstar = 20000, 
                     alpha = 0.01) 
 
cepp30000 = cepp.test(coords = coords, 
                      cases = cases, 
                      pop = newshape2$Population,  
                      nstar = 30000, 
                      alpha = 0.01)

summary(cepp16000) 
summary(cepp20000) 
summary(cepp30000)




cepp1000000 = cepp.test(coords = coords, 
                      cases = cases, 
                      pop = newshape2$Population,  
                      nstar = 1000000, 
                      alpha = 0.01)
summary(cepp1000000)

plot(newshape2$geometry.x, border = "grey60", axes = TRUE, 
     col = color.clusters(cepp1000000))
plot(newshape2$geometry.x, border = "grey60", axes = TRUE, 
     col = color.clusters(cepp1000000), xlim = range(450000:600000), ylim = c(4350000, 4500000))

plot(newshape2$geometry.x, border = "grey60", axes = TRUE, 
     col = color.clusters(cepp1000000), xlim = range(450000:600000), ylim = c(4350000, 4500000))

cepp.05 = cepp.test(coords = coords, 
                      cases = cases, 
                      pop = newshape2$Population,  
                      nstar = .05*sum(newshape2$Population), 
                      alpha = 0.01)
summary(cepp.05)

plot(newshape2$geometry.x, border = "grey60", axes = TRUE, 
     col = color.clusters(cepp.05))
plot(newshape2$geometry.x, border = "grey60", axes = TRUE, 
     col = color.clusters(cepp.05), xlim = range(450000:600000), ylim = c(4350000, 4500000))

plot(newshape2$geometry.x, border = "grey60", axes = TRUE, 
     col = color.clusters(cepp16000)) 
legend("topright", legend = c("n* = 16000")) 
plot(newshape2$geometry.x, border = "grey60", axes = TRUE, 
     col = color.clusters(cepp16000), xlim = range(450000:600000), ylim = c(4350000, 4500000)) 
legend("topright", legend = c("n* = 16000")) 
plot(newshape2$geometry.x, border = "grey60", axes = TRUE, 
     col = color.clusters(cepp16000), xlim = range(450000:600000), ylim = c(4250000, 4350000)) 
legend("topright", legend = c("n* = 16000")) 

plot(newshape2$geometry.x, border = "grey60", axes = TRUE, 
     col = color.clusters(cepp20000)) 
legend("topright", legend = c("n* = 20000")) 
plot(newshape2$geometry.x, border = "grey60", 
     col = color.clusters(cepp20000), xlim = range(450000:600000), ylim = c(4350000, 4500000)) 
plot(newshape2$geometry.x, border = "grey60", 
     col = color.clusters(cepp20000), xlim = range(450000:600000), ylim = c(4250000, 4320000)) 

plot(newshape2$geometry.x, border = "grey60", axes = TRUE, 
     col = color.clusters(cepp30000)) 
legend("topright", legend = c("n* = 30000"))
plot(newshape2$geometry.x, border = "grey60", axes = TRUE, 
     col = color.clusters(cepp30000), xlim = range(450000:600000), ylim = c(4350000, 4500000)) 
legend("topright", legend = c("n* = 30000")) 

newshape3 <- as.data.frame(newshape2)
summary(newshape3$counts)
summary(newshape3$Population)

#Case number was decided with approximate mean, mean + 1/3(max-mean), mean + 2/3(max-mean)
coords = as.data.frame(cbind("x" = newshape2$latitude, "y" = newshape2$longitude)) 
cases = (newshape2$counts)
bn650 = bn.test(coords = coords, 
              cases = cases, 
              pop = newshape2$Population, 
              cstar = 650, 
              alpha = 0.01) 
bn650 # simple info 
summary(bn650) # cluster info 
 
 
bn1000 = bn.test(coords = coords, 
               cases = cases, 
               pop = newshape2$Population, 
               cstar = 1000, 
               alpha = 0.01) 
bn1000 
summary(bn1000) 
 
 
bn1350 = bn.test(coords = coords, 
               cases = cases, 
               pop =newshape2$Population, 
               cstar = 1350, 
               alpha = 0.01) 
bn1350 
summary(bn1350)

plot(newshape2$geometry.x, col = color.clusters(bn650)) 
title(main = "Statistically Significant Clusters, k = 650") 
plot(newshape2$geometry.x, border = "grey60",  
     col = color.clusters(bn650), xlim = range(450000:600000), ylim = c(4350000, 4500000)) 
title(main = "Statistically Significant Clusters, k = 650")
plot(newshape2$geometry.x, col = color.clusters(bn1000)) 
title(main = "Statistically Significant Clusters, k = 1000")
plot(newshape2$geometry.x, border = "grey60",  
     col = color.clusters(bn1000), xlim = range(450000:600000), ylim = c(4350000, 4500000)) 
title(main = "Statistically Significant Clusters, k = 1000")
plot(newshape2$geometry.x, col = color.clusters(bn1350)) 
title(main = "Statistically Significant Clusters, k = 1350") 
plot(newshape2$geometry.x, border = "grey60",  
     col = color.clusters(bn1350), xlim = range(450000:600000), ylim = c(4350000, 4500000)) 
title(main = "Statistically Significant Clusters, k = 1350")

coords = as.data.frame(cbind("x" = newshape2$latitude, "y" = newshape2$longitude)) 
cases = (newshape2$counts)
population = newshape2$Population
# expected number of cases 
e = sum(cases)/sum(population) * population 
# apply circular scan method 
scan_.5 = scan.test(coords = coords, 
                 cases = cases, 
                 pop = population, 
                 ex = e,  
                 ubpop = 0.5, 
                 nsim = 999, 
                 alpha  = 0.1) 
 
summary(scan_.5) 
 
# apply circular scan method 
scan_.1 = scan.test(coords = coords, 
                 cases = cases, 
                 pop = population, 
                 ex = e,  
                 ubpop = 0.1, 
                 nsim = 999, 
                 alpha  = 0.1) 
 
summary(scan_.1) 

#color 26 clusters 
mycol = grDevices::hcl.colors(26) 

plot(newshape2$geometry.x, border="grey60", axes=TRUE, 
     col = color.clusters(scan_.5, col = mycol))  
title(main = "Plot of clusters with population upper bound .5") 
plot(newshape2$geometry.x, border = "grey60",  
     col = color.clusters(scan_.5, col = mycol), xlim = range(450000:600000), ylim = c(4350000, 4500000)) 
title(main = "Plot of clusters with population upper bound .5")
 

#color 41 clusters 
mycol = grDevices::hcl.colors(41) 
# color.clusters(scan, col = mycol) colors the 3 clusters using the desired clusters 
plot(newshape2$geometry.x, border="grey60", axes=TRUE, 
     col = color.clusters(scan_.1, col = mycol)) 
title(main = "Plot of clusters with population upper bound .1") 
plot(newshape2$geometry.x, border = "grey60",  
     col = color.clusters(scan_.1, col = mycol), xlim = range(450050:620000), ylim = c(4350000, 4500000)) 
title(main = "Plot of clusters with population upper bound .1")
plot(newshape2$geometry.x, border = "grey60", 
     col = color.clusters(scan_.1, col = mycol), xlim = range(450000:600000), ylim = c(4250000, 4320000)) 

library(spdep)
nb_regions <- poly2nb(newshape2)
newshape <- sf::st_sf(diabetes_pop)
newshape <- st_transform(newshape, "+init=epsg:32613")
coords <- st_coordinates(st_point_on_surface(newshape))
# plot neighbor matrices for 85 and 89
plot(newshape2$geometry.x, border = "lightgrey")
points(newshape2$latitude, newshape2$longitude, col = 'red', pch = 19)
plot(nb_regions, coords, add = TRUE)
title("neighbor plot for Colorado census tracts")

# constant risk version of moran's i
y = cases # number of cases
N = length(y) # number of regions
n = population #population sizes
e = sum(y) / sum(n) * n # expected per region
# make a function for i_cr
i_cr = function(y, rni, W) {
y_std = matrix((y - rni)/sqrt(rni))
return(sum(W * y_std %*% t(y_std))/sum(W))
}
nsim = 499
tsimc = numeric(nsim)
w = nb2mat(nb_regions, style = "B")
t0c = i_cr(y, e, w) # observed statistic
# statistics for data simualted under CRH
for (i in 1:nsim) tsimc[i] = i_cr(rpois(N, e), rni = e, W = w)
# p-value
(sum(tsimc >= t0c) + 1)/(nsim + 1)

load("/newhome/priceade/nc.rda")
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(smerc))
suppressPackageStartupMessages(library(spdep))

coords = as.matrix(cbind("east" = newshape2$longitude, "north" = newshape2$latitude))
cases = (newshape2$counts)
pop = newshape2$Population

# Find distance matrix
d = as.matrix(dist(coords))
ds <- apply(d, 1, sort)
range(ds[2,])

coords = as.matrix(cbind("east" = newshape2$longitude, "north" = newshape2$latitude))
cases = (newshape2$counts)
pop = newshape2$Population

##################################################
# Exponential decay weight matrix
# use different kappas in defining weights
w.1  <- dweights(coords, kappa = 394.9)
w7  <- dweights(coords, kappa = 55137.9)
(tango_394.9 <-  tango.test(cases, pop, w.1, nsim = 499))
(tango_55137.9  <- tango.test(cases, pop, w7, nsim = 499))


gof <- c(tango_394.9$gof,tango_394.9$gof)
sa <- c(tango_55137.9$sa,tango_55137.9$sa)

plot(tango_394.9)
title(main = "plot of Tango Statistics, Null and Observed, k = 394.9")
plot(tango_55137.9)
title(main = "plot of Tango Statistics, Null and Observed, k = 55137.9")


plot(newshape2$geometry.x, border = "grey60", axes = TRUE, 
     col = color.clusters(cepp16000), xlim = range(450000:600000), ylim = c(4350000, 4500000)) 
legend("topright", legend = c("n* = 16000")) 
plot(newshape2$geometry.x, border = "grey60", axes = TRUE, 
     col = color.clusters(cepp20000), xlim = range(450000:600000), ylim = c(4350000, 4500000)) 
legend("topright", legend = c("n* = 20000")) 
plot(newshape2$geometry.x, border = "grey60", axes = TRUE, 
     col = color.clusters(cepp30000), xlim = range(450000:600000), ylim = c(4350000, 4500000)) 
legend("topright", legend = c("n* = 30000")) 


plot(newshape2$geometry.x, border = "grey60",  
     col = color.clusters(bn650), xlim = range(450000:600000), ylim = c(4350000, 4500000)) 
title(main = "Statistically Significant Clusters, k = 650")
plot(newshape2$geometry.x, border = "grey60",  
     col = color.clusters(bn1000), xlim = range(450000:600000), ylim = c(4350000, 4500000)) 
title(main = "Statistically Significant Clusters, k = 1000")
plot(newshape2$geometry.x, border = "grey60",  
     col = color.clusters(bn1350), xlim = range(450000:600000), ylim = c(4350000, 4500000)) 
title(main = "Statistically Significant Clusters, k = 1350")

mycol = grDevices::hcl.colors(26)
plot(newshape2$geometry.x, border = "grey60",  
     col = color.clusters(scan_.5, col = mycol), xlim = range(450000:600000), ylim = c(4350000, 4500000)) 
title(main = "Plot of clusters with population upper bound .5")
mycol = grDevices::hcl.colors(41) 
plot(newshape2$geometry.x, border = "grey60",  
     col = color.clusters(scan_.1, col = mycol), xlim = range(450000:600000), ylim = c(4350000, 4500000)) 
title(main = "Plot of clusters with population upper bound .1")


