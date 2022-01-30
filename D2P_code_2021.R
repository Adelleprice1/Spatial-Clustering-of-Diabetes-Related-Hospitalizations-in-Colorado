shape <- sf::st_read("C:/Users/adell/Downloads/Diabetes_Hospitalization_Rate_(Census_Tracts)/Diabetes_Hospitalization_Rate_(Census_Tracts).shp")
plot(select_shape$geometry, border="grey60")
suppressPackageStartupMessages(library(GISTools))
select_shape <- na.rm(shape)
choropleth(select_shape$geometry, select_shape$DIABETES_A)
shades = auto.shading(select_shape$DIABETES_A)
choro.legend(px='bottomleft', sh=shades, fmt="%4.1f")
title(main = "Choropleth map of Diabetes-Related Hospitalization Rate in Colorado Census Tracts")



population <- sf::st_read("C:/Users/adell/Downloads/Population_Density_(Census_Tracts)/Population_Density_(Census_Tracts).shp")
plot(population$geometry, border="grey60")
