library(raster)

ghi <- raster("africa/morroco/solar_irradiation/solar_irradiaton_ghi.tif")
temp <- raster("africa/morroco/solar_irradiation/TEMP.tif")
slope <- raster("africa/morroco/slope/slope.tif")
elevation <- raster("africa/morroco/elevation/morocco_elevation.tif")
cities <- raster("africa/morroco/cities/cities_to_raster.tif")
road <- raster("africa/morroco/road_network/pry_network_to_raster.tif")
power <- raster("africa/morroco/power_line/powerline_to_raster.tif")
land_use<- raster("africa/morroco/land_use/land_use.tif")




ghi_res <- raster("africa/morroco/solar_irradiation/solar_irradiaton_ghi_resampled.tif")
temp_res <- raster("africa/morroco/solar_irradiation/TEMP_resampled.tif")
slope_res <- raster("africa/morroco/slope/slope_resampled.tif")
elevation_res <- raster("africa/morroco/elevation/morocco_elevation_resampled.tif")
cities_res <- raster("africa/morroco/cities/cities_to_raster_resampled.tif")
road_res <- raster("africa/morroco/road_network/pry_network_raster_resampled.tif")
power_res <- raster("africa/morroco/power_line/powerline_to_raster_resampled.tif")
#land_use_res <- raster("africa/morroco/land_use/land_use_resampled.tif")
land_use_res<- raster("africa/morroco/land_aspect/land_aspect_resampled.tif")
land_use_suitability <- raster("africa/morroco/land_use/land_use_suitability.tif")
land_use_suitability <- resample(land_use_suitability, scaled_ghi_res, method = "bilinear")


restricted_zone <- raster("africa/morroco/restricted_zone/restricted_zone_raster_resampled.tif")

hist(land_use_suitability)
plot(restricted_zone)
plot(scaled_restricted_zone)
hist(scaled_restricted_zone)
scaled_restricted_zone
plot(ghi_res)
plot(temp_res)
plot(slope_res)
plot(cities_res)
plot(road_res)
plot(power_res)
plot(land_use_res)

summary(ghi_res)
summary(temp_res)
summary(slope_res)
summary(elevation_res)
summary(cities_res)
summary(road_res)
summary(power_res)
summary(land_use_res)

scaled_ghi_res <-  calc(ghi_res, fun= function(x){ifelse(x>0, x*100/ 2.262724e+03, 0)})
scaled_temp_res <-  calc(temp_res, fun= function(x){ifelse(x>0, x*100/ 26.2000027, 0)})  #(1-(x/28.3104458) )* 100
#scaled_slope_res <- calc(slope_res, fun= function(x){ifelse(x>0, x*100/ 28.3104458, 0)})
scaled_slope_res <- calc(slope_res, fun= function(x){ifelse(x>0, (1-(x/27.0786228) )* 100, 0)})
#scaled_elevation_res <- calc(elevation_res, fun= function(x){ifelse(x>0, x*100/ 3883, 0)})
scaled_cities_res <-  calc(cities_res, fun= function(x){ifelse(x>0, x*100/ 2.361934, 0)})
scaled_road_res <-  calc(road_res, fun= function(x){ifelse(x>0, x*100/  3.3329275, 0)})
scaled_power_res <- calc(power_res, fun= function(x){ifelse(x>0, (1-(x/4.9910169) )* 100, 0)})
#scaled_land_use_res <- calc(land_use_res, fun= function(x){ifelse(x>0, (1-(x/255) )* 100, 0)})
scaled_land_use_res <- calc(land_use_res, fun = function(x){
  ifelse(x < 180, (x/ 180)*100, 
         ifelse(x == 180, 
                100, (1- ((x-180)/180))*100))})

scaled_land_use_res <- resample(scaled_land_use_res, scaled_ghi_res, method = "bilinear")


scaled_restricted_zone <- calc(restricted_zone, function(x){ifelse(x<0, 1, 0)})



optimal_site <- overlay(scaled_ghi_res, scaled_temp_res, scaled_slope_res, scaled_land_use_res, 
                        scaled_cities_res, scaled_road_res, scaled_power_res, scaled_restricted_zone, land_use_suitability, 
                        fun = function(a, b, c, d, e, f_, g, h, i){
                          (f[1]*a + f[2]*b + f[3]*c + f[4]*d + f[5]*e + f[6]*f_ + f[7]*g )*h*i
                        })

summary(optimal_site)
optimal_site
result<- calc(optimal_site, fun= function(x){ifelse(x>0, x*100/ 91.76404  , 0)})

writeRaster(result, "africa/morroco/optimal_location/optimal_location.tif", overwrite=TRUE)
plot(work)
hist(result)


hist(scaled_restricted_zone)
par(mfrow = c(2,1))
plot(optimal_site)
plot(optimal_site_1)
plot(suitability)

