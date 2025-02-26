# Yay, we got the model output
# Not-yay, it's a NetCDF file
# Live Ocean model output - hourly summer 2023 at Teawhit Head 42m depth buoy

# load the `ncdf4` and the `CFtime` packages
library(ncdf4)
library(CFtime)
library(lattice)
library(RColorBrewer)
library(here)
library(tidyverse)
library(lattice)
library(raster) # second tutorial https://semba-blog.netlify.app/11/03/2018/converting-netcdf-files-into-data-frame/
library(data.table) # https://stackoverflow.com/questions/2185252/reshaping-data-frame-from-wide-to-long-format
library(reshape2)

# I'm using this tutorial: https://pjbartlein.github.io/REarthSysSci/netCDF.html#introduction
# The tutorial uses an array with 12 layers (months) and latitude x longitude 

ncpath <- here("LiveOceanTH042_2021_2023.nc")
oxygen <- "oxygen" # for extraction
temp <- "temp"

ncin <- nc_open(ncpath)
print(ncin)

# 46 variables - that's a looooot more than the example

# float oxygen[s_rho,ocean_time] 
# so we need to grab s_rho, ocean_time

#    ocean_time  Size:26281   *** is unlimited ***
# standard_name: time
# long_name: Time [UTC]
# units: seconds since 1970-01-01
# calendar: proleptic_gregorian

ocean_time <- ncvar_get(ncin,"ocean_time")
nocean_time <- dim(ocean_time) # seems correct. file says 26281, variable says 26281L
head(ocean_time) # those seem like they could be seconds since the unix epoch

s_rho <- ncvar_get(ncin,"s_rho")
ns_rho <- dim(s_rho) # seems correct. file says 30, variable says 30L
head(s_rho) # no idea what this means

tunits <- ncatt_get(ncin,"ocean_time","units")

# DO in mmol/m^3 is oxygen
ounits <- ncatt_get(ncin, oxygen,"units")
ox_array <- ncvar_get(ncin, oxygen)
ofillvalue <- ncatt_get(ncin, oxygen, "_FillValue")
ox_array[ox_array==ofillvalue$value] <- NA

# temp in C is temperature
tunits <- ncatt_get(ncin, temp, "units")
temp_array <- ncvar_get(ncin, temp)
tfillvalue <- ncatt_get(ncin, temp, "_FillValue")
temp_array[temp_array==tfillvalue$value] <- NA

# z_rho has depth information
# https://www.myroms.org/wiki/Numerical_Solution_Technique
dunits <- ncatt_get(ncin, "z_rho", "units")
dep_array <- ncvar_get(ncin, "z_rho")
dfillvalue <- ncatt_get(ncin, "z_rho", "_FillValue")
dep_array[dep_array==dfillvalue$value] <- NA

# z_w has depth information
# Not dealing with it currently bc it needs s_w coordinates
# instead of [s_rho, ocean_time] and the current script isn't built for that


nc_close(ncin)

time <- as.POSIXct(ocean_time, tz = "UTC", origin = "1970-01-01") # those are the times we wanted! huzzah.

# each variable has coordinates [s_rho, ocean_time] that it can be grabbed by
temp_slice <- temp_array[,1] # first column
temp_slice2 <- temp_array[1,1]

# Create dataframe 

# Mess around a little first
coords <- as.matrix(expand.grid(s_rho, time)) # making space for each obs
dim(coords) # equals the size of ox_array - good!

temp_vec <- as.vector(temp_slice) # temp slice is the first column/first date

temp_df1 <- data.frame(cbind(coords, temp_vec))

names(temp_df1) <- c("s_rho", "Date_Time_UTC", "Temp_C")

# Reshape the array
temp_vec_long <- as.vector(temp_array) 
# the size of this is equal to the temp array width x length, which is good
# next step should be same dimensions as coords
temp_mat <- matrix(temp_vec_long, nrow = dim(s_rho), ncol = dim(time))
dim(temp_mat) # same dimensions as temp_array - good i think? this may be an unnecessary step for a 2D array

# Make big dataframe
temp_df_2 <- data.frame(cbind(coords, temp_vec_long)) # dimensions = ns_rho * nocean_time, which is good
names(temp_df_2) <- c("s_rho", "Date_UTC", "Temp_C")

# Now add oxygen
Oxygen_mmol_m3 <- as.vector(ox_array)
df3 <- data.frame(cbind(temp_df_2, Oxygen_mmol_m3))
df4 <- df3 %>% 
  mutate(s_rho = as.numeric(s_rho), 
         Date_UTC = as.POSIXct(Date_UTC, tz = "UTC"), 
         Temp_C = as.numeric(Temp_C))

# Now add depth
Depth <- as.vector(dep_array)
df5 <- data.frame(cbind(df4, Depth))

# Now convert oxygen
# See lab notebook, but mmol/m^3 x 0.015999 = mg/L oxygen
full_df <- df5 %>% 
  mutate(Oxygen_mg_L = Oxygen_mmol_m3 * 0.015999) # %>% # these seem legit

moor_depth_df <- full_df %>% 
  filter(Depth < -36 & Depth > -39)

final_df <- moor_depth_df %>% 
  group_by(Date_UTC) %>% 
  summarize(Temp_C = mean(Temp_C), 
            Oxygen_mg_L = mean(Oxygen_mg_L), 
            Depth = mean(Depth), 
            Oxygen_mmol_m3 = mean(Oxygen_mmol_m3))

# s_rho is a depth coordinate system where the bottom is -1 and the surface is 0
# https://www.mathworks.com/matlabcentral/answers/2056554-from-s-coordinate-at-rho-points-to-depth
# i do Not wanna do all that math. mooring is close to the bottom so let's use -0.95 rho


write.csv(full_df, here("LiveOceanTH042_2021_2023.csv"))
write.csv(final_df, here("LiveOcean_TH042_2021_2023_DepthClean.csv"))