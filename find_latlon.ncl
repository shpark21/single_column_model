fh     = addfile("/data3/era5/Tpl/Tpl.201707.6hr.grib", "r")

lat_1  = fh->g0_lat_2
lon_1  = fh->g0_lon_3

rm = new( (/1,2/), "float")

i_rlat = ind_nearest_coord(lat, lat_1, 0)
i_rlon = ind_nearest_coord(lon, lon_1, 0)

rm(0,0) = i_rlat
rm(0,1) = i_rlon

print("")
print("      Entered value ) latitude : "+lat+"     longitude : "+lon)
print("")
print("Corresponding index ) latitude : "+rm(0,0)+"     longitude : "+rm(0,1))
