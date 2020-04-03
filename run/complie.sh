#!/bin/sh -f 

# [ DO NOT CHANGE ] ==================================================
srcdir=../src
rundir=`pwd`

# ln -sf   ${srcdir}/libs/Mod_global.f90          . 
# ln -sf   ${srcdir}/libs/Mod_read.f90            .
# ln -sf   ${srcdir}/init/Mod_init.f90            .
# ln -sf   ${srcdir}/dyn/Mod_dyn_driver.f90       .        
# ln -sf   ${srcdir}/main/Mod_integration.f90     .      
# ln -sf   ${srcdir}/main/main.f90                .

rm -f mod_global.mod  Mod_global.o  mod_read.mod  Mod_read.o 
rm -f Mod_init.o mod_init.mod mod_integration.mod Mod_integration.o 
rm -f mod_dyn_driver.mod Mod_dyn_driver.o mod_write.mod Mod_write.o
rm -f output.nc main.exe 

ifort Mod_global.f90          \
      Mod_read.f90            \
      Mod_init.f90            \
      Mod_dyn_driver.f90      \
      Mod_integration.f90     \
      Mod_write.f90           \
      main.f90                \
-o    main.exe                \
     -L/$NETCDF/lib -I/$NETCDF/include -lnetcdf
./main.exe  
# 2>/dev/null

exit 0
#=====================================================================
