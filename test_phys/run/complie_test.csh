#!/bin/csh -f 

# [ DO NOT CHANGE ] ==================================================
set rundir=`pwd`
set srcdir=$rundir/../src
set blddir=$rundir/../src/bld

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
rm -f *.bin main.exe 

ifort ${srcdir}/libs/Mod_global.f90          \
      ${srcdir}/libs/Mod_read.f90            \
      ${srcdir}/init/Mod_init.f90            \
      ${srcdir}/dyn/Mod_dyn_driver.f90      \
      ${srcdir}/main/Mod_integration.f90     \
      ${srcdir}/libs/Mod_write.f90           \
      ${srcdir}/main/main.f90                \
-o    main.exe                \
     -L/$NETCDF/lib -I/$NETCDF/include -lnetcdf

mv *.o *.mod $blddir/obj
 
#./main.exe  
## 2>/dev/null
#
#rm -f mod_global.mod  Mod_global.o  mod_read.mod  Mod_read.o
#rm -f Mod_init.o mod_init.mod mod_integration.mod Mod_integration.o
#rm -f mod_dyn_driver.mod Mod_dyn_driver.o mod_write.mod Mod_write.o


#exit 0
#=====================================================================
