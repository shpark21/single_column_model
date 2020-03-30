#!/bin/sh -f 

# [ DO NOT CHANGE ] ==================================================
srcdir=`pwd`/src
rundir=`pwd`/run

rm -rf *.o *.mod

pgf90 ${srcdir}/libs/module_global.f90              \
      ${srcdir}/main/main.f90                       \
      ${srcdir}/main/Mod_intergration.f90           \
      ${srcdir}/init/Mod_initializaion.f90          \
      ${srcdir}/dyn/Mod_dyn_driver.f90              \
      ${srcdir}/phys/Mod_phys_driver.f90            \
      ${srcdir}/libs/Mod_read.f90                   \
      ${srcdir}/libs/Mod_write.f90                  \
-o    ${srcdir}/main.exe                            \
-Mbounds -L/$NETCDF/lib -I/$NETCDF/include -lnetcdf \
2>/dev/null

cp ${srcdir}/main.exe ${rundir}

rm -rf *.o 
cd ${srcdir}
rm -rf *.o 
cd ..
rm -rf ${srcdir}/main.exe

exit 0
#=====================================================================
