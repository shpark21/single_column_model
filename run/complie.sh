#!/bin/sh -f 

# [ DO NOT CHANGE ] ==================================================
srcdir=../src
rundir=`pwd`

rm -f mod_global.mod  Mod_global.o  mod_read.mod  Mod_read.o 
rm -f Mod_init.o mod_init.mod 

ifort Mod_global.f90          \
      Mod_read.f90            \
      Mod_init.f90            \
      Mod_dyn_driver.f90    \
      Mod_integration.f90   \
      main.f90 

./a.out  
# rm -f mod_global.mod  Mod_global.o  mod_read.mod  Mod_read.o 
# rm -f Mod_init.o mod_init.mod 
# -o      ${srcdir}/main.exe                        \ 
# 2>/dev/null
#



# rm -f mod_global.mod ${srcdir}/libs/mod_global.mod
# rm -f mod_read.mod   ${srcdir}/libs/mod_read.mod
# rm -f mod_write.mod  ${srcdir}/libs/mod_write.mod
# rm -f mod_init.mod   ${srcdir}/init/mod_init.mod

#
# cd ${srcdir}
# rm -f *.o *.mod
#
# cd ..

#exit 0
#=====================================================================
# ln -sf   ${srcdir}/libs/Mod_global.f90          . 
# ln -sf   ${srcdir}/libs/Mod_read.f90            .
# ln -sf   ${srcdir}/init/Mod_init.f90            .
# ln -sf   ${srcdir}/dyn/Mod_dyn_driver.f90       .        
# ln -sf   ${srcdir}/main/Mod_integration.f90     .      
# ln -sf   ${srcdir}/main/main.f90                .

# cp -f ${srcdir}/libs/Mod_global.f90          . 
# cp -f ${srcdir}/libs/Mod_read.f90            .
# cp -f ${srcdir}/init/Mod_init.f90            .
# cp -f ${srcdir}/dyn/Mod_dyn_driver.f90       .        
# cp -f ${srcdir}/main/Mod_integration.f90     .      
# cp -f ${srcdir}/main/main.f90                .
