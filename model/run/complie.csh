#! -f /bin/csh

set home_dir = '/home/gihh1131/HOMEWORK/MICROPHYSICS/single_column_model/model/src' 

pgf90 ${home_dir}/libs/Mod_global.f90 ${home_dir}/libs/Mod_read.f90 ${home_dir}/init/Mod_initialization.f90 ${home_dir}/dyn/Mod_dyn_driver.f90  ${home_dir}/libs/Mod_write.f90 ${home_dir}/main/test_main.f90

./a.out

rm -rf *.mod *.o
