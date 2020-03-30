#!/bin/sh -f

rm -f mod_global.mod a.out

ifort Mod_global.f90 Mod_integration.f90 dyn_test_main.f90 
                    
#ifort Mod_integration Mod_global.f90 dyn_test_main.f90 
./a.out

rm -f mod_global.mod a.out

