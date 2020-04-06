dset ^./output.nc
undef -9.99e+08
dtype netcdf
xdef 1 linear 1 1
ydef 1 linear 1 1
zdef 100 linear 1 1
tdef 301 linear 00z01jan2000 1mn
vars 3
W=>W 100 z   w
T=>T 100 t,z temp
Q=>Q 100 t,z q
endvars
