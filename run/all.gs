'reinit'
'set display white'
'clear'
'set grads off'

taxis=off
var = q


if ( taxis = on )
'sdfopen output_geo_fd.nc'
'sdfopen output_geo_fv.nc'
'sdfopen output_geo_ppm.nc'
'set z 1 100'
'set t 1 last'
'd 'var'.1'
'd 'var'.2'
'd 'var'.3'
endif

if ( taxis = off )
'sdfopen output_geo_fv.nc'
'sdfopen output_geo_ppm.nc'

'set t last'
'q dims'
st=sublin(result,5);last=subwrd(st,9)
t=1
while(t<=last)
'c'
'set grads off'
'set t 't
say t
'set z 1 100'
'set vrange -10 400'
'd 'var'.1'
'd 'var'.2'
* 'set ccolor 15' 
* 'd 'var'.1'

'set string 1 l 1 0';'set strsiz 0.13 0.13';'d sum('var',z=1,z=60)';a=sublin(result,2);b=subwrd(a,4)
                    'draw string 2.7 7.9 `5TOTAL-FD: `5'substr(b,1,5)'' ;
'set string 1 l 1 0';'set strsiz 0.13 0.13';'d sum('var'.2,z=1,z=60)';a=sublin(result,2);b=subwrd(a,4)
                    'draw string 4.9 7.9 `5TOTAL-FEFV: `5'substr(b,1,5)'' ;
'set string 1 l 1 0';'set strsiz 0.13 0.13';'d sum('var'.3,z=1,z=60)';a=sublin(result,2);b=subwrd(a,4)
                    'draw string 7.3 7.9 `5TOTAL-PPM: `5'substr(b,1,5)'' ;

* 'gxprint x.png'
* '!convert x.png -trim -quality 100 't'_arith.png'
* * '!convert x.png -trim -quality 100 't'_geo.png'
* '!rm -f x.png'
t=t+100
endwhile


endif
