'reinit'
'set display white'
'clear'
'set grads off'

'sdfopen output.nc'
'c'
'set z 0 50'
'set t 1 last'

'set gxout grfill'
*'set cmin 0.0'


'set cmin 0.0'
'd q'

'gxprint x.png'
*'!convert x.png -trim -quality 100 fv.png'
'!convert x.png -trim -quality 100 ppm.png'
'!rm -f x.png'
