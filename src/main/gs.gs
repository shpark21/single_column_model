'reinit'
'set display white'
'clear'
'set grads off'

'open temp.ctl'
t=1
while(t<=100)
'c'
'set vrange -5 20'
'set x 0 102'
'set t 't
*'d q-273.15'
'd q'
t=t+1
endwhile
