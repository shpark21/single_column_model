program driver

  implicit none

======= pms
  integer :: a,b 
  real :: p, m, s
=======
======= by jh
  integer :: a,b 

  /namelist/ nz

  OPEN(10, file='name.list')



======= by min
  integer :: a,b

  open()
  call cal_delta_t() 


  
end program driver
