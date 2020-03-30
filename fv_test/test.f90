program test

  IMPLICIT NONE
  INTEGER                      :: dt, i, it, j
  INTEGER, PARAMETER           :: dz=50, nvar = 10, nt = 10

  ! If w > 0, SFC boundary conditions must exist.
  REAL,    DIMENSION(0:nvar)   :: var
  REAL,    DIMENSION(0:nvar)   :: q

  REAL,    DIMENSION(nvar+1)   :: w

  REAL,    DIMENSION(nvar)     :: backward_flux_var
  REAL,    DIMENSION(nvar)     :: backward_flux_q
  REAL,    DIMENSION(nvar)     :: forward_flux_var 
  REAL,    DIMENSION(nvar)     :: forward_flux_q

  REAL,    DIMENSION(nvar)     :: next_var
  REAL,    DIMENSION(nvar)     :: next_q

  REAL,    DIMENSION(nt,0:nvar)   :: qt
  REAL,    DIMENSION(nt,0:nvar)   :: vart

  open(10,file="./temp.bin",status='unknown',form="unformatted",access="direct",recl=4*10*nt)
  open(20,file="./q.bin",status='unknown',form="unformatted",access="direct",recl=4*10*nt)

  ! IC, BC
  dt = 10

  w(:)=2.
  w(nvar+1)=0.

  var(:) = 1.
  var(0) = 2.

  q(:) = 10.
  q(0) = 0.

  ! Time integrating temperature using FV
  DO it = 1, nt
    DO i = 1, nvar
      backward_flux_var(i) = var(i-1)*w(i)*dt
      forward_flux_var(i)  = var(i)*w(i+1)*dt
      next_var(i) = var(i) + (backward_flux_var(i)-forward_flux_var(i))/dz
      write(*,*) "temp", i," = ", var(i), "temp_var ", i,"= ", next_var(i)
    ENDDO
    ! For check mass conservation 
    IF ( sum(var) .eq. sum(next_var) ) then
      write(*,*) "mass conservation was maintained -> OO "
      write(*,*) sum(var), sum(next_var)
    ELSE 
      write(*,*) "mass conservation was not maintained -> XX "
      write(*,*) sum(var), sum(next_var)
    ENDIF
    vart(it,:) = var(:)
    var(1:nvar)=next_var(:)
  ENDDO

  ! Time integrating q using FV
  DO it = 1, nt
    DO i = 1, nvar
      backward_flux_q(i) = q(i-1)*w(i)*dt
      forward_flux_q(i)  = q(i)*w(i+1)*dt
      next_q(i) = q(i) + (backward_flux_q(i)-forward_flux_q(i))/dz
      write(*,*) "q", i," = ", q(i), "next_q ", i,"= ", next_q(i)
    ENDDO
    IF ( sum(q) .eq. sum(next_q) ) then
      write(*,*) "mass conservation was maintained -> OO "
      write(*,*) sum(q), sum(next_q)
    ELSE 
      write(*,*) "mass conservation was not maintained -> XX "
      write(*,*) sum(q), sum(next_q)
    ENDIF 
    write(*,*) sum(q), sum(next_q)
    qt(it,:) = q(:)
    q(1:nvar)=next_q(:)
  ENDDO

   write(unit = 10, rec=1) (vart(it,1:nvar),it=1,nt)
   write(unit = 20, rec=1) (qt(it,1:nvar),it=1,nt)
 
end program test
