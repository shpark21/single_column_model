program test



  IMPLICIT NONE
  INTEGER                           :: dt, i, it, j
  INTEGER, PARAMETER                :: dz=1, nvar = 10, nt = 10

  ! For FD, Boundary conditions at both ends are required. 
  REAL,    DIMENSION(0:nvar+1)      :: var
  REAL,    DIMENSION(0:nvar+1)      :: q

  REAL,    DIMENSION(nvar)          :: w
  REAL,    DIMENSION(nvar)          :: C

  REAL,    DIMENSION(nvar)          :: next_var
  REAL,    DIMENSION(nvar)          :: next_q

  REAL,    DIMENSION(nt,0:nvar+1)   :: qt
  REAL,    DIMENSION(nt,0:nvar+1)   :: vart

  open(10,file="./temp_fd.bin",status='unknown',form="unformatted",access="direct",recl=4*10*nt)
  open(20,file="./q_fd.bin",status='unknown',form="unformatted",access="direct",recl=4*10*nt)

  ! IC, BC
  dt = 5 
  w(:)=0.1
  w(nvar)=0.

  var(:) = 0.
  var(nvar+1) = 0.
  var(1) = 1.

  q(:) = 5.
  q(1) = 1.

  ! CFL (Courant-Friedrichs-Lewy) condition
  C(:) = w(:)*dt/real(dz)

  write(*,*) c
  ! Time integrating temperature using FD
  DO it = 1, nt
    DO i = 1, nvar
      next_var(i) = var(i) - C(i)*(var(i+1)-var(i-1))
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

  ! Time integrating q using FD
  DO it = 1, nt
    DO i = 1, nvar
      next_q(i) = q(i) - C(i)*(q(i+1)-q(i-1))
      write(*,*) "q", i," = ", q(i), "next_q ", i,"= ", next_q(i)
    ENDDO
    IF ( sum(q) .eq. sum(next_q) ) then
      write(*,*) "mass conservation was maintained -> OO "
      write(*,*) sum(q), sum(next_q)
    ELSE
      write(*,*) "mass conservation was not maintained -> XX "
      write(*,*) sum(q), sum(next_q)
    ENDIF 
    qt(it,:) = q(:)
    q(1:nvar)=next_q(:)
  ENDDO

  write(unit = 10, rec=1) (vart(it,1:nvar),it=1,nt)
  write(unit = 20, rec=1) (qt(it,1:nvar),it=1,nt)
 
! do i = 1, nvar
! write(*,*) "var  = ", var(i), "next_var = ", next_var(i)
! ENDDO
end program test
