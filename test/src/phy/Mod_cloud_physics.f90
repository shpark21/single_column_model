PROGRAM Mod_cloud_physics

  IMPLICIT NONE
  INTEGER, PARAMETER :: nz=100, nbin=100
  REAL, PARAMETER :: rmin= 0.1, rratio = 2.
  REAL :: rmax
  REAL, DIMENSION(nz, nbin) :: conc
  REAL, DIMENSION(nbin) :: r, m, n_r
  REAL, DIMENSION(nbin+1) :: rb, mb
  INTEGER :: ir
  REAL :: pi
  REAL :: N0, r0, sigma, dev_r

  pi = ACOS(-1.)

  rmax = rmin*(rratio**nbin)

  rb(1) = rmin
  rb(nbin+1) = rmax
  DO ir = 2, nbin
    rb(ir) = rratio*rb(ir-1)
  ENDDO
!print*, rb

  mb = rb**3

!print*, '    '
!print*, mb     
  DO ir = 1, nbin
    m(ir) = (mb(ir) + mb(ir+1))/2.
  END DO

  r = m**(1./3.)   

!print*, '    '
!print*, m     
!
!print*, '    '
!print*, r     
!

  !--------------------------------------!

!  r0 = SUM(r)/REAL(nbin)
!  sigma = SQRT(SUM((r-r0)**2)/REAL(nbin))
  r0 = -1.
  sigma = 0.4 

  N0 = 1.

  DO ir = 1, nbin
    n_r(ir) = (N0/(SQRT(2*pi)*sigma*r(ir))) * EXP(((log(r(ir)/r0))**2)/(2.*(sigma**2))*-1.)
!    n_r(ir) = (N0/(SQRT(2*pi)*log(sigma))) * EXP(((log(r(ir)/r0))**2)/(2*(log(sigma)**2))*-1.)
    print*, log(r(ir)), n_r(ir)
  ENDDO
!  print*, n_r
!  print*, SUM(n_r * r)
!  print*, SUM(n_r)

END PROGRAM Mod_cloud_physics
