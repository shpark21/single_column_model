MODULE Mod_init

  USE Mod_global
  USE Mod_const
  USE Mod_read, only: Sub_read_NC_file

  IMPLICIT NONE

    CONTAINS
    
    !!---------------------------------------------!!
    !!  Cal. dt regared CFL condition              !!
    !!---------------------------------------------!!
    SUBROUTINE Sub_set_dt

    IMPLICIT NONE

      !Local
      REAL, PARAMETER                  :: courant_number = 1.0
      REAL, DIMENSION(:), ALLOCATABLE  :: CFL

      ALLOCATE( CFL (nz) )
      ! Courant-Friedrichs-Lewy condition
      WHERE (w%dz /= 0.)
        CFL = courant_number*(dz%dz/abs(w%dz))
      ELSEWHERE
        CFL = MAXVAL(CFL)
      END WHERE
      dt = INT(MINVAL(CFL))
      nt = INT(integrated_time/dt)

      IF ( nt*dt .ne. integrated_time ) then
        write(*,*) "  "
        write(*,*) "********WARMING"
        write(*,*) "Calculated Total integrated time is different from the namelist integrated_time"
        write(*,*) "Total integrated time     =  ", nt*dt
        write(*,*) "Namelist integrated_time  =  ", integrated_time
        write(*,*) "********"
        write(*,*) "  "
      ENDIF

      write(*,*) "dt =  ", dt
      write(*,*) "nt =  ", nt
      write(*,*) "  "

    END SUBROUTINE Sub_set_dt

    !!---------------------------------------------!!
    !!  Cal. vertical coordinate                   !!
    !!---------------------------------------------!!
    SUBROUTINE Sub_set_grid

    IMPLICIT NONE

      ! Cal. dz      
      IF ( dz_option .eq. 1) THEN
        dz%dz(:) = z_top/nz
      ELSE IF ( dz_option .eq. 2) THEN
        dz%dz(1) = ((dzr-1)*z_top)/((dzr**nz)-1) 
        DO iz = 2, nz
          dz%dz(iz) = dz%dz(iz-1)*dzr
        ENDDO !! z
      ENDIF

      ! Cal. height     
      z%dz(1)= dz%dz(1)
      DO iz = 2, nz
        z%dz(iz)= z%dz(iz-1) + dz%dz(iz)
      ENDDO

    END SUBROUTINE Sub_set_grid
   
    !!---------------------------------------------!!
    !!  Cal. Temperature                           !!
    !!---------------------------------------------!!
    SUBROUTINE Sub_set_T

      IMPLICIT NONE

      !! boundary condition
      DO it = 1, nt
        Temp%sfc_dt(it) = 20. + 273.5
      ENDDO !! time do

      !! initial  condition
      DO iz = 1, nz
        IF (iz .EQ. 1) THEN
          Temp%dz(iz) = Temp%sfc_dt(1) - gamma_dry * dz%dz(iz)
        ELSE
          Temp%dz(iz) = Temp%dz(iz-1) - gamma_dry * dz%dz(iz)
        ENDIF !! iz = 1 & others
      ENDDO !! Temp from 1 to nz

      DO it = 1, nt
        Temp%top_dt(it) = Temp%dz(nz)
      ENDDO !! time do

    END SUBROUTINE Sub_set_T

    !!---------------------------------------------!!
    !!  Cal. vertical wind                         !!
    !!---------------------------------------------!!
    SUBROUTINE Sub_set_W ( nz , dz , grid_w , stag_w )

      IMPLICIT NONE

      !In
      INTEGER,               INTENT(IN)   :: nz
      REAL, DIMENSION(nz),   INTENT(IN)   :: dz 
      REAL, DIMENSION(nz),   INTENT(IN)   :: grid_w
      !Local
      INTEGER                             :: iz
      REAL                                :: wgt 
      !Out
      REAL, DIMENSION(nz+1), INTENT(OUT)  :: stag_w

      ! Make stagged grid for advection
      DO iz = 2, nz
          wgt = dz(iz-1) / (dz(iz-1)+dz(iz))
          stag_w(iz) = grid_w(iz-1) + wgt*(grid_w(iz)-grid_w(iz-1))
      end do

      ! Set Boundary Condition
      ! most likely w = 0 at these points
      stag_w(1) = 0.; stag_w(nz+1) = 0.     ! Homogeneous Dirichlet BC

    END SUBROUTINE Sub_set_W  
  
    !!---------------------------------------------!!
    !!  Cal. drop number                           !!
    !!---------------------------------------------!!
    SUBROUTINE Sub_set_Q

    IMPLICIT NONE

      ! aaa=70
      ! ddd=90
      !
      ! bbb=aaa+10
      ! ccc=ddd-10
      ! q%dz(aaa:bbb)=100
      !
      ! DO iz = 1, 4
      ! q%dz(aaa+iz:bbb-iz)=100.-real(iz)*20
      ! q%dz(ccc+iz:ddd-iz)=100.-real(iz)*20
      ! ENDDO
      DO iz = 1, nz
        q%dz(iz) = 100*sin(real(iz)) + 100.
      ENDDO

      q%sfc_dt(:)=0.
      q%top_dt(:)=0.

    END SUBROUTINE Sub_set_Q  

 
    !!---------------------------------------------!!
    !!  Set. size distributions                    !!
    !!---------------------------------------------!!
    SUBROUTINE Sub_drop_distributions     &
               (                          &
                distribution_option,      &
                drop_column_num,          & !!  drop_column_num
                rbmin,                    & !!  drop_1st_diameter
                rbmax,                    & !!  drop_1st_diameter
                nr                        & !!  drop_num
               )

    USE Mod_const, only: rho,nc,qc,pi
    IMPLICIT NONE

    ! IN
    INTEGER, INTENT(IN)                      :: distribution_option, &
                                                drop_column_num              
    REAL,    INTENT(IN)                      :: rbmin,               &
                                                rbmax
    ! Local
    REAL, DIMENSION(drop_column_num)         :: r,  &
                                                D,  &
                                                dr 
    REAL, DIMENSION(drop_column_num+1)       :: rb, &
                                                dD
    INTEGER                                  :: ir, nbin
    REAL                                     :: ratio, &
                                                mu, sigma, lambda
    ! OUT
    ! REAL, DIMENSION(nz,column_num), INTENT(OUT) :: conc
    REAL, DIMENSION(drop_column_num),    INTENT(OUT) :: nr
    
    nbin  = drop_column_num

    rb(1) = rbmin ; rb(nbin+1) = rbmax       ! boundary of drop diameter
    ratio = (rb(nbin+1)/rb(1))**(1./nbin)
    DO ir = 2, nbin
      rb(ir) = ratio*rb(ir-1)  
    ENDDO
    
    DO ir = 1, nbin
      r(ir) = (rb(ir)+rb(ir+1))/2.
      dr(ir) = rb(ir+1)-rb(ir)
    ENDDO

    SELECT CASE(distribution_option)
      CASE(1)

        ! Log-normal distribution
        mu = LOG(r0)
        sigma = SQRT((2./9.)*LOG(qc/(nc*rho*(4./3.)*pi*(r0)**3)))
        DO ir = 1, nbin
          nr(ir) = (nc/((SQRT(2.*pi))*sigma*r(ir))) * EXP((-1.*(LOG(r(ir))-mu)**2)/(2.*(sigma**2))) &
                    * dr(ir)
        ENDDO
        write(*,*) "log-normal dist. in model (nc) =  ", sum(nr)
        write(*,*) "log-normal dist. const.   (nc) =  ", nc
        write(*,*) "accuracy  = ", (sum(nr)/nc) * 100., "%"

      CASE(2)

        ! Gamma distribution
        D = 2*r
        dD = 2*dr
        mu = (1.0e+9/nc) + 2.
        lambda = ((nc/qc)*(GAMMA(mu+4.)/GAMMA(mu+1.))*pi*(4./3.)*rho) ** (1./3.)
        !n0 = (nc*lamb**(mu+1.))/GAMMA(mu+1.)

        DO ir = 1, nbin
          !nr(ir) = n0*r(ir)^mu*EXP(-1*lamb*r(ir))
          nr(ir) = (nc/gamma(mu+1.)) * lambda*((lambda*D(ir))**mu) * exp(-1*lambda*D(ir))*dD(ir)
        ENDDO
        write(*,*) "gamma dist. in model (nc) =  ", sum(nr)
        write(*,*) "gamma dist. const.   (nc) =  ", nc
        write(*,*) "accuracy  = ", (sum(nr)/nc) * 100., "%"

    END SELECT

    ! DO ir = 1, nbin
    !   rr(ir) = dr(ir)*nr(ir)
    ! ENDDO
    !
    ! write(*,*) sum(rr)
    ! write(*,*) maxval(nr)

    ! conc(:,:)=0.
    END SUBROUTINE Sub_drop_distributions 

    !!---------------------------------------------!!
    !!---------------------------------------------!!
    SUBROUTINE hydrostatic_eq
    END SUBROUTINE hydrostatic_eq

    !!---------------------------------------------!!
    !!---------------------------------------------!!
    SUBROUTINE W_linear      
    END SUBROUTINE W_linear      

    !!---------------------------------------------!!
    !!---------------------------------------------!!
    SUBROUTINE W_exponantial
    END SUBROUTINE W_exponantial

END MODULE Mod_init
