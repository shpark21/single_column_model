MODULE Mod_init

  USE Mod_global

  IMPLICIT NONE

    CONTAINS
    
    SUBROUTINE Sub_init

    IMPLICIT NONE

      CALL Sub_allocate_dz
      CALL Sub_set_grid

      w%dz(1:49)=1.

      w%dz(51:100)=-1.
      CALL Sub_set_W ( nz , dz%dz , w%dz , w%stag_dz ) 

      CALL Sub_set_dt
      CALL Sub_allocate_dt

      CALL Sub_set_T 
      CALL Sub_Set_Q 

    END SUBROUTINE Sub_init


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

      write(*,*) "DT    =  ", dt
      write(*,*) "NT    =  ", nt

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
                column_num,               & !!  drop_column_num
                rmin,                     & !!  drop_1st_diameter
                ratio,                    & !!  drop_ratio
                nz,                       & 
                nr,                       & !!  drop_num
                conc                      & !!  zx op_conc
               )

    IMPLICIT NONE

    ! IN
    INTEGER, INTENT(IN) :: distribution_option, &
                           column_num,          &
                           nz                       
    REAL,    INTENT(IN) :: rmin,                &
                           rmax
    
    ! Local
    REAL, DIMENSION(column_num)              :: r,  &
                                                D 
    REAL, DIMENSION(column_num+1)            :: rb, &
                                                dr, rr, dD
    INTEGER                                  :: ir, nbin
    REAL                                     :: ratio, &
                                                nc,    &
                                                qc,    &
                                                pi,    &
                                                rho,   &
                                                r0, mu, sigma, lambda
    ! OUT
    REAL, DIMENSION(nz,column_num), INTENT(OUT) :: conc
    REAL, DIMENSION(column_num),    INTENT(OUT) :: nr

    nc = 1.0e8                              ! [# / m^3]
    qc = 0.001                              ! [kg / kg]
    pi = 4. * ATAN(1.)
    
    nbin = column_num
    rmin = drop_1st_diameter                ! [m ; 1 um]
    rmax = drop_last_diameter               ! [m ; 1 cm] 
    ratio = (rb(nbin)/rb(1))**(1./nbin)

    rb(1) = rmin ; rb(nbin+1) = rmax        ! boundary of drop diameter
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
      r0 = 1.0e-5
      mu = LOG(r0)
      sigma = SQRT((2./9.)*LOG(qc/(nc*rho*(4./3.)*pi*(r0)^3)))
      D = 2*r
      dD = 2*dr

      DO ir = 1, nbin
       nr(ir) = (nc/((SQRT(2.*pi))*sig*r(ir)))*EXP((-1.*(LOG(r(ir))-mu)^2)/(2.*sig^2))
      ENDDO

      CASE(2)
      ! Gamma distribution
      mu = (1.0e9/nc) + 2.
      lambda = ((nc/qc)*(GAMMA(mu+4.)/GAMMA(mu+1.))*pi*(4./3.)*rho)**(1./3.)
      !n0 = (nc*lamb**(mu+1.))/GAMMA(mu+1.)

      DO ir = 1, nbin
       !nr(ir) = n0*r(ir)^mu*EXP(-1*lamb*r(ir))
       nr(ir) = (nc/gamma(mu+1.))*lamb*(lamb*D(ir))^mu*exp(-1*lamb*D(ir))*dD(ir)
      ENDDO
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
