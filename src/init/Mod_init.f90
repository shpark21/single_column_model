MODULE Mod_init

  USE Mod_global

  IMPLICIT NONE

    CONTAINS

    ! SUBROUTINE Sub_Cal_P 
    !   IF (it = 0) THEN 
    !     CALL Sub_read_T
    !     CALL Sub_read_qv
    !   ELSE
    !  
    !     CALL hydrostatic_eq !! sub?? function?? need discussion    
    !
    !   ENDIF
    ! END SUBROUTINE Sub_Cal_P 

    !!---------------------------------------------!!
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
    !!---------------------------------------------!!
    SUBROUTINE Sub_Set_T

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

    END SUBROUTINE Sub_Set_T

    !!---------------------------------------------!!
    !!---------------------------------------------!!
    SUBROUTINE Sub_Cal_W

    IMPLICIT NONE

      ! w%dz(0:nz-1) = 1.0 !! m/s
      ! w%dz(nz)     = 0.0 !! m/s

      w%dz(0:nz/2) = 1.0 !! m/s
      w%dz(1+nz/2:nz)     = -1.0 !! m/s

      !   SELECT CASE (W_opt)
      !
      !     CASE ("linear")
      !       CALL W_linear
      !     CASE ("exp")
      !       CALL W_exponantial
      !     DEFAULT
      !       CALL W_linear
      !
      !   ENDSELECT !! about W_opt

    END SUBROUTINE Sub_Cal_W  
   
    !!---------------------------------------------!!
    !!---------------------------------------------!!
    SUBROUTINE Sub_drop_distributions     &
               (                          &
                distribution_option,      &
                column_num,               & !!  drop_column_num
                rmin,                     & !!  drop_1st_diameter
                ratio,                    & !!  drop_ratio
                nz,                       & 
                nr,                       & !!  drop_num
                conc                      & !!  drop_conc
               )

    IMPLICIT NONE

    ! IN
    INTEGER, INTENT(IN) :: distribution_option, &
                           column_num,          &
                           nz                       
    REAL,    INTENT(IN) :: rmin,                &
                           ratio
    
    ! Local
    REAL, DIMENSION(column_num)              :: r,  &
                                                m 
    REAL, DIMENSION(column_num+1)            :: rb, &
                                                mb, rr, dr
    INTEGER                                  :: ir, nbin
    REAL                                     :: rmax, &
                                                pi,   &
                                                r0, sigma, n0
    ! OUT
    REAL, DIMENSION(nz,column_num), INTENT(OUT) :: conc
    REAL, DIMENSION(column_num),    INTENT(OUT) :: nr

    pi = 4*ATAN(1.)
    nbin=column_num

    rmax  = rmin*(ratio**nbin) 

    rb(1) = rmin ; rb(nbin+1) = rmax        ! boundary of drop diameter
    DO ir = 2, nbin                         ! |
      rb(ir) = ratio*rb(ir-1)               ! V 
    ENDDO                                   ! [um***3 => kg]    
                                            ! boundary of mass
    mb(:) = rb(:)**3                        ! |
    DO ir = 1, nbin                         ! V  
      m(ir)=(mb(ir)+mb(ir+1))/2.            ! mass 
      dr(ir) = rb(ir+1)-rb(ir)
    ENDDO                                   ! |
                                            ! V
    r = m**(1./3.)                          ! drop diameter

    do ir = 1, nbin
    r(ir) = ir-1 
    enddo

    write(*,*) r

    SELECT CASE(distribution_option)
      CASE(1)
      ! Fitting Log-normal distribution
      r0    = 0. 
      sigma = 1.
      n0    = 1.

      DO ir = 1, nbin
       nr(ir) = (N0/(SQRT(2*pi)*sigma*r(ir))) * EXP(((log(r(ir))-r0)**2)/(2.*(sigma**2))*-1.)
      ENDDO
    END SELECT

    ! DO ir = 1, nbin
    !   rr(ir) = dr(ir)*nr(ir)
    ! ENDDO
    !
    ! write(*,*) sum(rr)
    ! write(*,*) maxval(nr)

    conc(:,:)=0.
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
