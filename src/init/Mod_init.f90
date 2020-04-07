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

      ! IF (it = 0) THEN
      !   W(1:nz) = 1.0 !! m/s
      !   W(nz+1) = 0.0 !! m/s
      ! ELSE
      !
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
      !
      ! ENDIF !! about it

      w%dz(:) = 1. 
      w%dz(nz) = 0. 

    END SUBROUTINE Sub_Cal_W  
   
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