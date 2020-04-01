MODULE Mod_dyn_driver

  USE Mod_global
  IMPLICIT NONE

  SUBROUTINE Sub_Finite_diff      &
             (                    &
               var, sfc_var,       &
               top_var,           &
               dz, nz,            &
               dt,                &
               w,                 &
               next_var           &
             )

    IMPLICIT NONE
    ! IN
    INTEGER,                    INTENT(IN)    :: dt
    INTEGER,                    INTENT(IN)    :: nz
    REAL,                       INTENT(IN)    :: sfc_var
    REAL,                       INTENT(IN)    :: top_var
    REAL,    DIMENSION(nz),     INTENT(IN)    :: var
    REAL,    DIMENSION(0:nz),   INTENT(IN)    :: w
    REAL,    DIMENSION(nz),     INTENT(IN)    :: dz
    ! Local
    INTEGER                                   :: i
    ! OUT
    REAL,    DIMENSION(nz),     INTENT(OUT)   :: next_var

    C(:) = w(:)*dt/dz(:)
    DO i = 1, nz
      IF ( i .eq. 1 ) THEN
        next_var(i) = var(i) - C(i)*(var(i+1)-sfc_var)
      ELSE IF ( i .eq. nz ) THEN
        next_var(i) = var(i) - C(i)*(top_var-var(i-1))
      ELSE
        next_var(i) = var(i) - C(i)*(var(i+1)-var(i-1))
      ENDIF
    ENDDO

  END SUBROUTINE Sub_Finite_diff

  !----------------------------------------------!
  SUBROUTINE Sub_Finite_volume    &
             (                    &
               var, sfc_var,      &
               dz, nz,            &
               dt,                &
               w,                 &
               next_var           &
             )

    IMPLICIT NONE
    ! IN
    INTEGER,                    INTENT(IN)    :: dt
    INTEGER,                    INTENT(IN)    :: nz
    REAL,                       INTENT(IN)    :: sfc_var
    REAL,    DIMENSION(nz),     INTENT(IN)    :: var
    REAL,    DIMENSION(0:nz),   INTENT(IN)    :: w
    REAL,    DIMENSION(nz),     INTENT(IN)    :: dz
    ! Local
    INTEGER                                   :: i
    REAL,    DIMENSION(nz)                    :: backward_flux_var
    REAL,    DIMENSION(nz)                    :: forward_flux_var
    ! OUT
    REAL,    DIMENSION(nz),     INTENT(OUT)   :: next_var

    DO i = 1, nz
      IF ( i .eq. 1 ) then
        backward_flux_var(i) = sfc_var*w(i)*dt
      ELSE
        backward_flux_var(i) = var(i-1)*w(i)*dt
      ENDIF
      forward_flux_var(i)  = var(i)*w(i+1)*dt
      next_var(i) = var(i) + (backward_flux_var(i)-forward_flux_var(i))/dz
    ENDDO
  END SUBROUTINE Sub_Finite_volume

END MODULE Mod_dyn_driver
