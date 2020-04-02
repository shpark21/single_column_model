MODULE Mod_dyn_driver

  USE Mod_global
  IMPLICIT NONE

  CONTAINS

  SUBROUTINE Sub_Finite_diff      &
             (                    &
               var, sfc_var,      &
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

    DO i = 1, nz
      IF ( i .eq. 1 ) THEN
        next_var(i) = var(i) - w(i)*dt*(var(i+1)-sfc_var)/(2*dz(i))
      ELSE IF ( i .eq. nz ) THEN
        next_var(i) = var(i) - w(i)*dt*(top_var-var(i-1))/(2*dz(i))
      ELSE
        next_var(i) = var(i) - w(i)*dt*(var(i+1)-var(i-1))/(dz(i)+dz(i+1))
      ENDIF
    ENDDO

  END SUBROUTINE Sub_Finite_diff

  !----------------------------------------------!
  SUBROUTINE Sub_Finite_volume    &
             (                    &
               var, sfc_var,      &
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
    REAL,    ALLOCATABLE                      :: backward_flux_var(:)
    REAL,    ALLOCATABLE                      :: forward_flux_var(:)
    ! OUT
    REAL,    DIMENSION(nz),     INTENT(OUT)   :: next_var

    IF(.NOT. ALLOCATED( backward_flux_var )) ALLOCATE(backward_flux_var(nz))
    IF(.NOT. ALLOCATED( forward_flux_var  )) ALLOCATE(forward_flux_var (nz))

    DO i = 1, nz
      
      IF ( i .eq. 1 ) then  
        backward_flux_var(i) = sfc_var*w(i-1)*dt  
      ELSE
        backward_flux_var(i) = var(i-1)*w(i-1)*dt
      ENDIF
      IF ( i .eq. nz ) then
        forward_flux_var(i)  = top_var*w(i)*dt
      ELSE
        forward_flux_var(i)  = var(i)*w(i)*dt
      ENDIF
      next_var(i) = var(i) + (backward_flux_var(i)-forward_flux_var(i))/dz(i)
      IF ( next_var(i) .lt. 0. ) THEN
        CALL FAIL_MSG("ERROR :: dynamics, Physical quantity cannot be negative.check 'dt', 'w'")
      ENDIF
    ENDDO

    IF (ALLOCATED(backward_flux_var ))  DEALLOCATE(backward_flux_var )
    IF (ALLOCATED(forward_flux_var  ))  DEALLOCATE(forward_flux_var  )

  END SUBROUTINE Sub_Finite_volume

END MODULE Mod_dyn_driver
