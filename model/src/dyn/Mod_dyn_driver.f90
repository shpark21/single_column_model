MODULE Mod_dyn_driver    

  USE Mod_global

  IMPLICIT NONE

  CONTAINS

  SUBROUTINE Sub_Integration

    DO it = 1, nt-1

      ext_Temp(it) = Temp(it, nz)  - ramma_dry * dz

      CALL Sub_FD_Method(nz, Temp(it+1,:), Temp(it,:), w(it,:), &
                         dt, sfc_Temp(it), ext_Temp(it), z(:), z_sfc, z_ext) 

    ENDDO !! time do  

  END SUBROUTINE Sub_Integration
  

  SUBROUTINE Sub_FD_Method(n_var,next_var, var, w, dt, sfc_var, ext_var, z, z_sfc, z_ext)
    IMPLICIT NONE
  
    INTEGER :: iz
    INTEGER, INTENT(IN) :: n_var
    INTEGER, INTENT(IN) :: dt
    REAL,    INTENT(IN) :: sfc_var, ext_var, z_sfc, z_ext
 
    REAL, DIMENSION(n_var),INTENT(IN) :: var 
    REAL, DIMENSION(n_var),INTENT(IN) :: w       
    REAL, DIMENSION(n_var),INTENT(IN) :: z       
    REAL, DIMENSION(n_var),INTENT(OUT) :: next_var

    DO iz = 1, n_var

      IF (iz .EQ. 1) THEN
        next_var(iz) = var(iz) - w(iz)*dt*((var(iz+1)-sfc_var)/(z(iz+1)-z_sfc))
      ELSE IF (iz .EQ. n_var) THEN
        next_var(iz) = var(iz) - w(iz)*dt*((ext_var-var(iz-1))/(z_ext-z(iz-1)))

      ELSE
        next_var(iz) = var(iz) - w(iz)*dt*((var(iz+1)-var(iz-1))/(z(iz+1)-z(iz-1)))
      ENDIF

      !PRINT*, var(+1, iz)
    ENDDO



  END SUBROUTINE Sub_FD_Method

  
END MODULE Mod_dyn_driver    
