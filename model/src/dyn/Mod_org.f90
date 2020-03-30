MODULE Mod_dyn_driver    

  USE Mod_global

  IMPLICIT NONE

  CONTAINS

  SUBROUTINE Sub_Integration

    DO it = 1, nt-1

      ext_Temp(it) = Temp(it, nz)  - ramma_dry * dz

      CALL Sub_FD_Method

    ENDDO !! time do  

  END SUBROUTINE Sub_Integration
  

  SUBROUTINE Sub_FD_Method
    IMPLICIT NONE

    DO iz = 1, nz

      IF (iz .EQ. 1) THEN
        Temp(it+1, iz) = Temp(it, iz) - W(it,iz)*dt*((Temp(it,iz+1)-sfc_Temp(it))/(z(iz+1)-z_sfc))
      ELSE IF (iz .EQ. nz) THEN
        Temp(it+1, iz) = Temp(it, iz) - W(it,iz)*dt*((ext_Temp(it)-Temp(it,iz-1))/(z_ext-z(iz-1)))

      ELSE
        Temp(it+1, iz) = Temp(it, iz) - W(it,iz)*dt*((Temp(it,iz+1)-Temp(it,iz-1))/(z(iz+1)-z(iz-1)))
      ENDIF

      !PRINT*, Temp(it+1, iz)
    ENDDO



  END SUBROUTINE Sub_FD_Method

  
END MODULE Mod_dyn_driver    
