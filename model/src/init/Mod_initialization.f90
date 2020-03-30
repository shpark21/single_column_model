  MODULE Mod_initialization

    USE Mod_global
  
    IMPLICIT NONE


      CONTAINS

      !!---------------------------------------------!!
      !!---Sub_name :                        --------!!
      !!---Input var :                       --------!!
      !!---Ouput var :                       --------!!
      !!---What is that :                    --------!!
      !!---------------------------------------------!!
      SUBROUTINE Sub_Cal_P 


!        IF (it = 0) THEN 
!          CALL Sub_read_T
!          CALL Sub_read_qv
!        ELSE
!       
!          CALL hydrostatic_eq !! sub?? function?? need discussion    
!
!        ENDIF


      END SUBROUTINE Sub_Cal_P 

      !!---------------------------------------------!!
      !!---Sub_name :                        --------!!
      !!---Input var :                       --------!!
      !!---Ouput var :                       --------!!
      !!---What is that :                    --------!!
      !!---------------------------------------------!!
      SUBROUTINE Sub_set_grid

      IMPLICIT NONE

        DO iz = 1, nz
          IF (iz .EQ. 1) THEN
            z(iz) = dz
          ELSE
            z(iz) = z(iz-1) + dz
          ENDIF
        ENDDO !! z

      END SUBROUTINE Sub_set_grid

      !!---------------------------------------------!!
      !!---Sub_name :                        --------!!
      !!---Input var :                       --------!!
      !!---Ouput var :                       --------!!
      !!---What is that :                    --------!!
      !!---------------------------------------------!!
      SUBROUTINE Sub_Set_T

        IMPLICIT NONE

        !! boundary condition
        DO it = 1, nt
          sfc_Temp(it) = 20. + 273.5
        ENDDO !! time do

        !! initial  condition
        DO iz = 1, nz
          IF (iz .EQ. 1) THEN
            Temp(1, iz) = sfc_Temp(1) - ramma_dry * dz
          ELSE
            Temp(1, iz) = Temp(1, iz-1) - ramma_dry * dz
          ENDIF !! iz = 1 & others
        ENDDO !! Temp from 1 to nz


      END SUBROUTINE Sub_Set_T




      !!---------------------------------------------!!
      !!---Sub_name : Sub_cal_W              --------!!
      !!---Input var : it, nt, nz            --------!!
      !!---Ouput var : w                     --------!!
      !!---What is that : To set W feild     --------!!
      !!---------------------------------------------!!
      SUBROUTINE Sub_Cal_W

        IMPLICIT NONE

!        IF (it = 0) THEN
!          W(1:nz) = 1.0 !! m/s
!          W(nz+1) = 0.0 !! m/s
!        ELSE
!
!          SELECT CASE (W_opt)
!
!            CASE ("linear")
!              CALL W_linear
!            CASE ("exp")
!              CALL W_exponantial
!            DEFAULT
!              CALL W_linear
!
!          ENDSELECT !! about W_opt

!        ENDIF !! about it

        DO it = 1, nt
          W(it, :) = -1.0*REAL(it)/REAL(nt-1) + (1.0 + 1.0/REAL(nt-1))
        ENDDO !! W

      END SUBROUTINE Sub_Cal_W  
     
      !!---------------------------------------------!!
      !!---Sub_name :                        --------!!
      !!---Input var :                       --------!!
      !!---Ouput var :                       --------!!
      !!---What is that :                    --------!!
      !!---------------------------------------------!!
      SUBROUTINE hydrostatic_eq

      END SUBROUTINE hydrostatic_eq

      !!---------------------------------------------!!
      !!---Sub_name :                        --------!!
      !!---Input var :                       --------!!
      !!---Ouput var :                       --------!!
      !!---What is that :                    --------!!
      !!---------------------------------------------!!
      SUBROUTINE W_linear      

      END SUBROUTINE W_linear      


      !!---------------------------------------------!!
      !!---Sub_name :                        --------!!
      !!---Input var :                       --------!!
      !!---Ouput var :                       --------!!
      !!---What is that :                    --------!!
      !!---------------------------------------------!!
      SUBROUTINE W_exponantial

      END SUBROUTINE W_exponantial



  END MODULE Mod_initialization
