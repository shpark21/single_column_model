  MODULE Mod_integration

    USE Mod_global
  
    IMPLICIT NONE



      CONTAINS

      !!---------------------------------------------!!
      !!---Sub_name :                        --------!!
      !!---Input var :                       --------!!
      !!---Ouput var :                       --------!!
      !!---What is that :                    --------!!
      !!---------------------------------------------!!
      SUBROUTINE Sub_Integration


        DO it = 1, nt

          CALL Sub_Finite_volume
          CALL Sub_Cal_P
          CALL Sub_Cal_W

        ENDDO !! time do
        !CALL cloud_pysics

      END SUBROUTINE Sub_Integration

      !!---------------------------------------------!!
      !!---Sub_name :                        --------!!
      !!---Input var :                       --------!!
      !!---Ouput var :                       --------!!
      !!---What is that :                    --------!!
      !!---------------------------------------------!!
      SUBROUTINE Sub_Finite_volume



      END SUBROUTINE Sub_Finite_volume





  END MODULE Mod_integration
