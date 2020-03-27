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


        IF (it = 0) THEN 
          CALL Sub_read_T
          CALL Sub_read_qv
        ELSE
       
          CALL hydrostatic_eq !! sub?? function?? need discussion    

        ENDIF


      END SUBROUTINE Sub_Cal_P 

      !!---------------------------------------------!!
      !!---Sub_name :                        --------!!
      !!---Input var :                       --------!!
      !!---Ouput var :                       --------!!
      !!---What is that :                    --------!!
      !!---------------------------------------------!!
      SUBROUTINE Sub_Cal_W


        IF (it = 0) THEN
          W(1:nz) = 1.0 !! m/s
          W(nz+1) = 0.0 !! m/s
        ELSE

          SELECT CASE (W_opt)

            CASE ("linear")
              CALL W_linear
            CASE ("exp")
              CALL W_exponantial
            DEFAULT
              CALL W_linear

          ENDSELECT !! about W_opt

        ENDIF !! about it

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
