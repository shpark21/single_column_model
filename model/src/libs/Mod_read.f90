  MODULE Mod_read  

    USE Mod_global
  
    IMPLICIT NONE

    NAMELIST /Time_control/ dt,       &
                            nt

    NAMELIST /Domain      / dz,       &
                            z_top,    &
                            z_sfc

    NAMELIST /Other_options/ ramma_dry

    NAMELIST /file_info/ T_output_file_path, &
                         T_output_file_name, &
                         q_output_file_path, &
                         q_output_file_name



      CONTAINS

      !!---------------------------------------------!!
      !!---Sub_name : Sub_read_namelist      --------!!
      !!---Input var : Global vars           --------!!
      !!---Ouput var : Global vars           --------!!
      !!---What is that : For reading namelist ------!!
      !!---------------------------------------------!!
      SUBROUTINE Sub_read_namelist

        IMPLICIT NONE

        OPEN(10, FILE='/home/gihh1131/HOMEWORK/MICROPHYSICS/single_column_model/model/run/namelist.info')

        READ(10, Time_control)
        READ(10, Domain      )
        READ(10, Other_options)
        READ(10, file_info)

      END SUBROUTINE Sub_read_namelist

      !!---------------------------------------------!!
      !!---Sub_name :                        --------!!
      !!---Input var :                       --------!!
      !!---Ouput var :                       --------!!
      !!---What is that :                    --------!!
      !!---------------------------------------------!!
      SUBROUTINE Sub_read_T  

      END SUBROUTINE Sub_read_T  
     
      !!---------------------------------------------!!
      !!---Sub_name :                        --------!!
      !!---Input var :                       --------!!
      !!---Ouput var :                       --------!!
      !!---What is that :                    --------!!
      !!---------------------------------------------!!
      SUBROUTINE Sub_read_qv   

      END SUBROUTINE Sub_read_qv   


  END MODULE Mod_read  
