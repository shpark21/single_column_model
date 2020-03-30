  PROGRAM main_program

    USE Mod_global
    USE Mod_read  
    USE Mod_initialization
    USE Mod_integration
    USE Mod_write 

    IMPLICIT NONE

      !!------1. setting global variable--------!!
      !!------2. read namelist          --------!!

      CALL Sub_read_namelist
      CALL Sub_allocate 

      !!------3. initialization process --------!!
      !!---to determinate dz            --------!!
      !!---to read T, q                 --------!!
      !!---to calculate  p, w           --------!!
      CALL Sub_read_T
      CALL Sub_read_qv
      CALL Sub_Cal_P
      CALL Sub_Cal_W

      !!------4. integration process    --------!!
      !!---including dynamics process and microphysics process   --------!!
      CALL Sub_Integration


      !!------5. write output file      --------!!
      CALL Sub_write

      CALL Sub_deallocate

  END PROGRAM main_program
