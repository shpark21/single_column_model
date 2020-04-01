PROGRAM main_prog

  USE Mod_global
  USE Mod_read
  USE Mod_init
  USE Mod_dyn_driver    
  USE Mod_integration
  ! USE Mod_write
  !
  IMPLICIT NONE
  CALL Sub_read_namelist

  CALL Sub_allocate
  
  CALL Sub_set_grid
  CALL Sub_Cal_W
  CALL Sub_Set_T

  q(:)=20.
  sfc_q(:)=0.
  top_q(:)=0.
  IF (dyn_option .eq. 1) THEN
    CALL Sub_Integration_FD
  ELSE IF (dyn_option .eq. 2) THEN
    CALL Sub_Integration_FV
  ENDIF

  ! CALL Sub_write_ascii(nt, nz, Temp, T_output_file_path, T_output_file_name)

END PROGRAM main_prog
