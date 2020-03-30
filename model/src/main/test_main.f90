PROGRAM test

  USE Mod_global
  USE Mod_read
  USE Mod_initialization
  USE Mod_dyn_driver    
  USE Mod_write

  IMPLICIT NONE

  CALL Sub_read_namelist

  nz = INT((z_top-z_sfc)/dz) 
  z_ext = z_top + dz

  CALL Sub_allocate

  CALL Sub_set_grid
  CALL Sub_Cal_W
  CALL Sub_Set_T

  CALL Sub_Integration

  CALL Sub_write_ascii(nt, nz, Temp, T_output_file_path, T_output_file_name)

END PROGRAM test
