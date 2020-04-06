PROGRAM main_prog

  USE NETCDF
  USE Mod_global
  USE Mod_read
  USE Mod_init
  USE Mod_dyn_driver    
  USE Mod_integration
  USE Mod_write
  !
  IMPLICIT NONE
  CALL Sub_read_namelist

  CALL Sub_allocate
  
  CALL Sub_set_grid
  CALL Sub_Cal_W
  CALL Sub_Set_T

  q%dz(20)=100.
  q%sfc_dt(:)=0.
  q%top_dt(:)=0.

  IF (dyn_option .eq. 1) THEN
    CALL Sub_Integration_FD
  ELSE IF (dyn_option .eq. 2) THEN
    CALL Sub_Integration_FV
  ELSE IF (dyn_option .eq. 3) THEN
    CALL Sub_Integration_PPM
  ENDIF

   write(*,*) sum(q%dout(:,2))
   write(*,*) sum(q%dout(:,300))

  CALL Sub_write_netcdf ( nz, nt, dz%dz, z%dz,      &
                          temp%dout, q%dout,        &
                          w%dz(1:nz),               &
                          output_path, output_name )     


  !CALL Sub_deallocate
END PROGRAM main_prog
