PROGRAM main_prog

  USE NETCDF
  USE Mod_global
  USE Mod_const
  USE Mod_read
  USE Mod_init
  USE Mod_dyn_driver    
  USE Mod_phys_driver    
  USE Mod_integration
  USE Mod_write
  
  IMPLICIT NONE

  CALL Sub_read_namelist
  CALL Sub_allocate_dz
  CALL Sub_set_grid

  CALL Sub_read_NC_file( in_path, z_in_name, &
                                      z%din, &
                            slat,      elat, &
                            slon,      elon   )
  CALL Sub_read_NC_file( in_path, t_in_name, &
                                   temp%din, &
                            slat,      elat, &
                            slon,      elon   )
  CALL Sub_read_NC_file( in_path, q_in_name, &
                                      q%din, &
                            slat,      elat, &
                            slon,      elon   )
  w%dz(1:49)=1.
  w%dz(51:100)=-1.

  CALL Sub_set_W ( nz , dz%dz , w%dz , w%stag_dz )
  CALL Sub_set_dt
  CALL Sub_allocate_dt
  CALL Sub_set_T
  CALL Sub_Set_Q




  IF (dyn_option .eq. 1) THEN
    CALL Sub_Integration_FD
  ELSE IF (dyn_option .eq. 2) THEN
    CALL Sub_Integration_FV
  ELSE IF (dyn_option .eq. 3) THEN
    CALL Sub_Integration_PPM
  ENDIF

  
  CALL Sub_drop_distributions (                    &
                               dist_option,        &
                               drop_column_num,    &
                               drop_min_diameter,  &
                               drop_max_diameter,  &
                               drop%num(:,1)       &
                              )

    ! open(unit = 20, file = "r.bin", status = "unknown", &
    !       form="unformatted",access="direct", recl=4*drop_column_num) 
    !
    ! write(20,rec=1) drop_num%dn(:)

   write(*,*) "it =            2", " total Q =",  sum(q%dout(:,2))
   write(*,*) "it = ",         nt,  "total Q =", sum(q%dout(:,nt))

  CALL Sub_write_netcdf ( nz, nt, dz%dz, z%dz,      &
                          temp%dout, q%dout,        &
                          w%dz(1:nz),               &
                          output_path, output_name )     


  CALL Sub_deallocate 

END PROGRAM main_prog
