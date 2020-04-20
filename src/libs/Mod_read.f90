MODULE Mod_read  

  USE NETCDF
  USE Mod_global

  IMPLICIT NONE

  NAMELIST /Time_control/ integrated_time, &
                          output_interval 

  NAMELIST /Domain      / input_nz, &
                          nz,       &
                          z_top

  NAMELIST /Dyn_options/ gamma_dry,     &
                         dyn_option,    &
                         dz_option,     &
                         dzr

  NAMELIST /Phys_options/ dist_option,       &
                          drop_column_num,   &
                          drop_min_diameter, &
                          drop_max_diameter

  NAMELIST /file_info/ in_path,     &
                       t_in_name,   &
                       q_in_name,   &
                       w_in_name,   &
                       z_in_name,   &
                       output_path, &
                       output_name


    CONTAINS

 !!---------------------------------------------!!
      !!---Sub_name : Sub_read_namelist      --------!!
      !!---Input var : Global vars           --------!!
      !!---Ouput var : Global vars           --------!!
      !!---What is that : For reading namelist ------!!
      !!---------------------------------------------!!
    SUBROUTINE Sub_read_namelist

      IMPLICIT NONE

      OPEN(10,FILE='./namelist.info', iostat=ionum)

      IF ( ionum .ne. 0 ) CALL FAIL_MSG("CHECKor namelist")
      
      READ(10, Time_control )
      READ(10, Domain       )
      READ(10, Dyn_options  )
      READ(10, Phys_options )
      READ(10, file_info    )

    END SUBROUTINE Sub_read_namelist


    SUBROUTINE Sub_read_netcdf ( var_in, in_path, in_name )

    IMPLICIT NONE

    !In
    REAL, DIMENSION(:),       INTENT(IN)   ::    var_in
    CHARACTER(LEN=*),         INTENT(IN)   ::   in_path, &
                                                in_name 
    !Local
    INTEGER :: ndims_in, nvars_in, ngatts_in, unlimdimid_in
    INTEGER :: lev_varid, var_varid, varid, len, ncid
    INTEGER :: nx, ny, ndim(4), nlon, nlat, nlev, ntime
    character(len=20) :: dim_names(3), dim_name, &
                         lon_units, lat_units, var_units, &
                         lon_name, lat_name, time_name, time_units, &
                         var_name


    ! IF (.NOT. ALLOCATED( var%din) ) ALLOCATE( var%din (nz_in,lat_in,lon_in) ) 
 
      CALL CHECK( nf90_open(trim(in_path)//"/"//trim(in_name),nf90_clobber,ncid) )
      CALL SUCCESS_MSG("Open in data")
  
      CALL CHECK( NF90_INQUIRE(ncid, ndims_in, nvars_in, ngatts_in, unlimdimid_in) )
         print*, ncid, ndims_in, nvars_in, ngatts_in, unlimdimid_in 

      ! CALL CHECK( NF90_INQ_VARID(ncid, "level", lev_varid) )
      ! CALL CHECK( NF90_INQ_VARID(ncid, "W",     var_varid) ) 


    END SUBROUTINE Sub_read_netcdf

END MODULE Mod_read  
