MODULE Mod_read  

  USE Mod_global
  USE netcdf     

  IMPLICIT NONE

  NAMELIST /Time_control/ dt,              &
                          nt,              &
                          output_interval 

  NAMELIST /Domain      / nz,       &
                          z_top

  NAMELIST /Dyn_Options/ gamma_dry,     &
                         dyn_option,    &
                         dz_option,     &
                         dzr

  NAMELIST /Phys_options/ dist_option,      &
                          drop_column_num,  &
                          drop_1st_diameter,&
                          drop_ratio,       &
                          Nc,               &
                          qc

  NAMELIST /file_info/ output_path, &
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

      IF ( ionum .ne. 0 ) CALL FAIL_MSG("error namelist")
      
      READ(10, Time_control)
      READ(10, Domain      )
      READ(10, Dyn_options )
      READ(10, Phys_options)
      READ(10, file_info   )

    END SUBROUTINE Sub_read_namelist

    !!---------------------------------------------!!
    !!---Sub_name :                        --------!!
    !!---Input var :                       --------!!
    !!---Ouput var :                       --------!!
    !!---What is that :                    --------!!
    !!---------------------------------------------!!
    SUBROUTINE Sub_read_NC_file(infile, out_var, lat_iy, lon_ix)

      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)                   :: infile
      REAL, INTENT(IN)                               :: lat_iy, &
                                                        lon_ix
      REAL, ALLOCATABLE, DIMENSION(:), INTENT(INOUT) :: out_var

      !! for reading nc file
      INTEGER               :: ncid
      CHARACTER(LEN=50)     :: zname, xname, yname, tname, vname
      INTEGER               :: nz, nx, ny, nt
      INTEGER, DIMENSION(4) :: dim_count, dim_start 
      INTEGER, DIMENSION(4) :: dimids
      INTEGER               :: xtype,ndims 

      REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: var
     
      IF (ALLOCATED(out_var)) DEALLOCATE(out_var)

      CALL check(nf90_open(infile, nf90_nowrite, ncid))
      CALL check(nf90_inquire_dimension(ncid, 1, tname, nt))
      CALL check(nf90_inquire_dimension(ncid, 2, zname, nz))
      CALL check(nf90_inquire_dimension(ncid, 3, xname, nx))
      CALL check(nf90_inquire_dimension(ncid, 4, yname, ny))

      !!!!-----------------------------  var(ny, nx, nz, nt)
      IF (.NOT. ALLOCATED(var)) ALLOCATE(var (1 , 1 , nz, 1 ))
      IF (.NOT. ALLOCATED(out_var)) ALLOCATE(out_var (nz))


      dim_count =(/  1    ,   1    , nz, 1 /)
      dim_start =(/lat_iy , lon_ix , 1 , 1 /)

!      CALL check(nf90_inq_varid(ncid, 'W'  , varid))
!      CALL check(nf90_get_var(ncid, 5, var(1,1,:,:),start=dim_start, count=dim_count))

      CALL check(nf90_inquire_variable(ncid,1,vname,xtype,ndims,dimids))
      CALL check(nf90_inq_varid(ncid, vname ,varid))
      CALL check(nf90_get_var(ncid,varid,var,start=dim_start, count=dim_count))

      out_var(:) = var(1,1,:,1)

    END SUBROUTINE Sub_read_NC_file
   
    !!---------------------------------------------!!
    !!---Sub_name :                        --------!!
    !!---Input var :                       --------!!
    !!---Ouput var :                       --------!!
    !!---What is that :                    --------!!
    !!---------------------------------------------!!
    SUBROUTINE Sub_read_qv   

    END SUBROUTINE Sub_read_qv   


END MODULE Mod_read  
