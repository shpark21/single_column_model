  MODULE Mod_write

    USE NETCDF
    USE Mod_global

    IMPLICIT NONE
    
    CONTAINS

      !!---------------------------------------------!!
      SUBROUTINE Sub_write_netcdf ( nz, nt, dz, z_out,   &
                                    temp_out, q_out,     &
                                    w_out,               &
                                    out_path, out_name  )

        USE NETCDF
        IMPLICIT NONE
        ! In
        INTEGER,                INTENT(IN) ::    nz, nt
        REAL, DIMENSION(nz),    INTENT(IN) ::        dz,  &
                                                  z_out,  &
                                                  w_out
        REAL, DIMENSION(nz,nt), INTENT(IN) ::  temp_out,  &
                                                  q_out
        CHARACTER(LEN=*),       INTENT(IN) ::  out_path,  &
                                               out_name
        ! Local
        INTEGER                            ::      irec
        INTEGER                            ::       nnt

        nnt=nt+1

        CALL Sub_nc_attri 

        ! Create the file.
        CALL CHECK( nf90_create(trim(out_path)//"/"//trim(out_name),nf90_clobber,ncid) )
        CALL SUCCESS_MSG("Creation of new output data")

        ! Define the dimensions. The time dimension has no limit.
        CALL check( nf90_def_dim(ncid,    z%vname,             nz, lev_dimid) )
        CALL check( nf90_def_dim(ncid, time%vname, nf90_unlimited, rec_dimid) )
  
        ! The dimids array is used to pass the dimids of the dimension of
        ! the netCDF variables. In Fortran, the unlimited dimension
        ! must come last on the list of dimids.
        dimid2 = (/ lev_dimid, rec_dimid /)
        dimid1 = (/ lev_dimid /)

        ! Define "lev" and its attributes 
        CALL check( nf90_def_var(ncid, z%vname, nf90_double, lev_dimid, varid=varid) )
        CALL CHECK( nf90_put_att(ncid, 1, des,  z%desc) )
        CALL CHECK( nf90_put_att(ncid, 1,  un, z%units) )
        CALL CHECK( nf90_put_att(ncid, 1,  ax,  z%axis) )

        ! Define "time" and its attributes 
        CALL check( nf90_def_var(NCID, time%vname, nf90_double, rec_dimid, varid=varid) )
        CALL CHECK( nf90_put_att(ncid, 2,  un, time%units) )
        CALL CHECK( nf90_put_att(ncid, 2,  ax,  time%axis) )

        ! Define "W" and its attributes 
        CALL CHECK( nf90_def_var(ncid, w%vname, nf90_float, dimid1, varid=varid) )
        CALL CHECK( nf90_put_att(ncid, 3, des,  w%desc) )
        CALL CHECK( nf90_put_att(ncid, 3,  un, w%units) )

        ! Define "T" and its attributes 
        CALL CHECK( nf90_def_var(ncid, temp%vname, nf90_float, dimid2, varid=varid) )
        CALL CHECK( nf90_put_att(ncid, 4, des,   temp%desc) )
        CALL CHECK( nf90_put_att(ncid, 4,  un,  temp%units) )

        ! Define "Q" and its attributes 
        CALL CHECK( nf90_def_var(ncid, q%vname, nf90_float, dimid2, q%varid) )
        CALL CHECK( nf90_put_att(ncid, 5, des,  q%desc) )
        CALL CHECK( nf90_put_att(ncid, 5,  un, q%units) )

                ! End define mode.
        CALL CHECK( nf90_enddef(ncid) )

        ! These settings tell netCDF to write one timestep of data.
        dim1_count = (/ nz /)
        dim1_start = (/ 1  /)

        ! Write the pretend data.
        CALL CHECK( nf90_put_var(ncid, 1, z_out) )
        CALL SUCCESS_MSG("lev")
        CALL CHECK( nf90_put_var(ncid, 2,    nt) )
        CALL SUCCESS_MSG("time")

        CALL CHECK( nf90_put_var(ncid, 3, w_out, start=dim1_start, count=dim1_count) )
        CALL SUCCESS_MSG("w")

        dim2_count = (/ nz, nnt /)
        dim2_start = (/ 1,   1  /)
        do irec = 1, nnt
          dim2_start(2) = irec
          dim2_count(2) = irec

          CALL CHECK( nf90_put_var(ncid, 4, temp_out(:,irec), start=dim2_start, count=dim2_count) )
          !CALL SUCCESS_MSG("T")
        
          CALL CHECK( nf90_put_var(ncid, 5, q_out(:,irec), start=dim2_start, count=dim2_count) )
          !CALL SUCCESS_MSG("Q")

        end do
        CALL SUCCESS_MSG("T")
        CALL SUCCESS_MSG("Q")
        ! Close the file. This causes netCDF to flush all buffers.
        CALL CHECK( nf90_close(ncid) ) 



      END SUBROUTINE Sub_write_netcdf


      !!---------------------------------------------!!
      SUBROUTINE Sub_write_bin   
      END SUBROUTINE Sub_write_bin   

      !!---------------------------------------------!!
      SUBROUTINE Sub_write_grib  
      END SUBROUTINE Sub_write_grib  

  END MODULE Mod_write
