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
        REAL, DIMENSION(nt,nz), INTENT(IN) ::  temp_out,  &
                                                  q_out
        CHARACTER(LEN=*),       INTENT(IN) ::  out_path,  &
                                               out_name
        ! Local
        INTEGER                            ::      irec
        INTEGER                            ::      nnt

        nnt=nt+1

        CALL Sub_nc_attri 

        ! Create the file.
        CALL CHECK( nf90_create(trim(out_path)//"/"//trim(out_name),nf90_clobber,ncid) )
        CALL SUCCESS_MSG("Creation of new output data")

        ! Define the dimensions. The time dimension has no limit.
        CALL CHECK( nf90_def_dim(ncid,rc_name,nf90_unlimited,rec_dimid) )
        CALL CHECK( nf90_def_dim(ncid,z%vname,nz,lev_dimid) )

        ! The dimids array is used to pass the dimids of the dimension of
        ! the netCDF variables. In Fortran, the unlimited dimension
        ! must come last on the list of dimids.
        dimid2 = (/ lev_dimid, rec_dimid /)
        dimid1 = (/ lev_dimid /)

        ! Define "lev" and its attributes 
        CALL CHECK( nf90_def_var(ncid, z%vname, nf90_float, lev_dimid, z%varid) )
        CALL CHECK( nf90_put_att(ncid, z%varid, des,  z%desc) )
        CALL CHECK( nf90_put_att(ncid, z%varid,  un, z%units) )
        CALL CHECK( nf90_put_att(ncid, z%varid,  ax,  z%axis) )

        ! Define "W" and its attributes 
        CALL CHECK( nf90_def_var(ncid, w%vname, nf90_float, dimid1, w%varid) )
        CALL CHECK( nf90_put_att(ncid, w%varid, des,  w%desc) )
        CALL CHECK( nf90_put_att(ncid, w%varid,  un, w%units) )

        ! Define "T" and its attributes 
        CALL CHECK( nf90_def_var(ncid, temp%vname, nf90_float, dimid2, temp%varid) )
        CALL CHECK( nf90_put_att(ncid, temp%varid, des,   temp%desc) )
        CALL CHECK( nf90_put_att(ncid, temp%varid,  un,  temp%units) )

        ! Define "Q" and its attributes 
        CALL CHECK( nf90_def_var(ncid, q%vname, nf90_float, dimid2, q%varid) )
        CALL CHECK( nf90_put_att(ncid, q%varid, des,  q%desc) )
        CALL CHECK( nf90_put_att(ncid, q%varid,  un, q%units) )

                ! End define mode.
        CALL CHECK( nf90_enddef(ncid) )

        ! These settings tell netCDF to write one timestep of data.
        dim1_count = (/ nz /)
        dim1_start = (/ 1  /)

        ! Write the pretend data.
        !CALL CHECK( nf90_put_var(ncid, z%varid, z_out, start=dim1_start, count=dim1_count) )
        CALL CHECK( nf90_put_var(ncid, z%varid, z_out) )
        CALL SUCCESS_MSG("z")

        CALL CHECK( nf90_put_var(ncid, w%varid, w_out, start=dim1_start, count=dim1_count) )
        CALL SUCCESS_MSG("w")

        dim2_count = (/ nnt, nz /)
        dim2_start = (/ 1,   1  /)
        do irec = 1, nnt

          dim2_start(2) = irec
          dim2_count(2) = irec

          CALL CHECK( nf90_put_var(ncid, temp%varid, temp_out(irec,:), start=dim2_start, count=dim2_count) )
          !CALL SUCCESS_MSG("T")
        
          CALL CHECK( nf90_put_var(ncid, q%varid, q_out(irec,:), start=dim2_start, count=dim2_count) )
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
