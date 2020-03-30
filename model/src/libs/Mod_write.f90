  MODULE Mod_write

    USE Mod_global

    IMPLICIT NONE
    
    CONTAINS

      SUBROUTINE Sub_write_netcdf

      END SUBROUTINE Sub_write_netcdf

      SUBROUTINE Sub_write_bin   

      END SUBROUTINE Sub_write_bin   

      SUBROUTINE Sub_write_grib  

      END SUBROUTINE Sub_write_grib  


   
      SUBROUTINE Sub_write_ascii(nt, nz, var, file_path, file_name)

        IMPLICIT NONE
        INTEGER, INTENT(IN) :: nt, nz
        REAL, DIMENSION(nt, nz), INTENT(IN) :: var
        CHARACTER(LEN=256), INTENT(IN) :: file_path, file_name
        INTEGER :: iz, it

        OPEN(10, FILE=TRIM(file_path)//TRIM(file_name), STATUS='unknown')
        !OPEN(10, FILE='/home/gihh1131/HOMEWORK/MICROPHYSICS/single_column_model/OUTPUT/FD_test_module.txt', STATUS='unknown')

        DO iz = 1, nz

         WRITE(10, *) (var(it,iz) , it = 1, nt)

        ENDDO

      END SUBROUTINE Sub_write_ascii 

  END MODULE Mod_write
