  MODULE Mod_global
  
    IMPLICIT NONE
     
    INTEGER :: it, iz   !! do parameter
    INTEGER :: nz

      !! for namelist val
    INTEGER :: dt, nt
    REAL    :: dz, z_top, z_sfc, z_ext
    REAL    :: ramma_dry
    CHARACTER(LEN=256) :: T_output_file_path, &
                        T_output_file_name, &
                        q_output_file_path, &
                        q_output_file_name

      !! Allocate val
    REAL, DIMENSION(:), ALLOCATABLE :: sfc_Temp !! unit = K
    REAL, DIMENSION(:), ALLOCATABLE :: ext_Temp !! unit = K
    REAL, DIMENSION(:,:), ALLOCATABLE :: Temp !! unit = K
    REAL, DIMENSION(:,:), ALLOCATABLE :: W    !! unit = m/s
    REAL, DIMENSION(:), ALLOCATABLE :: z    !! unit = m

    

      CONTAINS

      !!---------------------------------------------!!
      !!---Sub_name :                        --------!!
      !!---Input var :                       --------!!
      !!---Ouput var :                       --------!!
      !!---What is that :                    --------!!
      !!---------------------------------------------!!
      SUBROUTINE Sub_allocate

        IF (.NOT. ALLOCATED(Temp)) ALLOCATE(Temp(nt, nz))
        IF (.NOT. ALLOCATED(W   )) ALLOCATE(W   (nt, nz))
        IF (.NOT. ALLOCATED(z   )) ALLOCATE(z   (nz))
        IF (.NOT. ALLOCATED(sfc_Temp   )) ALLOCATE(sfc_Temp   (nt))
        IF (.NOT. ALLOCATED(ext_Temp   )) ALLOCATE(ext_Temp   (nt))

      END SUBROUTINE Sub_allocate
     
      !!---------------------------------------------!!
      !!---Sub_name :                        --------!!
      !!---Input var :                       --------!!
      !!---Ouput var :                       --------!!
      !!---What is that :                    --------!!
      !!---------------------------------------------!!
      SUBROUTINE Sub_deallocate

      END SUBROUTINE Sub_deallocate


  END MODULE Mod_global
