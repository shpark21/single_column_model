MODULE Mod_global

  IMPLICIT NONE
   
  INTEGER :: it, iz   !! do parameter

    !! for namelist val
  INTEGER            :: nt,              &
                        nz,              &
                        dt,              &
                        ionum,           &
                        output_interval
  INTEGER            :: dyn_option
  INTEGER            :: dz_option
  REAL               :: z_top,      &
                        z_sfc,      &
                        z_ext       
  REAL               :: gamma_dry
  REAL               :: dzr,        &
                        dz_1st 
  CHARACTER(LEN=256) :: T_output_file_path, &
                        T_output_file_name, &
                        q_output_file_path, &
                        q_output_file_name

    !! Allocate val
  REAL, DIMENSION(:),   ALLOCATABLE   :: sfc_Temp     !! unit = K
  REAL, DIMENSION(:),   ALLOCATABLE   :: top_Temp     !! unit = K
  REAL, DIMENSION(:),   ALLOCATABLE   :: Temp         !! unit = K
  REAL, DIMENSION(:),   ALLOCATABLE   :: next_Temp    !! unit = K
  REAL, DIMENSION(:,:), ALLOCATABLE   :: output_Temp  !! unit = K

  REAL, DIMENSION(:),   ALLOCATABLE   :: sfc_q        !! unit = num.
  REAL, DIMENSION(:),   ALLOCATABLE   :: top_q        !! unit = num.
  REAL, DIMENSION(:),   ALLOCATABLE   :: q            !! unit = number
  REAL, DIMENSION(:),   ALLOCATABLE   :: next_q       !! unit = number
  REAL, DIMENSION(:,:), ALLOCATABLE   :: output_q     !! unit = K

  REAL, DIMENSION(:), ALLOCATABLE   :: w          !! unit = m/s
  REAL, DIMENSION(:), ALLOCATABLE   :: z          !! unit = m
  REAL, DIMENSION(:), ALLOCATABLE   :: dz          !! unit = m

    CONTAINS

    !!-----------------------------!!
    SUBROUTINE Sub_allocate

      IF (.NOT. ALLOCATED(Temp       )) ALLOCATE(Temp            (nz))
      IF (.NOT. ALLOCATED(sfc_Temp   )) ALLOCATE(sfc_Temp        (nt))
      IF (.NOT. ALLOCATED(top_Temp   )) ALLOCATE(top_Temp        (nt))
      IF (.NOT. ALLOCATED(next_Temp  )) ALLOCATE(next_Temp       (nz))
      IF (.NOT. ALLOCATED(output_Temp)) ALLOCATE(output_Temp(0:nt,nz))

      IF (.NOT. ALLOCATED(q          )) ALLOCATE(q            (nz))
      IF (.NOT. ALLOCATED(sfc_q      )) ALLOCATE(sfc_q        (nt))
      IF (.NOT. ALLOCATED(top_q      )) ALLOCATE(top_q        (nt))
      IF (.NOT. ALLOCATED(next_q     )) ALLOCATE(next_q       (nz))
      IF (.NOT. ALLOCATED(output_q   )) ALLOCATE(output_q(0:nt,nz))

      IF (.NOT. ALLOCATED(w          )) THEN  
        IF ( dyn_option .eq. 1) THEN
          ALLOCATE(w       (nz))
        ELSE IF ( dyn_option .eq. 2 ) THEN
          ALLOCATE(w       (0:nz))
        ELSE
          CALL Fail_msg(" dyn_option must be integer // Choose either 1 or 2 ")
        ENDIF
      ENDIF
      IF (.NOT. ALLOCATED(z          )) ALLOCATE(z        (nz))
      IF (.NOT. ALLOCATED(dz         )) ALLOCATE(dz       (nz))

    END SUBROUTINE Sub_allocate
   
    !!-----------------------------!!
    SUBROUTINE Sub_deallocate
    END SUBROUTINE Sub_deallocate

    !!-----------------------------!!
    SUBROUTINE FAIL_MSG(f_msg)

      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: f_msg

      write (*,'("FAIL: ", a)') f_msg
      stop "##### ERROR: PROGRAM ABORTED. #####"

    END SUBROUTINE FAIL_MSG



END MODULE Mod_global
