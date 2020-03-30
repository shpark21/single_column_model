MODULE Mod_global

  IMPLICIT NONE
    ! Time 
    INTEGER                           ::  dt,               &   ! delta t
                                          nt,               &   ! nuber of time step   
                                          output_interval
    ! Coordinate
    INTEGER                           ::  ztop,             &   ! 
                                          nz                    ! 
    !REAL,DIMENSION(:),ALLOCATABLE  ::  dz                    ! 
    INTEGER                           ::  dz                    ! 
    INTEGER                           ::  dyn_option 
                                          
    ! Scalar variable 
    REAL,DIMENSION(:),ALLOCATABLE     ::  temp,             & 
                                          q,                & 
                                          p 

    REAL,DIMENSION(:),ALLOCATABLE     ::  next_temp,             & 
                                          next_q
    ! Vector variable 
    REAL,DIMENSION(:),ALLOCATABLE     ::  w 
    ! Parameter
    REAL,PARAMETER                    ::  dry_gam  = -0.0098, &       ! unit = K/m
                                          sfc_temp = 290.,    &
                                          sfc_q    = 0.



    CONTAINS

    SUBROUTINE FAIL_MSG(f_msg)

      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: f_msg

      write (*,'("FAIL: ", a)') f_msg
      stop "##### ERROR: PROGRAM ABORTED. #####"

    END SUBROUTINE FAIL_MSG

END MODULE Mod_global
