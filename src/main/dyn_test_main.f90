PROGRAM main_program

  USE Mod_global
  ! USE Mod_read  
  ! USE Mod_initialization
  USE Mod_integration 
  ! USE Mod_write 

  IMPLICIT NONE
    INTEGER                           ::  it, iz
    INTEGER                           ::  ionum

    !------1. setting global variable--------!!

    !------2. read namelist          --------!!

    ! CALL Sub_read_namelist
    ! CALL Sub_allocate 
    ! Open namelist file.

    ! Read namelist variable.
    NAMELIST/Time_control/nt,dt,output_interval
    NAMELIST/Domain/ztop,nz
    NAMELIST/Options/dyn_option

    ! Open namelist
    OPEN(UNIT=10, FILE='namelist.info', STATUS='old', IOSTAT=ionum)

    IF(ionum .ne. 0) THEN
      CALL Fail_msg("Unable to open the namelist file")
    ENDIF

    ! Read namelist variable.
    READ(10, nml=Time_control)
    READ(10, nml=Domain)


    it = 0
    !------3. initialization process --------!!
    allocate(      temp(nz)     )
    allocate(         q(nz)     )
    allocate( next_temp(nz)     )
    allocate(    next_q(nz)     ) 
    allocate(         p(nz)     )
    allocate(         w(nz+1)   )

    !---to determinate dz            --------!!
    dz=ztop/nz

    !---to read T, q, w              --------!!
    DO iz = 1, nz
      IF ( iz .eq. 1 ) then
        temp(iz) = sfc_temp + dz*dry_gam
      ELSE
        temp(iz) = temp(iz-1) + dz*dry_gam
      ENDIF
    ENDDO

    q(:)    = 0.
    q(40)   = 10.
    q(50)   = 10.
    w(:)    = 20.
    w(nz+1) = 10.

    !---to calculate  p           --------!!
    ! CALL Sub_read_T
    ! CALL Sub_read_qv
    ! CALL Sub_Cal_P

    !------4. integration process    --------!!
    !---including dynamics process and microphysics process   --------!!
    CALL Sub_Integration                     &
         (                                   &
           temp, next_temp, sfc_temp,        &
           q, next_q, sfc_q,                 &
           w,                                &
           dz, nz, dt, nt, output_interval   &    
         )    

    !------5. write output file      --------!!
    ! CALL Sub_write
    !
    ! CALL Sub_deallocate

END PROGRAM main_program
