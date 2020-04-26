MODULE Mod_intepolate

  USE Mod_global

  IMPLICIT NONE

    CONTAINS


    !!---------------------------------------------!!
    !!  Cal. vertical coordinate                   !!
    !!---------------------------------------------!!
    SUBROUTINE Sub_set_grid

    USE Mod_const, only: Ps, R, g
    IMPLICIT NONE
      REAL, DIMENSION(in_nz), INTENT(in) :: z_in,    &
                                            z_conv,  &   
                                            temp_in, &
                                            qv_in

      ! read_psfc = 1 : using psfc form input.
      ! if not, using psfc form constant value in model.(Ps = 1013.)
      if (read_psfc = 1) Ps = Psfc

      if ( read_pres == 1 ) then
          Tv = temp_in*(1+(0.61*qv_in))
          H = (R*Tv)/g
          z_in = -H*(log(z_in/Ps))
      if ( read_pres == 2 ) then
          z_in = vert_in/g
      endif

    if ( temp_var == 'theta' ) then
        if ( vert_var == 'p' ) then
            T_in = temp_in*((vert_in/Ps)**(R/Cp))
        else
            print*, " :: Without air pressure information,"
            print*, " ::  'Temp' can't be calculated from 'theta'."
            print*, " :: Please Check the 'vertical variable'."
        endif
    else
        T_in = temp_in
    endif

!!!!! :: Interpolate to fit nz
    do i = 1, nz
        do j = 1, nlev
            if ( z_in(j) <= z_full(i) .and. z_full(i) <= z_in(j+1) ) then
                d1 = z_full(i) - z_in(j)
                d2 = z_in(j+1) - z_full(i)
                z_out(i)  =  z_in(j)*(d2/(d1+d2)) +  z_in(j+1)*(d1/(d1+d2))
                w_out(i)  =  w_in(j)*(d2/(d1+d2)) +  w_in(j+1)*(d1/(d1+d2))
                T_out(i)  =  T_in(j)*(d2/(d1+d2)) +  T_in(j+1)*(d1/(d1+d2))
                qv_out(i) = qv_in(j)*(d2/(d1+d2)) + qv_in(j+1)*(d1/(d1+d2))
            endif
        enddo
        ! print*, 'i : ', i, 'z_out : ', z_out(i), 'T_out : ', T_out(i),  'Q_out : ', qv_out(i)
    enddo

    END SUBROUTINE Sub_set_grid
   
END MODULE Mod_interpolate




    




    
