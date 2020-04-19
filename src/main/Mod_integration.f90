MODULE Mod_integration

  USE Mod_global
  USE Mod_dyn_driver

  IMPLICIT NONE

    CONTAINS

    !!---------------------------------------------!!
    !!---------------------------------------------!!
    !!---------------------------------------------!!
    SUBROUTINE Sub_Integration_FV

      IMPLICIT NONE

      ! temp%dout     = 0. 
      ! q%dout        = 0.

      temp%dout(:,1)=temp%dz(:)
      q%dout(:,1)=q%dz(:)

      DO it = 1, nt

        CALL Sub_Finite_volume ( temp%dz, temp%sfc_dt(it),  &
                                 temp%top_dt(it),           &
                                 dz%dz, nz,                 &
                                 dt,                        &
                                 w%stag_dz,                 &
                                 temp%next_dz               &
                                                           )

        CALL Sub_Finite_volume ( q%dz, q%sfc_dt(it),        &
                                 q%top_dt(it),              &
                                 dz%dz, nz,                 &
                                 dt,                        &
                                 w%stag_dz,                 &
                                 q%next_dz                  & 
                                                           )

        IF (ALLOCATED(temp%dz)) DEALLOCATE(temp%dz)
        IF (ALLOCATED(q%dz   )) DEALLOCATE(q%dz   )
        IF (.NOT. ALLOCATED(temp%dz)) ALLOCATE(temp%dz(nz))
        IF (.NOT. ALLOCATED(q%dz   )) ALLOCATE(q%dz   (nz))

        temp%dz(:)=temp%next_dz(:)
        q%dz(:)=q%next_dz(:)
 
        temp%dout(:,it+1)=temp%next_dz(:)
        q%dout(:,it+1)=q%next_dz(:)

        ! CALL Sub_Cal_P
        ! CALL Sub_Cal_W
        !!CALL cloud_pysics
      ENDDO !! time do

    END SUBROUTINE Sub_Integration_FV

    !!---------------------------------------------!!
    !!---------------------------------------------!!
    !!---------------------------------------------!!
    SUBROUTINE Sub_Integration_FD

      IMPLICIT NONE

      temp%dout(:,1)=temp%dz(:)
      q%dout(:,1)=q%dz(:)

      DO it = 1, nt

        CALL Sub_Finite_diff ( temp%dz, temp%sfc_dt(it),    &
                                 temp%top_dt(it),           &
                                 dz%dz, nz,                 &
                                 dt,                        &
                                 w%stag_dz,                 &
                                 temp%next_dz               &
                                                           )
  
        CALL Sub_Finite_diff ( q%dz, q%sfc_dt(it),          &
                                 q%top_dt(it),              &
                                 dz%dz, nz,                 &
                                 dt,                        &
                                 w%stag_dz,                 &
                                 q%next_dz                  & 
                                                           )
        IF (ALLOCATED(temp%dz)) DEALLOCATE(temp%dz)
        IF (ALLOCATED(q%dz   )) DEALLOCATE(q%dz   )
        IF (.NOT. ALLOCATED(temp%dz)) ALLOCATE(temp%dz(nz))
        IF (.NOT. ALLOCATED(q%dz   )) ALLOCATE(q%dz   (nz))

        temp%dz(:)=temp%next_dz(:)
        q%dz(:)=q%next_dz(:)

        temp%dout(:,it+1)=temp%next_dz(:)
        q%dout(:,it+1)=q%next_dz(:)

        ! CALL Sub_Cal_P
        ! CALL Sub_Cal_W
        !!CALL cloud_pysics
      ENDDO !! time do

    END SUBROUTINE Sub_Integration_FD

    !!---------------------------------------------!!
    !!---------------------------------------------!!
    !!---------------------------------------------!!
    SUBROUTINE Sub_Integration_PPM

      IMPLICIT NONE

      ! temp%dout     = 0. 
      ! q%dout        = 0.

      temp%dout(:,1)=temp%dz(:)
      q%dout(:,1)=q%dz(:)

      DO it = 1, nt

        CALL Sub_Finite_volume_PPM ( temp%dz, temp%sfc_dt(it),  &
                                     temp%top_dt(it),           &
                                     dz%dz, nz,                 &
                                     dt,                        &
                                     w%stag_dz,                 &
                                     temp%next_dz               &
                                                               )
        CALL Sub_Finite_volume_PPM ( q%dz, q%sfc_dt(it),        &
                                     q%top_dt(it),              &
                                     dz%dz, nz,                 &
                                     dt,                        &
                                     w%stag_dz,                 &
                                     q%next_dz                  &
                                                               )
        IF (ALLOCATED(temp%dz)) DEALLOCATE(temp%dz)
        IF (ALLOCATED(q%dz   )) DEALLOCATE(q%dz   )
        IF (.NOT. ALLOCATED(temp%dz)) ALLOCATE(temp%dz(nz))
        IF (.NOT. ALLOCATED(q%dz   )) ALLOCATE(q%dz   (nz))
        temp%dz(:)=temp%next_dz(:)
        q%dz(:)=q%next_dz(:)

        temp%dout(:,it+1)=temp%next_dz(:)
        q%dout(:,it+1)=q%next_dz(:)

        ! CALL Sub_Cal_P
        ! CALL Sub_Cal_W
        !!CALL cloud_pysics
      ENDDO !! time do

    END SUBROUTINE Sub_Integration_PPM

ENDMODULE
