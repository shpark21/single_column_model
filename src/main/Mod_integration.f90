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

      INTEGER :: nnt
      output_temp(0,:)=next_temp(:)
      output_q(0,:)=next_q(:)

      DO it = 1, nt

        write(*,*) it
        CALL Sub_Finite_volume ( temp, sfc_temp(it),  &    
                                 dz, nz,              &    
                                 dt,                  &    
                                 w,                   &    
                                 next_temp           ) 


        CALL Sub_Finite_volume ( q, sfc_q(it),        &   
                                 dz, nz,              & 
                                 dt,                  & 
                                 w,                   & 
                                 next_q              )  

        temp(:)=next_temp(:)
        q(:)=next_q(:)
 
        output_temp(it,:)=next_temp(:)
        output_q(it,:)=next_q(:)

        ! CALL Sub_Cal_P
        ! CALL Sub_Cal_W
        !!CALL cloud_pysics
      ENDDO !! time do


       nnt = nt + 1
       OPEN(51,file="temp.bin",form="unformatted", &
               status="unknown",access="direct",recl=4*nnt*nz)

       !write(51,rec=1) (temp_wr(it,:),it=1,nt)

    END SUBROUTINE Sub_Integration_FV

    !!---------------------------------------------!!
    !!---------------------------------------------!!
    !!---------------------------------------------!!
    SUBROUTINE Sub_Integration_FD

      IMPLICIT NONE

      INTEGER :: nnt
      output_temp(0,:)=next_temp(:)
      output_q(0,:)=next_q(:)

      DO it = 1, nt

        CALL Sub_Finite_diff ( temp, sfc_temp(it),  &
                                 top_temp(it),      &
                                 dz, nz,            &
                                 dt,                &
                                 w,                 &
                                 next_temp         )


        CALL Sub_Finite_diff ( q, sfc_q(it),        &
                                 top_q(it),         &
                                 dz, nz,            &
                                 dt,                &
                                 w,                 &
                                 next_q            )

        temp(:)=next_temp(:)
        q(:)=next_q(:)

        output_temp(it,:)=next_temp(:)
        output_q(it,:)=next_q(:)

        ! CALL Sub_Cal_P
        ! CALL Sub_Cal_W
        !!CALL cloud_pysics
      ENDDO !! time do

       nnt = nt + 1
       OPEN(51,file="temp.bin",form="unformatted", &
               status="unknown",access="direct",recl=4*nnt*nz)
  
       !write(51,rec=1) (temp_wr(it,:),it=1,nt)
  
    END SUBROUTINE Sub_Integration_FD
ENDMODULE
