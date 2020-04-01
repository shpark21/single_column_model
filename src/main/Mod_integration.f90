MODULE Mod_integration

  USE Mod_global

  IMPLICIT NONE

    CONTAINS

    !!---------------------------------------------!!
    !!---------------------------------------------!!
    !!---------------------------------------------!!
    SUBROUTINE Sub_Integration_FV

      IMPLICIT NONE

      output_temp(0,:)=next_temp(:)
      output_q(0,:)=next_q(:)

      DO it = 1, nt

        CALL Sub_Finite_volume ( temp, sfc_temp,      &    
                                 dz, nz,              &    
                                 dt,                  &    
                                 w,                   &    
                                 next_temp           ) 


        CALL Sub_Finite_volume ( q, sfc_q,            &   
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
      write(*,*) sum(next_temp), sum(next_q)
      ENDDO !! time do


       integer :: nnt
       nnt = nt + 1
       OPEN(51,file="temp.bin",form="unformatted", &
               status="unknown",access="direct",recl=4*nnt*nz)

       !write(51,rec=1) (temp_wr(it,:),it=1,nt)
       write(51,rec=1) (q_wr(it,:),it=0,nt)

    END SUBROUTINE Sub_Integration_FV

    !!---------------------------------------------!!
    !!---------------------------------------------!!
    !!---------------------------------------------!!
    SUBROUTINE Sub_Integration_FD

      IMPLICIT NONE

      output_temp(0,:)=next_temp(:)
      output_q(0,:)=next_q(:)

      DO it = 1, nt

        CALL Sub_Finite_diff ( temp, sfc_temp,      &
                                 top_temp           &
                                 dz, nz,            &
                                 dt,                &
                                 w,                 &
                                 next_temp         )


        CALL Sub_Finite_diff ( q, sfc_q,            &
                                 top_q              &
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
      write(*,*) sum(next_temp), sum(next_q)
      ENDDO !! time do

       integer :: nnt
       nnt = nt + 1
       OPEN(51,file="temp.bin",form="unformatted", &
               status="unknown",access="direct",recl=4*nnt*nz)
  
       !write(51,rec=1) (temp_wr(it,:),it=1,nt)
       write(51,rec=1) (q_wr(it,:),it=0,nt)
  
    END SUBROUTINE Sub_Integration_FD
ENDMODULE
