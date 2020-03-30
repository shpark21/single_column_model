SUBROUTINE Sub_Integration 
            (                                   &
              temp, next_temp, sfc_temp,        &
              q, next_q, sfc_temp,              &
              w,                                &    
              dz, nz, dt, nt, output_interval   &
            )
USE Mod_global

   IMPLICIT NONE
   ! IN
   INTEGER,                    INTENT(IN)   :: dz, dt
   INTEGER,                    INTENT(IN)   :: nz, nt
   INTEGER,                    INTENT(IN)   :: output_interval
   REAL,    DIMENSION(nvar),   INTENT(IN)   :: temp, q
   REAL,    DIMENSION(nvar+1), INTENT(IN)   :: w
   REAL,                       INTENT(IN)   :: sfc_temp
   REAL,                       INTENT(IN)   :: sfc_q
   ! Local                     
   INTEGER :: it
   ! OUT    
   REAL,    DIMENSION(nvar),   INTENT(OUT)  :: next_temp
   REAL,    DIMENSION(nvar),   INTENT(OUT)  :: next_q

   ! DO it = 1, nt
   !
   !   CALL Sub_Finite_volume( temp,            &
   !                           sfc_temp,        &    
   !                           dz,              &    
   !                           dt,              &    
   !                           w,               &    
   !                           next_temp       ) 
   !   temp(:)=next_temp(:)
   !
   !   CALL Sub_Finite_volume( q,               &
   !                           sfc_q,           &   
   !                           dz,              & 
   !                           dt,              & 
   !                           w,               & 
   !                           next_q       )  
   !   q(:)=next_q(:)
   !
   !   ! CALL Sub_Cal_P
   !   ! CALL Sub_Cal_W
   !
   !   ! CALL Sub_write_otuput
   !   write(*,*) "temp", it," = ", temp(it), "next_temp ", it,"=
   !   ", next_temp(it)
   !   write(*,*) "q", it," = ", q(it), "next_q ", it,"= ",
   !   next_q(it)
   ! ENDDO !! time do
   ! write(*,*) sum(temp), sum(next_temp) 
   ! write(*,*) sum(q), sum(next_q) 
   !
   ! !CALL cloud_pysics

 END SUBROUTINE Sub_Integration )
