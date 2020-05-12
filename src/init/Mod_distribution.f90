MODULE Mod_distribution

  USE Mod_global
  USE Mod_const
  USE Mod_read, only: Sub_read_NC_file

  IMPLICIT NONE

    CONTAINS
    
    !!---------------------------------------------!!
    !!  Set. size distributions                    !!
    !!---------------------------------------------!!
    SUBROUTINE Sub_drop_distributions     &
               (                          &
                distribution_option,      &
                drop_column_num,          & !!  drop_column_num
                rbmin,                    & !!  drop_min_diameter
                rbmax,                    & !!  drop_max_diameter
                N                         & !!  drop_num
               )

    USE Mod_const, only : rho, Nc, qc, pi

    IMPLICIT NONE

    ! IN
    INTEGER, INTENT(IN)                      :: distribution_option, &
                                                drop_column_num              
    REAL,    INTENT(IN)                      :: rbmin,               &
                                                rbmax
    ! Local
    REAL, DIMENSION(drop_column_num)         :: r,  &
                                                D,  &
                                                m,  &
                                                dr, & 
                                                dD, &
                                                dm
    REAL, DIMENSION(drop_column_num+1)       :: rb, &
                                                mb
    INTEGER                                  :: ir, nbin
    REAL                                     :: ratio, &
                                                mu, sigma, lambda
    ! OUT
    ! REAL, DIMENSION(nz,column_num), INTENT(OUT) :: conc
    REAL, DIMENSION(drop_column_num),    INTENT(OUT) :: N
    
    nbin  = drop_column_num

    rb(1) = rbmin ; rb(nbin + 1) = rbmax       ! boundary of drop diameter
    ratio = (rb(nbin + 1) / rb(1)) ** (1. / nbin)
    DO ir = 2, nbin
      rb(ir) = ratio * rb(ir - 1)  
    ENDDO
   
    DO ir = 1, (nbin + 1)
      mb(ir) = rho * (4. / 3.)* pi * (rb(ir)) ** 3.
    ENDDO
    
    DO ir = 1, nbin
      m(ir) = (mb(ir) + mb(ir + 1)) / 2.
      !r(ir) = (rb(ir)+rb(ir+1))/2.
      r(ir) = ((3. * m(ir)) / (4. * rho * pi)) ** (1. / 3.)
      dr(ir) = rb(ir + 1) - rb(ir)
      dm(ir) = mb(ir + 1) - mb(ir)
      dlnr(ir) = (LOG(rb(ir + 1)) - LOG(rb(ir)))
    ENDDO

    SELECT CASE(distribution_option)

      CASE(1)

        ! Log-normal distribution
        mu = LOG(r0)
        sigma = SQRT((2. / 9.) * LOG(qc / (Nc * rho * (4. / 3.) * pi *(r0) ** 3.)))
        DO ir = 1, nbin
          N(ir) = (Nc / ((SQRT(2. * pi)) * sigma * r(ir))) * EXP((-1. * (LOG(r(ir)) - mu) ** 2.) / &
                  (2. * (sigma ** 2))) * dr(ir)
        ENDDO

        write(*,*) "log-normal dist. in model (Nc) =  ", SUM(N)
        write(*,*) "log-normal dist. const.   (Nc) =  ", Nc
        write(*,*) "accuracy  = ", (SUM(N) / Nc) * 100., "%"

        write(*,*) "log-normal dist. in model (qc) =  ", SUM(m * N)
        write(*,*) "log-normal dist. const.   (qc) =  ", qc
        write(*,*) "accuracy  = ", (SUM(m * N) / qc) * 100., "%"

      CASE(2)

        ! Gamma distribution
        D = 2 * r
        dD = 2 * dr
        mu = (1.0e+9 / Nc) + 2.
        lambda = ((Nc / qc) * (GAMMA(mu + 4.) / GAMMA(mu + 1.)) * pi * (1. / 6.) * rho) ** (1./3.)
        !N0 = (Nc*lamb**(mu+1.))/GAMMA(mu+1.)

        DO ir = 1, nbin
          !N(ir) = N0*r(ir)^mu*EXP(-1*lamb*r(ir))
          N(ir) = (Nc / GAMMA(mu + 1.)) * lambda *((lambda * D(ir)) ** mu) * EXP(-1. * lambda * D(ir)) * dD(ir)
        ENDDO

        write(*,*) "gamma dist. in model (Nc) =  ", SUM(N)
        write(*,*) "gamma dist. const.   (Nc) =  ", Nc
        write(*,*) "accuracy  = ", (SUM(N) / Nc) * 100., "%"

        write(*,*) "gamma dist. in model (qc) =  ", SUM(m * N)
        write(*,*) "gamma dist. const.   (qc) =  ", qc
        write(*,*) "accuracy  = ", (SUM(m * N) / qc) * 100., "%"

    END SELECT

    END SUBROUTINE Sub_drop_distributions 

END MODULE Mod_distribution
