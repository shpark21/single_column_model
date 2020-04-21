MODULE Mod_const

  IMPLICIT NONE

    REAL, PARAMETER     :: Rd  = 287.        !!! Rd = 287 J/(kg*K); J = N*m =(kg*m/s^2)*m
    REAL, PARAMETER     :: Cp  = 1005.       !!! Cp = 1005.J/(kg*K)
    REAL, PARAMETER     :: g   = 9.8         !!! unit = m/s
    REAL, PARAMETER     :: pi  = 4*ATAN(1.)  !!! pi = 3.141591   
    REAL, PARAMETER     :: Ps  = 1013        !!! hPa 
    REAL, PARAMETER     :: rho = 1000        !!! kg/m^3 
    REAL, PARAMETER     :: nc  = 1.0e+8      !!! #/m^3 
    REAL, PARAMETER     :: qc  = 0.002       !!! [kg / kg]
    REAL, PARAMETER     :: r0  = 1.0e-5      !!! [kg / kg]

  CONTAINS

END MODULE Mod_const
