begin

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; init.

n0 = 1000.                                ; [ # / m^3 ]

nbin = 37                                 ; it could be optional

rb = new((nbin + 1), "float")

r = new(nbin, "float")

rb(0) = 1.0e-6                            ; [ m ; 1 um ]

;rb(nbin) = 0.01                           ; [ m ; 1 cm ]

dr = new(nbin, "float")

;rratio = (rb(nbin) / rb(0))^(1. / nbin)
rratio = 1.3

;do irb = 1, (nbin - 1)
do irb = 1, nbin

   rb(irb) = rb(0) * rratio^irb

end do

do ir = 0, (nbin - 1)

   r(ir) = (rb(ir) + rb(ir + 1)) / 2.

   dr(ir) = rb(ir + 1) - rb(ir)

end do

mu = avg(log(r))

sig = stddev(log(r))

pi = 4. * atan(1.)

nr_ln = new(nbin, "float")

nr_g = new(nbin, "float")

m = new(nbin, "float")

rho = 1000.                                  ; [ kg / m^3 ]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; log-normal dist.

do ir = 0, (nbin - 1)

   nr_ln(ir) = (n0 / ((sqrt(2. * pi)) * sig * r(ir))) * exp((-1. * (log(r(ir)) - mu)^2) / (2. * sig^2))
   
   m(ir) = nr_ln(ir) * (4. * pi * r(ir)^3 * rho) / 3.

end do

delete(n0)

delete(mu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; gamma dist.

nc = 1000.                 ; [ # / m^3 ]

;qc = 0.1                  ; [ kg / kg ]
qc = 0.001                 ; [ kg / kg ]

mu = 2.

lamb = (rho * (4. / 3.) * pi * (nc / qc) * (gamma(mu + 4.) / gamma(mu + 1.)))^(1. / 3.)

n0 = (nc * lamb^(mu + 1)) / gamma(mu + 1.)

do ir = 0, (nbin - 1)

   nr_g(ir) = n0 * r(ir)^mu * exp(-1 * lamb * r(ir))

end do

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; plot

wks = gsn_open_wks("x11", "gd")

res = True

plot = gsn_csm_xy(wks, r, nr_g*dr, res)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

end
