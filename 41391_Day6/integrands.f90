MODULE integrands
USE precision, ONLY: wp

IMPLICIT NONE

PUBLIC :: ex1_frac
PRIVATE

CONTAINS

    SUBROUTINE ex1_frac(s, N, inte)
        !! Computes one term of the midpoint-rule summation used to approximate pi.
        !!
        !! Integral (exact):
        !!   pi = ∫_0^1 4/(1 + x^2) dx
        !!
        !! Midpoint-rule / Riemann-sum approximation with N sub-intervals:
        !!   pi ≈ (1/N) * Σ_{s=1..N} 4/(1 + x_s^2)
        !! where
        !!   x_s = (s - 0.5)/N
        !!
        !! This routine returns the single summand (including the 1/N factor)
        !! so the caller can loop s=1..N and accumulate.


        INTEGER, INTENT(IN) :: s, N
        REAL(wp), INTENT(OUT) :: inte

        REAL(wp) :: x

        IF (N < 1) ERROR STOP 'integrands/ex1_frac: N must be >= 1'
        IF (s < 1 .OR. s > N) ERROR STOP 'integrands/ex1_frac: s must satisfy 1 <= s <= N'

        x = (REAL(s, KIND=wp) - 0.5_wp) / REAL(N, KIND=wp)
        inte = (1.0_wp / REAL(N, KIND=wp)) * (4.0_wp / (1.0_wp + x*x))
    END SUBROUTINE ex1_frac



END MODULE integrands
