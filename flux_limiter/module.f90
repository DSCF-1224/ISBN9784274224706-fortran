module pkg_flux_limiter

    ! required MODULE(s)
    use , intrinsic :: iso_fortran_env

    ! require all variables to be explicitly declared
    implicit none



    ! accessibility setting
    private
    public  :: minmod



    contains



    pure elemental function minmod ( x, y )

        ! argument(s) for this FUNCTION
        real(REAL64) , intent(in) :: x, y

        ! return value of this FUNCTION
        real(REAL64) :: minmod

        ! variable(s) for this FUNCTION
        real(REAL64) :: sign_x

        sign_x = sign( a= 1.0_REAL64 , b= x )
        minmod = sign_x * max( min( abs(x) , sign_x * y ) , 0.0_REAL64 )
        return

    end function minmod

end module pkg_flux_limiter
