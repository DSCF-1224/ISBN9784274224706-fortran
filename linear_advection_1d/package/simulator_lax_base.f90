module simulator_lax_base

    ! required MODULE
    use,     intrinsic :: iso_fortran_env
    use, non_intrinsic :: config
    use, non_intrinsic :: simulator_using_numerical_flux_base



    ! require all variables to be explicitly declared
    implicit none



    ! accessibility setting
    private
    public  :: type_simulator_lax_base
    public  :: size



    ! TYPE declaration
    type , abstract , extends(type_simulator_using_numerical_flux_base) :: type_simulator_lax_base

        ! additional field(s) for this TYPE
        ! NONE

        contains

        ! kind: FUNCTION
        procedure , pass , public :: numerical_flux_lax_base

    end type type_simulator_lax_base



    contains



    pure elemental function numerical_flux_lax_base ( simulator, q_minus, q_plus, nu ) result(flux)

        ! bounded argument for this FUNCTION
        class(type_simulator_lax_base) , intent(in) :: simulator

        ! unbounded argument(s) for this FUNCTION
        real(REAL64) , intent(in) :: q_minus
        real(REAL64) , intent(in) :: q_plus
        real(REAL64) , intent(in) :: nu

        ! return value of this FUNCTION
        real(REAL64) :: flux

        flux &!
            = 0.5_REAL64 &!
            * simulator%physical_flux(&!
                ( 1.0_REAL64 + nu ) * q_plus  + &!
                ( 1.0_REAL64 - nu ) * q_minus   &!
            )

    end function numerical_flux_lax_base

end module simulator_lax_base
