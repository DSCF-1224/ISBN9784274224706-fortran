module simulator_upwind_base

    ! required MODULE
    use,     intrinsic :: iso_fortran_env
    use, non_intrinsic :: config
    use, non_intrinsic :: simulator_using_numerical_flux_base



    ! require all variables to be explicitly declared
    implicit none


    ! accessibility setting
    private
    public  :: type_simulator_upwind_base
    public  :: size



    ! TYPE declaration
    type , abstract , extends(type_simulator_using_numerical_flux_base) :: type_simulator_upwind_base

        ! additional field(s) for this TYPE
        ! NONE

        contains

        ! kind: FUNCTION
        procedure , pass , public :: numerical_flux_upwind

    end type type_simulator_upwind_base



    contains



    pure elemental function numerical_flux_upwind ( simulator, q_left, q_right ) result(flux)

        ! bounded argument for this FUNCTION
        class(type_simulator_upwind_base) , intent(in) :: simulator

        ! unbounded argument(s) for this FUNCTION
        real(REAL64), intent(in) :: q_left
        real(REAL64), intent(in) :: q_right

        ! return value of this FUNCTION
        real(REAL64) :: flux

        flux = 0.5_REAL64 * (&!
            simulator % physical_flux     ( q_left + q_right ) + &!
            simulator % physical_flux_abs ( q_left - q_right )   &!
        )

    end function numerical_flux_upwind

end module simulator_upwind_base
