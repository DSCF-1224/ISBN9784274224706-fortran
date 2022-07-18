module simulator_using_numerical_flux_base

    ! required MODULE
    use,     intrinsic :: iso_fortran_env
    use, non_intrinsic :: config
    use, non_intrinsic :: simulator_base



    ! require all variables to be explicitly declared
    implicit none



    ! accessibility setting
    private
    public  :: type_simulator_using_numerical_flux_base
    public  :: size



    ! TYPE declaration
    type , abstract , extends(type_simulator_base) :: type_simulator_using_numerical_flux_base

        ! additional field(s) for this TYPE
        ! NONE

        contains

        ! kind: FUNCTION
        procedure( numerical_flux_abstract ) , pass , public , deferred :: numerical_flux

        ! kind: FUNCTION
        procedure , pass , public :: quantity_change => quantity_change_using_numerical_flux

    end type type_simulator_using_numerical_flux_base



    ! INTERFACE declaration
    interface

        pure elemental function numerical_flux_abstract ( simulator, index_node ) result(flux)

            ! required TYPE(s) specification
            import INT32
            import REAL64
            import type_simulator_using_numerical_flux_base

            ! bounded argument for this FUNCTION
            class(type_simulator_using_numerical_flux_base) , intent(in) :: simulator

            ! unbounded argument(s) for this FUNCTION
            integer(INT32) , intent(in) :: index_node

            ! return value of this FUNCTION
            real(REAL64) :: flux

        end function numerical_flux_abstract

    end interface



    contains



    pure elemental function quantity_change_using_numerical_flux ( simulator, index_node ) result(quantity_change)

        ! bounded argument for this FUNCTION
        class(type_simulator_using_numerical_flux_base) , intent(in) :: simulator

        ! unbounded argument(s) for this FUNCTION
        integer(INT32) , intent(in) :: index_node

        ! return value of this FUNCTION
        real(REAL64) :: quantity_change

        ! variable(s) for this FUNCTION

        associate ( &!
            dt => simulator%step_time , &!
            dx => simulator%step_node   &!
        )

            quantity_change &!
                = simulator%numerical_flux( index_node - 1_INT32 ) &!
                - simulator%numerical_flux( index_node )

            quantity_change = &!
            quantity_change * dt / dx

        end associate

    end function quantity_change_using_numerical_flux

end module simulator_using_numerical_flux_base
