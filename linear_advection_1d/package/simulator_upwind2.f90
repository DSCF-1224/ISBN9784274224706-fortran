module simulator_upwind2

    ! required MODULE
    use,     intrinsic :: iso_fortran_env
    use, non_intrinsic :: config
    use, non_intrinsic :: simulator_upwind_base



    ! require all variables to be explicitly declared
    implicit none



    ! accessibility setting
    private
    public  :: type_simulator_upwind2



    ! TYPE declaration
    type , extends(type_simulator_upwind_base) :: type_simulator_upwind2

        ! additional field(s) for this TYPE
        ! NONE

        contains

        ! kind: FUNCTION
        procedure , pass , public :: numerical_flux    => numerical_flux_upwind2
        procedure , pass , public :: maxval_index_node => maxval_index_node_upwind2
        procedure , pass , public :: minval_index_node => minval_index_node_upwind2

    end type type_simulator_upwind2



    contains



    pure elemental function numerical_flux_upwind2 ( simulator, index_node ) result(flux)

        ! bounded argument for this FUNCTION
        class(type_simulator_upwind2) , intent(in) :: simulator

        ! unbounded argument(s) for this FUNCTION
        integer(INT32) , intent(in) :: index_node

        ! return value of this FUNCTION
        real(REAL64) :: flux

        associate ( q => simulator%quantity%copy )

            flux = simulator%numerical_flux_upwind(&!
                q_left  = 1.5_REAL64 * q( index_node           ) - 0.5_REAL64 * q( index_node - 1_INT32 ) , &!
                q_right = 1.5_REAL64 * q( index_node + 1_INT32 ) - 0.5_REAL64 * q( index_node + 2_INT32 )   &!
            )

        end associate

    end function numerical_flux_upwind2



    pure elemental function maxval_index_node_upwind2 (simulator) result(index)

        ! bounded argument for this FUNCTION
        class(type_simulator_upwind2) , intent(in) :: simulator

        ! return value of this FUNCTION
        integer(INT32) :: index

        index = size(simulator%quantity) - 2_INT32

    end function maxval_index_node_upwind2



    pure elemental function minval_index_node_upwind2 (simulator) result(index)

        ! bounded argument for this FUNCTION
        class(type_simulator_upwind2) , intent(in) :: simulator

        ! return value of this FUNCTION
        integer(INT32) :: index

        index = simulator%minval_index_node_default() + 1_INT32

    end function minval_index_node_upwind2

end module simulator_upwind2
