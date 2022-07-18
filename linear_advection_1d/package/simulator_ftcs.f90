module simulator_ftcs

    ! required MODULE
    use,     intrinsic :: iso_fortran_env
    use, non_intrinsic :: config
    use, non_intrinsic :: simulator_base



    ! require all variables to be explicitly declared
    implicit none



    ! accessibility setting
    private
    public  :: type_simulator_ftcs



    ! TYPE declaration
    type , extends(type_simulator_base) :: type_simulator_ftcs

        ! additional field(s) for this TYPE
        ! NONE

        contains

        ! kind: FUNCTION
        procedure , pass , public :: numerical_flux    => numerical_flux_ftcs
        procedure , pass , public :: maxval_index_node => maxval_index_node_ftcs
        procedure , pass , public :: minval_index_node => minval_index_node_ftcs

    end type type_simulator_ftcs



    contains



    pure elemental function numerical_flux_ftcs ( simulator, index_node ) result(flux)

        ! bounded argument for this FUNCTION
        class(type_simulator_ftcs) , intent(in) :: simulator

        ! unbounded argument(s) for this FUNCTION
        integer(INT32) , intent(in) :: index_node

        ! return value of this FUNCTION
        real(REAL64) :: flux

        associate ( q => simulator%quantity%copy )
            flux = 0.5_REAL64 * simulator%physical_flux( q( index_node + 1_INT32 ) + q(index_node) )
        end associate

    end function numerical_flux_ftcs



    pure elemental function maxval_index_node_ftcs (simulator) result(index)

        ! bounded argument for this FUNCTION
        class(type_simulator_ftcs) , intent(in) :: simulator

        ! return value of this FUNCTION
        integer(INT32) :: index

        index = size(simulator%quantity) - 1_INT32

    end function maxval_index_node_ftcs



    pure elemental function minval_index_node_ftcs (simulator) result(index)

        ! bounded argument for this FUNCTION
        class(type_simulator_ftcs) , intent(in) :: simulator

        ! return value of this FUNCTION
        integer(INT32) :: index

        index = simulator%minval_index_node_default()

    end function minval_index_node_ftcs

end module simulator_ftcs
