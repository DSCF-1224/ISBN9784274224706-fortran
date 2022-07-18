module simulator_lax

    ! required MODULE
    use,     intrinsic :: iso_fortran_env
    use, non_intrinsic :: config
    use, non_intrinsic :: simulator_lax_base



    ! require all variables to be explicitly declared
    implicit none



    ! accessibility setting
    private
    public  :: type_simulator_lax



    ! TYPE declaration
    type , extends(type_simulator_lax_base) :: type_simulator_lax

        ! additional field(s) for this TYPE
        ! NONE

        contains

        ! kind: FUNCTION
        procedure , pass , public :: numerical_flux    => numerical_flux_lax
        procedure , pass , public :: maxval_index_node => maxval_index_node_lax
        procedure , pass , public :: minval_index_node => minval_index_node_lax

    end type type_simulator_lax



    contains



    pure elemental function numerical_flux_lax ( simulator, index_node ) result(flux)

        ! bounded argument for this FUNCTION
        class(type_simulator_lax) , intent(in) :: simulator

        ! unbounded argument(s) for this FUNCTION
        integer(INT32) , intent(in) :: index_node

        ! return value of this FUNCTION
        real(REAL64) :: flux

        associate ( q => simulator%quantity%copy )

            flux = simulator%numerical_flux_lax_base(&!
                q_minus = q( index_node + 1_INT32 ) , &!
                q_plus  = q( index_node           ) , &!
                nu      = 1.0_REAL64 / simulator%courant_number &!
            )

        end associate

    end function numerical_flux_lax



    pure elemental function maxval_index_node_lax (simulator) result(index)

        ! bounded argument for this FUNCTION
        class(type_simulator_lax) , intent(in) :: simulator

        ! return value of this FUNCTION
        integer(INT32) :: index

        index = size(simulator%quantity) - 1_INT32

    end function maxval_index_node_lax



    pure elemental function minval_index_node_lax (simulator) result(index)

        ! bounded argument for this FUNCTION
        class(type_simulator_lax) , intent(in) :: simulator

        ! return value of this FUNCTION
        integer(INT32) :: index

        index = simulator%minval_index_node_default()

    end function minval_index_node_lax

end module simulator_lax
