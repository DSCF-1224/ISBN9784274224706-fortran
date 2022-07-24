module pkg_simulator_harten_yee

    ! required MODULE
    use,     intrinsic :: iso_fortran_env
    use, non_intrinsic :: config
    use, non_intrinsic :: pkg_simulator_using_numerical_flux_base
    use, non_intrinsic :: pkg_flux_limiter



    ! require all variables to be explicitly declared
    implicit none



    ! accessibility setting
    private
    public  :: type_simulator_harten_yee



    ! TYPE declaration
    type , extends(type_simulator_using_numerical_flux_base) :: type_simulator_harten_yee

        ! additional field(s) for this TYPE
        ! NONE

        contains

        ! kind: FUNCTION
        procedure ,   pass , public  :: numerical_flux    => numerical_flux_harten_yee
        procedure ,   pass , public  :: maxval_index_node => maxval_index_node_harten_yee
        procedure ,   pass , public  :: minval_index_node => minval_index_node_harten_yee
        procedure ,   pass , private :: calc_tvd_delta
        procedure ,   pass , private :: tvd_g
        procedure , nopass , private :: tvd_gamma
        procedure ,   pass , private :: tvd_phi
        procedure ,   pass , private :: calc_tvd_sigma

    end type type_simulator_harten_yee



    contains



    pure elemental function numerical_flux_harten_yee ( simulator, index_node ) result(flux)

        ! bounded argument for this FUNCTION
        class(type_simulator_harten_yee) , intent(in) :: simulator

        ! unbounded argument(s) for this FUNCTION
        integer(INT32) , intent(in) :: index_node

        ! return value of this FUNCTION
        real(REAL64) :: flux

        ! variable(s) for this FUNCTION

        associate ( q => simulator%quantity%copy )
            flux = 0.5_REAL64 * ( simulator%physical_flux( q( index_node + 1_INT32 ) + q(index_node) ) + simulator%tvd_phi(index_node) )
        end associate

    end function numerical_flux_harten_yee



    pure elemental function maxval_index_node_harten_yee (simulator) result(index)

        ! bounded argument for this FUNCTION
        class(type_simulator_harten_yee) , intent(in) :: simulator

        ! return value of this FUNCTION
        integer(INT32) :: index

        index = size(simulator%quantity) - 2_INT32

    end function maxval_index_node_harten_yee



    pure elemental function minval_index_node_harten_yee (simulator) result(index)

        ! bounded argument for this FUNCTION
        class(type_simulator_harten_yee) , intent(in) :: simulator

        ! return value of this FUNCTION
        integer(INT32) :: index

        index = simulator%minval_index_node_default() + 1_INT32

    end function minval_index_node_harten_yee



    pure elemental function calc_tvd_delta ( simulator, index_node ) result(tvd_delta)

        ! bounded argument for this FUNCTION
        class   (type_simulator_harten_yee) , intent(in) :: simulator
        integer (INT32)                     , intent(in) :: index_node

        ! return value of this FUNCTION
        real(REAL64) :: tvd_delta

        associate ( q => simulator%quantity%copy )
            tvd_delta = q( index_node + 1_INT32 ) - q(index_node)
        end associate

    end function calc_tvd_delta



    pure elemental function tvd_g ( simulator, index_node )

        ! bounded argument for this FUNCTION
        class   (type_simulator_harten_yee) , intent(in) :: simulator
        integer (INT32)                     , intent(in) :: index_node

        ! return value of this FUNCTION
        real(REAL64) :: tvd_g

        tvd_g = &!
            minmod( &!
                x = simulator%calc_tvd_delta( index_node           ) , &!
                y = simulator%calc_tvd_delta( index_node - 1_INT32 )   &!
            )

    end function tvd_g



    pure elemental function tvd_gamma ( tvd_g_left, tvd_g_right, tvd_delta, tvd_sigma )

        ! bounded argument for this FUNCTION
        real(REAL64) , intent(in) :: tvd_g_left
        real(REAL64) , intent(in) :: tvd_g_right
        real(REAL64) , intent(in) :: tvd_delta
        real(REAL64) , intent(in) :: tvd_sigma

        ! return value of this FUNCTION
        real(REAL64) :: tvd_gamma

        tvd_gamma &!
            = tvd_sigma &!
            * ( tvd_g_right - tvd_g_left ) &!
            * tvd_delta &!
            / ( tvd_delta * tvd_delta + 1.0e-012_REAL64 )

    end function tvd_gamma



    pure elemental function tvd_phi ( simulator, index_node )

        ! bounded argument for this FUNCTION
        class   (type_simulator_harten_yee) , intent(in) :: simulator
        integer (INT32)                     , intent(in) :: index_node

        ! return value of this FUNCTION
        real(REAL64) :: tvd_phi

        ! variable(s) for this FUNCTION
        integer ( INT32  ) :: index_left
        integer ( INT32  ) :: index_right
        real    ( REAL64 ) :: tvd_delta
        real    ( REAL64 ) :: tvd_g_left
        real    ( REAL64 ) :: tvd_g_right
        real    ( REAL64 ) :: tvd_sigma

        associate ( c => simulator%characteristic_velocity )

            index_left  = index_node
            index_right = index_node + 1_INT32

            tvd_g_left  = simulator%tvd_g          ( index_left  )
            tvd_g_right = simulator%tvd_g          ( index_right )
            tvd_delta   = simulator%calc_tvd_delta ( index_node  )
            tvd_sigma   = simulator%calc_tvd_sigma ()

            tvd_phi &!
                = tvd_sigma &!
                * ( tvd_g_left + tvd_g_right ) &!
                - abs( c + simulator%tvd_gamma( tvd_g_left= tvd_g_left, tvd_g_right= tvd_g_right, tvd_delta= tvd_delta, tvd_sigma= tvd_sigma ) ) &!
                * tvd_delta

        end associate

    end function tvd_phi




    pure elemental function calc_tvd_sigma (simulator) result(tvd_sigma)

        ! bounded argument for this FUNCTION
        class(type_simulator_harten_yee) , intent(in) :: simulator

        ! return value of this FUNCTION
        real(REAL64) :: tvd_sigma

        associate ( &!
            c  => simulator%characteristic_velocity , &!
            nu => simulator%courant_number            &!
        )

            tvd_sigma = 0.5_REAL64 * ( abs(c) - nu * c )

        end associate

    end function calc_tvd_sigma

end module pkg_simulator_harten_yee
