program linear_advection_1d

    ! required MODULE(s)
    use ,     intrinsic :: iso_fortran_env
    use , non_intrinsic :: config
    use , non_intrinsic :: pkg_simulator_ftcs
    use , non_intrinsic :: pkg_simulator_harten_yee
    use , non_intrinsic :: pkg_simulator_lax
    use , non_intrinsic :: pkg_simulator_lax_wendroff
    use , non_intrinsic :: pkg_simulator_upwind1
    use , non_intrinsic :: pkg_simulator_upwind2



    ! require all variables to be explicitly declared
    implicit none


    ! constant(s) for this PROGRAM
    integer(INT32) , parameter :: num_nodes      = 21
    integer(INT32) , parameter :: num_time_steps =  6



    ! constant(s) for this PROGRAM
    real(REAL64) , parameter :: step_node = 1.0e-001_REAL64
    real(REAL64) , parameter :: step_time = 5.0e-002_REAL64



    call execute_samples ( &!
        characteristic_velocity =  1.0_REAL64 , &!
        quantity_left           =  1.0_REAL64 , &!
        quantity_right          =  0.0_REAL64 , &!
        folder_path             =  'sample1'    &!
    )

    call execute_samples ( &!
        characteristic_velocity = -1.0_REAL64 , &!
        quantity_left           =  1.0_REAL64 , &!
        quantity_right          =  0.0_REAL64 , &!
        folder_path             =  'sample2'    &!
    )

    call execute_samples ( &!
        characteristic_velocity =  1.0_REAL64 , &!
        quantity_left           =  0.0_REAL64 , &!
        quantity_right          =  1.0_REAL64 , &!
        folder_path             =  'sample3'    &!
    )

    call execute_samples ( &!
        characteristic_velocity = -1.0_REAL64 , &!
        quantity_left           =  0.0_REAL64 , &!
        quantity_right          =  1.0_REAL64 , &!
        folder_path             =  'sample4'    &!
    )



    contains



    subroutine execute_samples ( characteristic_velocity, quantity_left, quantity_right, folder_path )

        ! argument(s) for this SUBROUTINE
        real(REAL64) , intent(in) :: characteristic_velocity
        real(REAL64) , intent(in) :: quantity_left
        real(REAL64) , intent(in) :: quantity_right

        ! argument(s) for this SUBROUTINE
        character( len= * ) , intent(in) :: folder_path

        ! variable(s) for this PROGRAM
        real(REAL64) :: initial_quantity (num_nodes)

        ! variable(s) for this PROGRAM
        type( type_simulator_ftcs         ) :: simulator_ftcs
        type( type_simulator_harten_yee   ) :: simulator_harten_yee
        type( type_simulator_lax          ) :: simulator_lax
        type( type_simulator_lax_wendroff ) :: simulator_lax_wendroff
        type( type_simulator_upwind1      ) :: simulator_upwind1
        type( type_simulator_upwind2      ) :: simulator_upwind2



        ! STEP.01
        ! set the initial distribution of advected quantity
        call set_initial_condition(&!
            quantity_left    = quantity_left       , &!
            quantity_right   = quantity_right      , &!
            initial_quantity = initial_quantity(:)   &!
        )



        ! STEP.02
        ! preparation of the simulations
        call simulator_ftcs%initialize_field( &!
            num_time_steps          = num_time_steps          , &!
            characteristic_velocity = characteristic_velocity , &!
            step_node               = step_node               , &!
            step_time               = step_time               , &!
            initial_condition       = initial_quantity(:)       &!
        )

        call simulator_harten_yee%initialize_field( &!
            num_time_steps          = num_time_steps          , &!
            characteristic_velocity = characteristic_velocity , &!
            step_node               = step_node               , &!
            step_time               = step_time               , &!
            initial_condition       = initial_quantity(:)       &!
        )

        call simulator_lax%initialize_field( &!
            num_time_steps          = num_time_steps          , &!
            characteristic_velocity = characteristic_velocity , &!
            step_node               = step_node               , &!
            step_time               = step_time               , &!
            initial_condition       = initial_quantity(:)       &!
        )

        call simulator_lax_wendroff%initialize_field( &!
            num_time_steps          = num_time_steps          , &!
            characteristic_velocity = characteristic_velocity , &!
            step_node               = step_node               , &!
            step_time               = step_time               , &!
            initial_condition       = initial_quantity(:)       &!
        )

        call simulator_upwind1%initialize_field( &!
            num_time_steps          = num_time_steps          , &!
            characteristic_velocity = characteristic_velocity , &!
            step_node               = step_node               , &!
            step_time               = step_time               , &!
            initial_condition       = initial_quantity(:)       &!
        )

        call simulator_upwind2%initialize_field( &!
            num_time_steps          = num_time_steps          , &!
            characteristic_velocity = characteristic_velocity , &!
            step_node               = step_node               , &!
            step_time               = step_time               , &!
            initial_condition       = initial_quantity(:)       &!
        )



        ! STEP.03
        ! execute the simulations
        call simulator_ftcs         % execute( '../result/' // folder_path // '/ftcs.dat'         )
        call simulator_harten_yee   % execute( '../result/' // folder_path // '/harten_yee.dat'   )
        call simulator_lax          % execute( '../result/' // folder_path // '/lax.dat'          )
        call simulator_lax_wendroff % execute( '../result/' // folder_path // '/lax_wendroff.dat' )
        call simulator_upwind1      % execute( '../result/' // folder_path // '/upwind1.dat'      )
        call simulator_upwind2      % execute( '../result/' // folder_path // '/upwind2.dat'      )



        ! STEP.END
        return

    end subroutine execute_samples



    subroutine set_initial_condition ( quantity_left, quantity_right, initial_quantity )

        ! argument(s) for this SUBROUTINE
        real(REAL64) , intent( in    ) :: quantity_left
        real(REAL64) , intent( in    ) :: quantity_right
        real(REAL64) , intent( inout ) :: initial_quantity (num_nodes)

        ! support variable(s) for this SUBROUTINE
        integer(INT32) :: itr_node

        do concurrent ( itr_node = MINVAL_ITR : num_nodes )

            if ( (itr_node - MINVAL_ITR) .le. 0.5_REAL64 * num_nodes ) then
                initial_quantity(itr_node) = quantity_left
            else
                initial_quantity(itr_node) = quantity_right
            end if

        end do

        return

    end subroutine set_initial_condition

end program linear_advection_1d
