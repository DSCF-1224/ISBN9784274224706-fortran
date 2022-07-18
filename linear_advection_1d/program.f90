program linear_advection_1d

    ! required MODULE(s)
    use ,     intrinsic :: iso_fortran_env
    use , non_intrinsic :: config
    use , non_intrinsic :: simulator_ftcs
    use , non_intrinsic :: simulator_lax
    use , non_intrinsic :: simulator_lax_wendroff
    use , non_intrinsic :: simulator_upwind1
    use , non_intrinsic :: simulator_upwind2



    ! require all variables to be explicitly declared
    implicit none


    ! constant(s) for this PROGRAM
    integer(INT32) , parameter :: num_nodes      = 21
    integer(INT32) , parameter :: num_time_steps =  6



    ! constant(s) for this PROGRAM
    real(REAL64) :: characteristic_velocity = 1.0_REAL64
    real(REAL64) :: step_node               = 0.1_REAL64
    real(REAL64) :: step_time               = 0.05_REAL64



    ! variable(s) for this PROGRAM
    real(REAL64) :: initial_condition (num_nodes)



    ! variable(s) for this PROGRAM
    type( type_simulator_ftcs         ) :: simulator_ftcs
    type( type_simulator_lax          ) :: simulator_lax
    type( type_simulator_lax_wendroff ) :: simulator_lax_wendroff
    type( type_simulator_upwind1      ) :: simulator_upwind1
    type( type_simulator_upwind2      ) :: simulator_upwind2



    ! STEP.01
    ! set the initial distribution of advected quantity
    call set_initial_condition( initial_condition(:) )



    ! STEP.02
    ! preparation of the simulations
    call simulator_ftcs%initialize_field( &!
        num_time_steps          = num_time_steps          , &!
        characteristic_velocity = characteristic_velocity , &!
        step_node               = step_node               , &!
        step_time               = step_time               , &!
        initial_condition       = initial_condition(:)      &!
    )

    call simulator_lax%initialize_field( &!
        num_time_steps          = num_time_steps          , &!
        characteristic_velocity = characteristic_velocity , &!
        step_node               = step_node               , &!
        step_time               = step_time               , &!
        initial_condition       = initial_condition(:)      &!
    )

    call simulator_lax_wendroff%initialize_field( &!
        num_time_steps          = num_time_steps          , &!
        characteristic_velocity = characteristic_velocity , &!
        step_node               = step_node               , &!
        step_time               = step_time               , &!
        initial_condition       = initial_condition(:)      &!
    )

    call simulator_upwind1%initialize_field( &!
        num_time_steps          = num_time_steps          , &!
        characteristic_velocity = characteristic_velocity , &!
        step_node               = step_node               , &!
        step_time               = step_time               , &!
        initial_condition       = initial_condition(:)      &!
    )

    call simulator_upwind2%initialize_field( &!
        num_time_steps          = num_time_steps          , &!
        characteristic_velocity = characteristic_velocity , &!
        step_node               = step_node               , &!
        step_time               = step_time               , &!
        initial_condition       = initial_condition(:)      &!
    )



    ! STEP.03
    ! execute the simulations
    call simulator_ftcs         % execute( '../result/ftcs.dat'         )
    call simulator_lax          % execute( '../result/lax.dat'          )
    call simulator_lax_wendroff % execute( '../result/lax_wendroff.dat' )
    call simulator_upwind1      % execute( '../result/upwind1.dat'      )
    call simulator_upwind2      % execute( '../result/upwind2.dat'      )



    contains



    subroutine set_initial_condition ( initial_condition )

        ! argument(s) for this SUBROUTINE
        real(REAL64) , intent(inout):: initial_condition(num_nodes)

        ! support variable(s) for this SUBROUTINE
        integer(INT32) :: itr_node

        do concurrent ( itr_node = MINVAL_ITR : num_nodes )

            if ( (itr_node - MINVAL_ITR) .le. 0.5_REAL64 * num_nodes ) then
                initial_condition(itr_node) = 1.0_REAL64
            else
                initial_condition(itr_node) = 0.0_REAL64
            end if

        end do

        return

    end subroutine set_initial_condition

end program linear_advection_1d
