module simulator_base

    ! required MODULE
    use,     intrinsic :: iso_fortran_env
    use, non_intrinsic :: config
    use, non_intrinsic :: advected_quantity



    ! require all variables to be explicitly declared
    implicit none



    ! accessibility setting
    private
    public  :: type_simulator_base
    public  :: size



    ! TYPE declaration
    type , abstract :: type_simulator_base

        ! field(s) for this TYPE
        integer(INT32) , public :: num_nodes
        integer(INT32) , public :: num_time_steps

        ! field(s) for this TYPE
        real(REAL64) , public :: characteristic_velocity
        real(REAL64) , public :: courant_number
        real(REAL64) , public :: step_node
        real(REAL64) , public :: step_time

        ! field(s) for this TYPE
        type(type_advected_quantity) :: quantity

        contains

        ! kind: FUNCTION
        procedure( maxval_index_node_abstract ) , pass , public , deferred :: maxval_index_node
        procedure( minval_index_node_abstract ) , pass , public , deferred :: minval_index_node
        procedure( quantity_change_abstract   ) , pass , public , deferred :: quantity_change

        ! kind: FUNCTION
        procedure ,   pass , private :: node_coordinate
        procedure ,   pass , private :: physical_flux_on_node
        procedure ,   pass , private :: physical_flux_abs_using_advected_quantity
        procedure ,   pass , private :: physical_flux_using_advected_quantity
        procedure , nopass , public  :: minval_index_node_default

        ! kind: SUBROUTINE
        procedure , pass , public  :: execute
        procedure , pass , private :: evolute_time
        procedure , pass , public  :: initialize_field
        procedure , pass , private :: save_solution
        procedure , pass , private :: validate_conditions

        ! kind: INTERFACE
        generic , public :: physical_flux     => physical_flux_on_node
        generic , public :: physical_flux     => physical_flux_using_advected_quantity
        generic , public :: physical_flux_abs => physical_flux_abs_using_advected_quantity

    end type type_simulator_base



    ! INTERFACE declaration
    interface

        pure elemental function maxval_index_node_abstract (simulator) result(index)

            ! required TYPE(s) specification
            import INT32
            import type_simulator_base

            ! bounded argument for this FUNCTION
            class(type_simulator_base) , intent(in) :: simulator

            ! return value of this FUNCTION
            integer(INT32) :: index

        end function maxval_index_node_abstract



        pure elemental function minval_index_node_abstract (simulator) result(index)

            ! required TYPE(s) specification
            import INT32
            import type_simulator_base

            ! bounded argument for this FUNCTION
            class(type_simulator_base) , intent(in) :: simulator

            ! return value of this FUNCTION
            integer(INT32) :: index

        end function minval_index_node_abstract



        pure elemental function quantity_change_abstract ( simulator, index_node ) result(quantity_change)

            ! required TYPE(s) specification
            import INT32
            import REAL64
            import type_simulator_base

            ! bounded argument for this FUNCTION
            class(type_simulator_base) , intent(in) :: simulator

            ! unbounded argument(s) for this FUNCTION
            integer(INT32) , intent(in) :: index_node

            ! return value of this FUNCTION
            real(REAL64) :: quantity_change

        end function quantity_change_abstract

    end interface



    ! INTERFACE declaration
    interface allocated
        module procedure :: is_allocated
    end interface allocated



    contains



    pure elemental function is_allocated (simulator)

        ! argument(s) for this FUNCTION
        class(type_simulator_base) , intent(in) :: simulator

        ! return value of this FUNCTION
        logical :: is_allocated

        is_allocated = allocated(simulator%quantity)

    end function is_allocated



    pure elemental function minval_index_node_default () result(index)

        ! return value of this FUNCTION
        integer(INT32) :: index

        index = 2_INT32

    end function minval_index_node_default



    pure elemental function node_coordinate ( simulator, index_node )

        ! bounded argument for this FUNCTION
        class(type_simulator_base) , intent(in) :: simulator

        ! unbounded argument(s) for this FUNCTION
        integer(INT32) , intent(in) :: index_node

        ! return value of this FUNCTION
        real(REAL64) :: node_coordinate

        node_coordinate = simulator%step_node * (index_node - 1_INT32)

    end function node_coordinate



    pure elemental function physical_flux_on_node ( simulator, index_node ) result(flux)

        ! bounded argument for this FUNCTION
        class(type_simulator_base) , intent(in) :: simulator

        ! unbounded argument(s) for this FUNCTION
        integer(INT32) , intent(in) :: index_node

        ! return value of this FUNCTION
        real(REAL64) :: flux

        associate ( q => simulator%quantity%copy )
            flux = simulator%physical_flux( q(index_node) )
        end associate

    end function physical_flux_on_node



    pure elemental function physical_flux_using_advected_quantity ( simulator, quantity ) result(flux)

        ! bounded argument for this FUNCTION
        class(type_simulator_base) , intent(in) :: simulator

        ! unbounded argument(s) for this FUNCTION
        real(REAL64) , intent(in) :: quantity

        ! return value of this FUNCTION
        real(REAL64) :: flux

        associate ( c => simulator%characteristic_velocity )
            flux = c * quantity
        end associate

    end function physical_flux_using_advected_quantity



    pure elemental function physical_flux_abs_using_advected_quantity ( simulator, quantity ) result(flux)

        ! bounded argument for this FUNCTION
        class(type_simulator_base) , intent(in) :: simulator

        ! unbounded argument(s) for this FUNCTION
        real(REAL64) , intent(in) :: quantity

        ! return value of this FUNCTION
        real(REAL64) :: flux

        associate ( c => simulator%characteristic_velocity )
            flux = abs(c) * quantity
        end associate

    end function physical_flux_abs_using_advected_quantity



    subroutine evolute_time (simulator)

        ! bounded argument for this SUBROUTINE
        class(type_simulator_base) , intent(inout) :: simulator

        ! support variable(s) for this SUBROUTINE
        integer(INT32) :: itr_node

        ! STEP.01
        ! copy the last quantity at each node
        call simulator%quantity%update_copy

        ! STEP.02
        ! evolute the quantity at each node
        associate ( q => simulator%quantity%main )

            do concurrent ( itr_node = simulator%minval_index_node() : simulator%maxval_index_node() )
                q(itr_node) = &!
                q(itr_node) + simulator%quantity_change(itr_node)
            end do

        end associate

        ! STEP.END
        return

    end subroutine evolute_time



    subroutine execute ( simulator, file_path )

        ! bounded argument for this SUBROUTINE
        class(type_simulator_base) , intent(inout) :: simulator

        ! unbounded argument(s) for this SUBROUTINE
        character( len= * ) , intent(in) :: file_path

        ! variable(s) for this SUBROUTINE
        integer :: unit_save

        ! variable(s) for this SUBROUTINE
        type(type_iomsg_handler) :: iomsg_handler

        ! support variable(s) for this SUBROUTINE
        integer(INT32) :: itr_time_step


        ! STEP.01
        ! open a file to save computed result
        open(&!
            newunit = unit_save          , &!
            file    = file_path          , &!
            action  = 'write'            , &!
            form    = 'formatted'        , &!
            status  = 'unknown'          , &!
            iostat  = iomsg_handler%code , &!
            iomsg   = iomsg_handler%msg    &!
        )

        call iomsg_handler%validate_open(&!
            unit_display   = ERROR_UNIT , &!
            allowance_stop = .true.       &!
        )

        ! STEP.02
        ! save initial condition
        call simulator%save_solution( unit_save )

        ! STEP.03
        ! compute & save the solution
        do itr_time_step = MINVAL_ITR , simulator%num_time_steps

            ! STEP.01
            ! evolute time
            call simulator%evolute_time

            ! STEP.02
            ! save initial condition
            call simulator%save_solution( unit_save )

        end do

        ! STEP.0X
        ! close the used file
        close(&!
            unit   = unit_save          , &!
            iostat = iomsg_handler%code , &!
            iomsg  = iomsg_handler%msg    &!
        )

        call iomsg_handler%validate_close(&!
            unit_display   = ERROR_UNIT , &!
            allowance_stop = .true.       &!
        )

        ! STEP.END
        return

    end subroutine execute



    subroutine initialize_field ( simulator, num_time_steps, characteristic_velocity, step_node, step_time, initial_condition )

        ! bounded argument for this SUBROUTINE
        class(type_simulator_base) , intent(inout) :: simulator

        ! unbounded argument(s) for this SUBROUTINE
        integer(INT32) , intent(in) :: num_time_steps

        ! unbounded argument(s) for this SUBROUTINE
        real(REAL64) , intent(in) :: characteristic_velocity
        real(REAL64) , intent(in) :: step_node
        real(REAL64) , intent(in) :: step_time

        ! unbounded argument(s) for this SUBROUTINE
        real(REAL64) , intent(in) :: initial_condition(:)

        simulator % num_time_steps          = num_time_steps
        simulator % characteristic_velocity = characteristic_velocity
        simulator % step_node               = step_node
        simulator % step_time               = step_time

        call simulator%validate_conditions

        simulator % courant_number = characteristic_velocity * step_time / step_node
        simulator % num_nodes      = size( initial_condition )

        call simulator%quantity%reallocate_field( simulator%num_nodes )

        simulator%quantity%main(:) = initial_condition(:)

        return

    end subroutine initialize_field



    subroutine save_solution ( simulator, unit_save )

        ! bounded argument for this SUBROUTINE
        class(type_simulator_base) , intent(in) :: simulator

        ! unbounded argument(s) for this SUBROUTINE
        integer , intent(in) :: unit_save

        ! variable(s) for this SUBROUTINE
        type(type_iomsg_handler) :: iomsg_handler

        ! support variable(s) for this SUBROUTINE
        integer(INT32) :: itr_node

        ! STEP.01
        ! write computed quantity at each node
        do itr_node = MINVAL_ITR , simulator%num_nodes

            write(&!
                unit   = unit_save          , &!
                fmt    = '(*(ES25.16E3))'   , &!
                iostat = iomsg_handler%code , &!
                iomsg  = iomsg_handler%msg    &!
            ) &!
                simulator%node_coordinate ( itr_node ) , &!
                simulator%quantity%main   ( itr_node )

            call iomsg_handler%validate_write(&!
                unit_display   = ERROR_UNIT , &!
                allowance_stop = .true.       &!
            )

        end do

        ! STEP.02
        ! write a blank line
        write(&!
            unit   = unit_save          , &!
            fmt    = *                  , &!
            iostat = iomsg_handler%code , &!
            iomsg  = iomsg_handler%msg    &!
        ) ! NOTHING TO WRITE

        call iomsg_handler%validate_write(&!
            unit_display   = ERROR_UNIT , &!
            allowance_stop = .true.       &!
        )

        return

    end subroutine save_solution



    subroutine validate_conditions ( simulator )

        ! bounded argument for this SUBROUTINE
        class(type_simulator_base) , intent(in) :: simulator

        ! variable(s) for this SUBROUTINE
        logical :: is_valid

        is_valid = .true.

        if ( simulator%num_time_steps .le. 0.0_REAL64 ) then
            write( unit= ERROR_UNIT, fmt= '(A)' ) 'The number of time steps must be greater than ZERO.'
            is_valid = .false.
        end if

        if ( simulator%step_node .lt. 0.0_REAL64 ) then
            write( unit= ERROR_UNIT, fmt= '(A)' ) 'Spatial node step must be greater than ZERO.'
            is_valid = .false.
        end if

        if ( simulator%step_time .lt. 0.0_REAL64 ) then
            write( unit= ERROR_UNIT, fmt= '(A)' ) 'Time step must be greater than ZERO.'
            is_valid = .false.
        end if

        if ( .not. is_valid ) then
            write( unit= ERROR_UNIT, fmt= '(A)' ) 'Failed to execute the simulation.'
            stop
        end if

        return

    end subroutine validate_conditions

end module simulator_base
