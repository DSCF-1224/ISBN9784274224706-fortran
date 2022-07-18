module advected_quantity

    ! required MODULE
    use,     intrinsic :: iso_fortran_env
    use, non_intrinsic :: config



    ! require all variables to be explicitly declared
    implicit none



    ! accessibility setting
    private
    public  :: type_advected_quantity
    public  :: allocated
    public  :: size



    ! TYPE declaration
    type :: type_advected_quantity

        ! field(s) for this TYPE
        real(REAL64) , public , allocatable :: main(:)
        real(REAL64) , public , allocatable :: copy(:)

        contains

        ! kind: SUBROUTINE
        procedure , private :: allocate_field
        procedure , private :: deallocate_field
        procedure , public  :: reallocate_field
        procedure , public  :: update_copy

    end type type_advected_quantity



    ! INTERFACE declaration
    interface allocated
        module procedure :: is_allocated
    end interface allocated



    ! INTERFACE declaration
    interface size
        module procedure :: size_quantity
    end interface size



    contains



    pure elemental function is_allocated (quantity)

        ! argument(s) for this FUNCTION
        class(type_advected_quantity) , intent(in) :: quantity

        ! return value of this FUNCTION
        logical :: is_allocated

        is_allocated = allocated(quantity%main) .or. allocated(quantity%copy)

    end function is_allocated



    pure elemental function size_quantity (quantity)

        ! argument(s) for this FUNCTION
        class(type_advected_quantity) , intent(in) :: quantity

        ! return value of this FUNCTION
        integer(INT32) :: size_quantity

        if ( allocated(quantity%main) ) then
            size_quantity = size( quantity%main(:) )
        else
            size_quantity = 0_INT32
        end if

    end function size_quantity



    subroutine allocate_field ( quantity, num_nodes )

        ! bounded argument for this SUBROUTINE
        class(type_advected_quantity) , intent(inout) :: quantity

        ! unbounded argument(s) for this SUBROUTINE
        integer(INT32) , intent(in) :: num_nodes

        ! variable(s) for this SUBROUTINE
        type(type_errmsg_handler) :: errmsg_handler

        allocate(&!
            quantity%main(num_nodes)     , &!
            stat   = errmsg_handler%code , &!
            errmsg = errmsg_handler%msg    &!
        )

        call errmsg_handler%validate_allocate(&!
            unit_display   = ERROR_UNIT , &!
            allowance_stop = .true.       &!
        )

        allocate(&!
            quantity%copy(num_nodes)     , &!
            stat   = errmsg_handler%code , &!
            errmsg = errmsg_handler%msg    &!
        )

        call errmsg_handler%validate_allocate(&!
            unit_display   = ERROR_UNIT , &!
            allowance_stop = .true.       &!
        )

        return

    end subroutine allocate_field



    subroutine deallocate_field (quantity)

        ! bounded argument for this SUBROUTINE
        class(type_advected_quantity) , intent(inout) :: quantity

        ! variable(s) for this SUBROUTINE
        type(type_errmsg_handler) :: errmsg_handler

        if ( allocated(quantity%main) ) then

            deallocate(&!
                quantity%main                , &!
                stat   = errmsg_handler%code , &!
                errmsg = errmsg_handler%msg    &!
            )

            call errmsg_handler%validate_allocate(&!
                unit_display   = ERROR_UNIT , &!
                allowance_stop = .true.       &!
            )

        end if

        if ( allocated(quantity%copy) ) then

            deallocate(&!
                quantity%copy                , &!
                stat   = errmsg_handler%code , &!
                errmsg = errmsg_handler%msg    &!
            )

            call errmsg_handler%validate_allocate(&!
                unit_display   = ERROR_UNIT , &!
                allowance_stop = .true.       &!
            )

        end if

        return

    end subroutine deallocate_field



    subroutine reallocate_field ( quantity, num_nodes )

        ! bounded argument for this SUBROUTINE
        class(type_advected_quantity) , intent(inout) :: quantity

        ! unbounded argument(s) for this SUBROUTINE
        integer(INT32) , intent(in) :: num_nodes

        call quantity % deallocate_field
        call quantity %   allocate_field ( num_nodes )

        return

    end subroutine reallocate_field



    subroutine update_copy (quantity)

        ! bounded argument for this SUBROUTINE
        class(type_advected_quantity) , intent(inout) :: quantity

        ! support variable(s) for this SUBROUTINE
        integer(INT32) :: itr_node

        do concurrent ( itr_node = MINVAL_ITR : size(quantity) )
            quantity%copy(itr_node) = &!
            quantity%main(itr_node)
        end do

        return

    end subroutine update_copy

end module advected_quantity
