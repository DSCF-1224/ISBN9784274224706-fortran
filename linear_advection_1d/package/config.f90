module config

    ! required MODULE
    use,     intrinsic :: iso_fortran_env
    use, non_intrinsic :: simple_errmsg_handler

    ! require all variables to be explicitly declared
    implicit none

    ! constant(s) for this MODULE
    integer(INT32) :: MINVAL_ITR = 1_INT32

end module config
