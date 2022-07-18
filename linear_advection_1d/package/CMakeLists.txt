if (NOT (EXISTS ${CMAKE_Fortran_MODULE_DIRECTORY}/simple_errmsg_handler))
    add_subdirectory( ${CMAKE_CURRENT_SOURCE_DIR}/../../simple_errmsg_handler ${CMAKE_Fortran_MODULE_DIRECTORY}/simple_errmsg_handler )
endif()


add_library(
    _linear_advection_1d
    STATIC
    ${CMAKE_CURRENT_SOURCE_DIR}/advected_quantity.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/config.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/simulator_base.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/simulator_ftcs.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/simulator_lax_base.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/simulator_lax.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/simulator_lax_wendroff.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/simulator_upwind_base.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/simulator_upwind1.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/simulator_upwind2.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/simulator_using_numerical_flux_base.f90
)

target_link_libraries( _linear_advection_1d PRIVATE _simple_errmsg_handler )

# EOF
