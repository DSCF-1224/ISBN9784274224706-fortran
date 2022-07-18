cmake_minimum_required(VERSION 3.0)

enable_language(Fortran)

project(sample.exe Fortran)

set(CMAKE_Fortran_COMPILER         "gfortran")
set(CMAKE_Fortran_FLAGS            "-ffree-line-length-none -fimplicit-none -pedantic -std=f2008 -Wall -Werror -Wextra")
set(CMAKE_Fortran_FLAGS_DEBUG      "-fbacktrace -fbounds-check -g -O0")
set(CMAKE_Fortran_FLAGS_RELEASE    "-O2")
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/fortran-modules)

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/package)

add_executable(${PROJECT_NAME}
    ${CMAKE_CURRENT_SOURCE_DIR}/program.f90
)

target_link_libraries( ${PROJECT_NAME} PRIVATE _linear_advection_1d )

# EOF
