cmake_minimum_required(VERSION 3.0)

project(fortran-cli Fortran)

if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set (CMAKE_Fortran_FLAGS "-ffree-line-length-none -g -fbacktrace")
endif ()

if (${CMAKE_PROJECT_NAME} STREQUAL ${PROJECT_NAME})
  add_subdirectory (lib/fortran-container)
  add_subdirectory (lib/fortran-string)
endif ()

set(source
  src/cli.F90
  src/cli_mod.F90
  src/parse_arg_mod.F90
)

add_library (fortran_cli SHARED ${source})
target_link_libraries (fortran_cli fortran_container fortran_string)