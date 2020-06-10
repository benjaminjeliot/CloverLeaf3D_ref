# CMakeLists.txt

cmake_minimum_required(VERSION 3.12)

project(cloverleaf3d LANGUAGES CXX C Fortran)

option(ENABLE_SENSEI "Enable SENSEI in-situ visualization and analysis support" ON)

find_package(MPI REQUIRED COMPONENTS Fortran CXX C)

if(ENABLE_SENSEI)
  find_package(SENSEI REQUIRED)
endif()

add_executable(cloverleaf3d
  data.f90
  definitions.f90
  pack_kernel.f90
  clover.f90
  report.f90
  timer.f90
  parse.f90
  read_input.f90
  initialise_chunk_kernel.f90
  initialise_chunk.f90
  build_field.f90
  update_halo_kernel.f90
  update_halo.f90
  ideal_gas_kernel.f90
  ideal_gas.f90
  start.F90
  generate_chunk_kernel.f90
  generate_chunk.f90
  initialise.f90
  field_summary_kernel.f90
  field_summary.f90
  viscosity_kernel.f90
  viscosity.f90
  calc_dt_kernel.f90
  calc_dt.f90
  timestep.f90
  accelerate_kernel.f90
  accelerate.f90
  revert_kernel.f90
  revert.f90
  PdV_kernel.f90
  PdV.f90
  flux_calc_kernel.f90
  flux_calc.f90
  advec_cell_kernel.f90
  advec_cell_driver.f90
  advec_mom_kernel.f90
  advec_mom_driver.f90
  advection.f90
  reset_field_kernel.f90
  reset_field.f90
  hydro.F90
  visit.f90
  clover_leaf.f90
  timer_c.c
)

target_link_libraries(cloverleaf3d PRIVATE MPI::MPI_Fortran)

set(SENSEI_INSITU_SOURCE_FILES
  conduit_fortran_extra.F90
  insitu.f90
  sensei_bridge.cpp
)

if (ENABLE_SENSEI)
  target_sources(cloverleaf3d PRIVATE ${SENSEI_INSITU_SOURCE_FILES})
  target_compile_definitions(cloverleaf3d PRIVATE "SENSEI_ENABLED")
  target_link_libraries(cloverleaf3d PRIVATE sensei)
endif()

