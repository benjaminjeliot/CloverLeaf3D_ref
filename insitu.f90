! insitu.f90

module insitu

use ascent
use conduit

implicit none

type(c_ptr) :: ascent_instance

contains

subroutine insitu_initialize()

  use mpi, only: mpi_comm_world

  implicit none

  type(c_ptr) ascent_options

  ascent_instance = ascent_create()

  ascent_options = conduit_node_create()
  call conduit_node_set_path_int32(ascent_options, "mpi_comm", mpi_comm_world)
  call conduit_node_set_path_char8_str(ascent_options, "web/stream", "false")
  call conduit_node_set_path_char8_str(ascent_options, "pipeline/type", "vtkm")

  call ascent_open(ascent_instance, ascent_options)
  call conduit_node_destroy(ascent_options)

end subroutine insitu_initialize

subroutine insitu_execute()

  use conduit_blueprint
  use conduit_blueprint_mesh
  use iso_c_binding, only: c_bool
  use mpi

  use data_module
  use definitions_module
  use ideal_gas_module, only : ideal_gas
  use update_halo_module, only : update_halo
  use viscosity_module, only : viscosity

  implicit none

  integer, parameter :: n_ghost_layers = 2
  integer :: x_min, x_max, y_min, y_max, z_min, z_max
  integer (kind=8) :: n_cells_x, n_cells_y, n_cells_z, n_nodes_x, n_nodes_y, n_nodes_z, n_cells, n_nodes
  integer :: chunk_idx, i, j, k, ghost_flag
  integer :: fields(NUM_FIELDS)
  type(c_ptr) :: mesh_data, verify_info, ascent_actions, add_pipelines_action, pipelines, add_scenes_action, scenes
  logical (kind=c_bool) :: verify_result
  real(8), allocatable :: ghost_mask(:, :, :)

  do chunk_idx = 1, chunks_per_task
    call ideal_gas(chunk_idx, .FALSE.)
  end do

  fields = 0
  fields(FIELD_DENSITY0) = 1
  fields(FIELD_PRESSURE) = 1
  fields(FIELD_VISCOSITY) = 1
  fields(FIELD_XVEL0) = 1
  fields(FIELD_YVEL0) = 1
  fields(FIELD_ZVEL0) = 1
  call update_halo(fields, 1)

  call viscosity()

  do chunk_idx = 1, chunks_per_task
    if (chunks(chunk_idx)%task == parallel%task) then

      x_min = chunks(chunk_idx)%field%x_min
      x_max = chunks(chunk_idx)%field%x_max
      y_min = chunks(chunk_idx)%field%y_min
      y_max = chunks(chunk_idx)%field%y_max
      z_min = chunks(chunk_idx)%field%z_min
      z_max = chunks(chunk_idx)%field%z_max
      n_cells_x = x_max - x_min + 1 + 2 * n_ghost_layers
      n_cells_y = y_max - y_min + 1 + 2 * n_ghost_layers
      n_cells_z = z_max - z_min + 1 + 2 * n_ghost_layers
      n_nodes_x = n_cells_x + 1
      n_nodes_y = n_cells_y + 1
      n_nodes_z = n_cells_z + 1
      n_cells = n_cells_x * n_cells_y * n_cells_z
      n_nodes = n_nodes_x * n_nodes_y * n_nodes_z

      allocate(ghost_mask(x_min - n_ghost_layers : x_max + n_ghost_layers, &
                          y_min - n_ghost_layers : y_max + n_ghost_layers, &
                          z_min - n_ghost_layers : z_max + n_ghost_layers))

      do k = z_min - n_ghost_layers, z_max + n_ghost_layers
        do j = y_min - n_ghost_layers, y_max + n_ghost_layers
          do i = x_min - n_ghost_layers, x_max + n_ghost_layers
            ghost_flag = 0
            if (k < z_min .or. k > z_max) then
              ghost_flag = 1
            end if
            if (j < y_min .or. j > y_max) then
              ghost_flag = 1
            end if
            if (i < x_min .or. i > x_max) then
              ghost_flag = 1
            end if
            ghost_mask(i, j, k) = ghost_flag
          end do
        end do
      end do

      mesh_data = conduit_node_create()

      call conduit_node_set_path_int32(mesh_data, "state/cycle", step)
      call conduit_node_set_path_float64(mesh_data, "state/time", time)
      call conduit_node_set_path_int32(mesh_data, "state/domain_id", parallel%task)

      call conduit_node_set_path_char8_str(mesh_data, "coordsets/coords/type", "rectilinear")
      call conduit_node_set_path_external_float64_ptr(mesh_data, "coordsets/coords/values/x", &
        chunks(chunk_idx)%field%vertexx, n_nodes_x)
      call conduit_node_set_path_external_float64_ptr(mesh_data, "coordsets/coords/values/y", &
        chunks(chunk_idx)%field%vertexy, n_nodes_y)
      call conduit_node_set_path_external_float64_ptr(mesh_data, "coordsets/coords/values/z", &
        chunks(chunk_idx)%field%vertexz, n_nodes_z)
      call conduit_node_set_path_char8_str(mesh_data, "topologies/mesh/type", "rectilinear")
      call conduit_node_set_path_char8_str(mesh_data, "topologies/mesh/coordset", "coords")

      ! ghost mask
      call conduit_node_set_path_char8_str(mesh_data, "fields/ascent_ghosts/association", "element")
      call conduit_node_set_path_char8_str(mesh_data, "fields/ascent_ghosts/topology", "mesh")
      call conduit_node_set_path_char8_str(mesh_data, "fields/ascent_ghosts/type", "scalar")
      call conduit_node_set_path_external_float64_ptr(mesh_data, "fields/ascent_ghosts/values", ghost_mask, n_cells)

      ! density
      call conduit_node_set_path_char8_str(mesh_data, "fields/density/association", "element")
      call conduit_node_set_path_char8_str(mesh_data, "fields/density/topology", "mesh")
      call conduit_node_set_path_external_float64_ptr(mesh_data, "fields/density/values", chunks(chunk_idx)%field%density0, n_cells)

      ! energy
      call conduit_node_set_path_char8_str(mesh_data, "fields/energy/association", "element")
      call conduit_node_set_path_char8_str(mesh_data, "fields/energy/topology", "mesh")
      call conduit_node_set_path_external_float64_ptr(mesh_data, "fields/energy/values", chunks(chunk_idx)%field%energy0, n_cells)

      ! pressure
      call conduit_node_set_path_char8_str(mesh_data, "fields/pressure/association", "element")
      call conduit_node_set_path_char8_str(mesh_data, "fields/pressure/topology", "mesh")
      call conduit_node_set_path_external_float64_ptr(mesh_data, "fields/pressure/values", chunks(chunk_idx)%field%pressure, &
        n_cells)

      ! viscosity
      call conduit_node_set_path_char8_str(mesh_data, "fields/viscosity/association", "element")
      call conduit_node_set_path_char8_str(mesh_data, "fields/viscosity/topology", "mesh")
      call conduit_node_set_path_external_float64_ptr(mesh_data, "fields/viscosity/values", &
        chunks(chunk_idx)%field%viscosity, n_cells)

      ! velocity x, y, z
      call conduit_node_set_path_char8_str(mesh_data, "fields/velocity/association", "vertex")
      call conduit_node_set_path_char8_str(mesh_data, "fields/velocity/topology", "mesh")
      call conduit_node_set_path_external_float64_ptr(mesh_data, "fields/velocity/values/u", chunks(chunk_idx)%field%xvel0, n_nodes)
      call conduit_node_set_path_external_float64_ptr(mesh_data, "fields/velocity/values/v", chunks(chunk_idx)%field%yvel0, n_nodes)
      call conduit_node_set_path_external_float64_ptr(mesh_data, "fields/velocity/values/w", chunks(chunk_idx)%field%zvel0, n_nodes)

      ! Verify that the mesh data is sensible
      verify_info = conduit_node_create()
      verify_result = conduit_blueprint_mesh_verify(mesh_data, verify_info)
      if (verify_result) then
        ! write (*, *) "Mesh verify successful"
      else
        write (*, *) "Mesh verify failed"
        call conduit_node_print_detailed(verify_info)
      end if
      call conduit_node_destroy(verify_info)

      ! Print the JSON for the chunk data
      ! call conduit_node_print_detailed(mesh_data)

      ! Publish the data
      call ascent_publish(ascent_instance, mesh_data)

      ! Create the Ascent actions
      ascent_actions = conduit_node_create()

      add_pipelines_action = conduit_node_append(ascent_actions)
      call conduit_node_set_path_char8_str(add_pipelines_action, "action", "add_pipelines")
      pipelines = conduit_node_fetch(add_pipelines_action, "pipelines")

      ! vector magnitude
      call conduit_node_set_path_char8_str(pipelines, "pl1/f1/type", "vector_magnitude")
      call conduit_node_set_path_char8_str(pipelines, "pl1/f1/params/field", "velocity")
      call conduit_node_set_path_char8_str(pipelines, "pl1/f1/params/output_name", "velocity_magnitude")

      add_scenes_action = conduit_node_append(ascent_actions)
      call conduit_node_set_path_char8_str(add_scenes_action, "action", "add_scenes")
      scenes = conduit_node_fetch(add_scenes_action, "scenes")

      ! density plot
      call conduit_node_set_path_char8_str(scenes, "s1/plots/p1/type", "volume")
      call conduit_node_set_path_char8_str(scenes, "s1/plots/p1/field", "density")
      call conduit_node_set_path_char8_str(scenes, "s1/image_prefix", "density_%04d")

      ! energy plot
      call conduit_node_set_path_char8_str(scenes, "s2/plots/p1/type", "volume")
      call conduit_node_set_path_char8_str(scenes, "s2/plots/p1/field", "energy")
      call conduit_node_set_path_char8_str(scenes, "s2/image_prefix", "energy_%04d")

      ! pressure plot
      call conduit_node_set_path_char8_str(scenes, "s3/plots/p1/type", "volume")
      call conduit_node_set_path_char8_str(scenes, "s3/plots/p1/field", "pressure")
      call conduit_node_set_path_char8_str(scenes, "s3/image_prefix", "pressure_%04d")

      ! viscosity plot
      call conduit_node_set_path_char8_str(scenes, "s4/plots/p1/type", "volume")
      call conduit_node_set_path_char8_str(scenes, "s4/plots/p1/field", "viscosity")
      call conduit_node_set_path_char8_str(scenes, "s4/image_prefix", "viscosity_%04d")

      ! velocity plot
      call conduit_node_set_path_char8_str(scenes, "s5/plots/p1/type", "volume")
      call conduit_node_set_path_char8_str(scenes, "s5/plots/p1/field", "velocity_magnitude")
      call conduit_node_set_path_char8_str(scenes, "s5/plots/p1/pipeline", "pl1")
      call conduit_node_set_path_char8_str(scenes, "s5/image_prefix", "velocity_magnitude_%04d")

      ! Execute the actions
      call ascent_execute(ascent_instance, ascent_actions)

      call conduit_node_destroy(ascent_actions)
      call conduit_node_destroy(mesh_data)

      deallocate(ghost_mask)

    end if
  end do

end subroutine insitu_execute

subroutine insitu_finalize()

  implicit none

  call ascent_close(ascent_instance)
  call ascent_destroy(ascent_instance)

end subroutine insitu_finalize

end module insitu
