! conduit_fortran_extra.F90

module conduit_extra

implicit none

interface

subroutine c_conduit_node_set_path_external_int8_ptr(cnode, path, data, num_elements) &
               bind(C, name="conduit_node_set_path_external_int8_ptr")
    use iso_c_binding
    implicit none
    type(C_PTR), value, intent(IN) :: cnode
    character(kind=C_CHAR), intent(IN) :: path(*)
    integer(kind=c_int8_t), intent (IN), dimension (*) :: data
    integer(C_SIZE_T), value, intent(in) :: num_elements
end subroutine c_conduit_node_set_path_external_int8_ptr

end interface

contains

subroutine conduit_node_set_path_external_int8_ptr(cnode, path, data, num_elements)
    use iso_c_binding
    implicit none
    type(C_PTR), value, intent(IN) :: cnode
    character(*), intent(IN) :: path
    integer(kind=c_int8_t), intent (IN), dimension (*) :: data
    integer(C_SIZE_T), value, intent(in) :: num_elements
    !---
    call c_conduit_node_set_path_external_int8_ptr(cnode, trim(path) // C_NULL_CHAR, data, num_elements)
end subroutine conduit_node_set_path_external_int8_ptr

end module conduit_extra
