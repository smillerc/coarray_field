module test_module
  use iso_fortran_env
  implicit none

  type :: field_t
    real(real64), dimension(:, :), allocatable :: data
  contains
    procedure :: gpu_add
  end type

contains

  subroutine gpu_add(self)
    class(field_t), intent(inout) :: self

    character(len=*), parameter :: sum_kernel = ${sum.cl}$
    character(len=*), parameter :: nbody_kernel = ${nbody.cl}$
  end subroutine
end module test_module
