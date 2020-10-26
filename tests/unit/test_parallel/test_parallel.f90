program test_parallel
  use iso_fortran_env
  use mod_parallel
  implicit none(type, external)

  integer, parameter :: lower_left = 1
  integer, parameter :: down = 2
  integer, parameter :: lower_right = 3
  integer, parameter :: left = 4
  integer, parameter :: right = 5
  integer, parameter :: upper_left = 6
  integer, parameter :: up = 7
  integer, parameter :: upper_right = 8

  integer, dimension(8) :: neighbors = 0        !< (lower_left, down, lower_right, left, right, upper_left, up, upper_right)

  if(this_image() == 6) then
    neighbors = tile_neighbors_2d(is_periodic=.true.)

    print *, 'neighbors'
    print *, neighbors(upper_left), neighbors(up), neighbors(upper_right)
    print *, neighbors(left), this_image(), neighbors(right)
    print *, neighbors(lower_left), neighbors(down), neighbors(lower_right)
  end if

  print *, "Success"
end program test_parallel
