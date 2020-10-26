module test_mod
  implicit none(type, external)
contains
  subroutine simple_test()
    real, allocatable, dimension(:) :: arr[:]
    allocate(arr(10)[*])
    arr(:)[this_image()] = this_image()
    print *, "# of images", num_images()

    print *, "Test passed."
  end subroutine
end module test_mod

program test_coarray
  use test_mod
  implicit none(type, external)
  call simple_test()
end program test_coarray
