program data_layout
  use iso_fortran_env
  use omp_lib, only: omp_get_wtime

  implicit none (type, external)


  real(real64) :: t_start, t_end

  real(real64), dimension(:,:), allocatable :: rho, u, v, p, E, buff
  real(real64), dimension(:,:,:), allocatable :: prim_vars !< (rho, u, v, p), i, j)
  real(real64), dimension(:,:,:), allocatable :: packed !< ((rho, u, v, p, E), i, j)
  real(real64) :: single(4)
  integer(int32) :: i, j, ni, nj
  real(real64) :: gamma_minus_one = (5.0_real64/3.0_real64) - 1.0_real64

  ni = 1000
  nj = 1000

  allocate(rho(ni, nj))
  allocate(u(ni, nj))
  allocate(v(ni, nj))
  allocate(p(ni, nj))
  allocate(E(ni, nj))
  allocate(buff(ni, nj))
  rho = 1.0_real64
  u = 2.0_real64
  v = 3.0_real64
  p = 4.0_real64
  E = 0.0_real64
  buff = 0.0_real64
  
  allocate(prim_vars(4, ni, nj))
  prim_vars = 0.0_real64
  prim_vars(1,:,:) = rho
  prim_vars(2,:,:) = u
  prim_vars(3,:,:) = v
  prim_vars(4,:,:) = p
  
  allocate(packed(5, ni, nj))
  packed = 0.0_real64
  packed(1,:,:) = rho
  packed(2,:,:) = u
  packed(3,:,:) = v
  packed(4,:,:) = p
  packed(5,:,:) = E

  t_start = omp_get_wtime()
  do j = 1, nj - 1
    do i = 1, ni - 1
      E(i,j) = (p(i,j) / (rho(i,j) * gamma_minus_one)) + 0.5_real64*(u(i,j) * u(i,j) + v(i,j) * v(i,j))
    end do
  end do
  t_end = omp_get_wtime()
  if(this_image() == 1) write(*, '(a, es16.6)') "Version  1: ", t_end - t_start

  t_start = omp_get_wtime()
  do j = 1, nj - 1
    do i = 1, ni - 1
      E(i,j) = (prim_vars(4,i,j) / (prim_vars(1, i,j) * gamma_minus_one)) + 0.5_real64*(prim_vars(2,i,j) * prim_vars(2,i,j) + prim_vars(3,i,j) * prim_vars(3,i,j))
    end do
  end do
  t_end = omp_get_wtime()
  if(this_image() == 1) write(*, '(a, es16.6)') "Version 2a: ", t_end - t_start

  t_start = omp_get_wtime()
  do j = 1, nj - 1
    do i = 1, ni - 1
      single = prim_vars(:,i,j)
      E(i,j) = (single(4) / (single(1) * gamma_minus_one)) + 0.5_real64*(single(2) * single(2) + single(3) * single(3))
    end do
  end do
  t_end = omp_get_wtime()
  if(this_image() == 1) write(*, '(a, es16.6)') "Version 2b: ", t_end - t_start

  t_start = omp_get_wtime()
  do j = 1, nj - 1
    do i = 1, ni - 1
      packed(5,i,j) = (packed(4,i,j) / (packed(1, i,j) * gamma_minus_one)) + 0.5_real64*(packed(2,i,j) * packed(2,i,j) + packed(3,i,j) * packed(3,i,j))
    end do
  end do
  t_end = omp_get_wtime()
  if(this_image() == 1) write(*, '(a, es16.6)') "Version  3: ", t_end - t_start
  
  
  t_start = omp_get_wtime()
  E(:,:) = (prim_vars(4,:,:) / (prim_vars(1,:,:) * gamma_minus_one)) + 0.5_real64*(prim_vars(2,:,:) * prim_vars(2,:,:) + prim_vars(3,:,:) * prim_vars(3,:,:))
  t_end = omp_get_wtime()
  if(this_image() == 1) write(*, '(a, es16.6)') "Version  4: ", t_end - t_start

  ! SIMD versions

  t_start = omp_get_wtime()
  do j = 1, nj - 1
    !$omp simd
    do i = 1, ni - 1
      E(i,j) = (p(i,j) / (rho(i,j) * gamma_minus_one)) + 0.5_real64*(u(i,j) * u(i,j) + v(i,j) * v(i,j))
    end do
     !$omp end simd
  end do
  t_end = omp_get_wtime()
  if(this_image() == 1) write(*, '(a, es16.6)') "Version 1  SIMD: ", t_end - t_start

  t_start = omp_get_wtime()
  do j = 1, nj - 1
    !$omp simd
    do i = 1, ni - 1
      E(i,j) = (prim_vars(4,i,j) / (prim_vars(1, i,j) * gamma_minus_one)) + 0.5_real64*(prim_vars(2,i,j) * prim_vars(2,i,j) + prim_vars(3,i,j) * prim_vars(3,i,j))
    end do
    !$omp end simd
  end do
  t_end = omp_get_wtime()
  if(this_image() == 1) write(*, '(a, es16.6)') "Version 2a SIMD: ", t_end - t_start

  t_start = omp_get_wtime()
  do j = 1, nj - 1
    !$omp simd
    do i = 1, ni - 1
      single = prim_vars(:,i,j)
      E(i,j) = (single(4) / (single(1) * gamma_minus_one)) + 0.5_real64*(single(2) * single(2) + single(3) * single(3))
    end do
  end do
  t_end = omp_get_wtime()
  if(this_image() == 1) write(*, '(a, es16.6)') "Version 2b SIMD: ", t_end - t_start

  t_start = omp_get_wtime()
  do j = 1, nj - 1
    !$omp simd
    do i = 1, ni - 1
      packed(5,i,j) = (packed(4,i,j) / (packed(1, i,j) * gamma_minus_one)) + 0.5_real64*(packed(2,i,j) * packed(2,i,j) + packed(3,i,j) * packed(3,i,j))
    end do
     !$omp end simd
  end do
  t_end = omp_get_wtime()
  if(this_image() == 1) write(*, '(a, es16.6)') "Version 3  SIMD: ", t_end - t_start

  ! t_start = omp_get_wtime()
  ! !$omp simd
  ! E(:,:) = (prim_vars(4,:,:) / (prim_vars(1,:,:) * gamma_minus_one)) + 0.5_real64*(prim_vars(2,:,:) * prim_vars(2,:,:) + prim_vars(3,:,:) * prim_vars(3,:,:))
  ! !$omp end simd
  ! t_end = omp_get_wtime()
  ! write(*, '(a, es16.6)') "Version 4 SIMD: ", t_end - t_start

  t_start = omp_get_wtime()
  do j = 1, nj - 1
    !$omp simd
    do i = 1, ni - 1
      buff(i,j) = u(i,j) * u(i,j) + v(i,j) * v(i,j)
    end do
     !$omp end simd
  end do

  do j = 1, nj - 1
    !$omp simd
    do i = 1, ni - 1
      E(i,j) = (p(i,j) / (rho(i,j) * gamma_minus_one)) + 0.5_real64*buff(i,j)
    end do
     !$omp end simd
  end do
  t_end = omp_get_wtime()
  if(this_image() == 1) write(*, '(a, es16.6)') "Version 4  SIMD: ", t_end - t_start

  t_start = omp_get_wtime()
  do j = 1, nj - 1
    !$omp simd
    do i = 1, ni - 1
      buff(i,j) = (p(i,j) / (rho(i,j) * gamma_minus_one))
    end do
     !$omp end simd
  end do

  do j = 1, nj - 1
    !$omp simd
    do i = 1, ni - 1
      E(i,j) = buff(i,j) + 0.5_real64*(u(i,j) * u(i,j) + v(i,j) * v(i,j))
    end do
     !$omp end simd
  end do
  t_end = omp_get_wtime()
  if(this_image() == 1) write(*, '(a, es16.6)') "Version 5  SIMD: ", t_end - t_start
  
end program data_layout