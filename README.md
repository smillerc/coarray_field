# A high level coarray field object to manage domain decomposition

* The goal of this project is to demonstrate how coarrays can be used to decompose a domain for physics-type simulations.
* This project starts off with the `Field` class from the book [Modern Fortran](https://github.com/modern-fortran/tsunami) by Milan Curcic
* Rudimentary field serialization is via the [`h5fortran`](https://github.com/geospace-code/h5fortran) library by Michael Hirsch
* Eventual OpenCL integration via the [`Focal`](https://github.com/LKedward/focal) library by Laurence Kedward 

## Build instructions
```bash
mkdir build
cd build
cmake .. -DCMAKE_BUILD_TYPE="Debug" \
    -DENABLE_COARRAY=ON \
    -DUSE_OPENMP_THREADS=NO \
    -DUSE_OPENMP_SIMD=YES \
    -DUSE_OPENCL=NO \
    -DENABLE_TESTING=YES
```

## Example Use [WIP]
```fortran
type(field_2d_t) :: rho, u, rho_u

rho = field_2d(name='rho', long_name='density', &
               descrip='Cell-centered mass density', &
               units='g/cc', global_dims=[20, 20], n_halo_cells=2)

u = field_2d(name='u', long_name='x-velocity', &
               descrip='Cell-centered x-velocity', &
               units='cm/s', global_dims=[20, 20], n_halo_cells=2)

! Create a new field based off of existing ones
rho_u = rho * u

! Get max
call rho_u%maxval()
```