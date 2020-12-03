include_directories(${HDF5_INCLUDE_DIRS} ${HDF5_Fortran_INCLUDE_DIRS})
add_library(h5fortran
  third_party/h5fortran/src/interface.f90
  third_party/h5fortran/src/attributes.f90
  third_party/h5fortran/src/read.f90
  third_party/h5fortran/src/write.f90
  third_party/h5fortran/src/pathlib.f90
  third_party/h5fortran/src/reader.f90
  third_party/h5fortran/src/reader_lt.f90
  third_party/h5fortran/src/writer.f90
  third_party/h5fortran/src/writer_lt.f90)

target_include_directories(h5fortran PUBLIC ${HDF5_INCLUDE_DIRS})
target_link_libraries(h5fortran ${HDF5_LIBRARIES} ${HDF5_HL_LIBRARIES})

add_library(field
  mod_parallel.f90 
  globals.f90 
  error.f90
  field_2d/field_2d.f90 
  field_2d/field_2d_cpu_operators.f90 
  field_2d/field_2d_gpu_operators.f90)

target_link_libraries(field PUBLIC h5fortran ${Coarray_LIBRARIES})
