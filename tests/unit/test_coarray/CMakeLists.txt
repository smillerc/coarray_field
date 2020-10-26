# add_executable(test_coarray simple.f90) target_link_libraries(test_coarray ${Coarray_LIBRARIES})
# add_caf_test(test_coarray 4 test_coarray)

add_caf_test(test_coarray N_IMAGES 4 SOURCES simple.f90)

add_caf_fypp_test(
  test_caf
  SOURCES
  test_caf.fypp
  LINK_LIBRARIES
  ${Coarray_LIBRARIES}
  INCLUDE_DIR "../include"
  N_IMAGES
  4)
