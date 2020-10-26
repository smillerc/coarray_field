cmake_minimum_required(VERSION 3.8)

project(
  field
  VERSION 0.1.0
  LANGUAGES Fortran C)

set(CMAKE_VERBOSE_MAKEFILE OFF)
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

cmake_policy(SET CMP0074 NEW)
cmake_policy(SET CMP0069 NEW) # Link-time optimization
# include(CheckIPOSupported) check_ipo_supported()

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)

# set output paths for modules, archives, and executables
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/include)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

find_package(Python)

option(USE_OPENMP_THREADS "Enable OpenMP parallelization" OFF)
option(USE_OPENMP_SIMD "Enable OpenMP SIMD vectorization" OFF)
if(USE_OPENMP_THREADS OR USE_OPENMP_SIMD)
  find_package(OpenMP REQUIRED Fortran)
endif()

option(USE_OPENCL "Enable OpenCL to run portions on GPUs" OFF)
if(USE_OPENCL)
  find_package(OpenCL)
endif()

option(ENABLE_COARRAY "Build with coarrays enabled" OFF)
option(ENABLE_COARRAY_SINGLE "Build with coarrays but only for a single image (useful for debugging)" OFF)
option(BUILD_FOR_SHARED_MEMORY "Build for shared memory only (single node)" OFF)
option(BUILD_FOR_DISTRIBUTED_MEMORY "Build for distributed memory (multiple nodes)" OFF)
option(OUTPUT_OPTIMIZATION_REPORTS "Turn on optimization reports" OFF)
option(USE_ASAN "Enable GCC's address sanitizer to check for leaks" OFF)
option(USE_TSAN "Enable GCC's thread sanitizer to check for race conditions" OFF)
option(ENABLE_TESTING "Enable unit testing" OFF)
option(ENABLE_PROFILING "Enable profile flags" OFF)

include(ProcessorCount)
processorcount(NUM_IMAGES)

if(ENABLE_COARRAY OR ENABLE_COARRAY_SINGLE OR BUILD_FOR_DISTRIBUTED_MEMORY)
  find_package(Coarray REQUIRED)
endif()

find_package(HDF5 COMPONENTS Fortran HL REQUIRED)

# Set compiler flags
include(SetFortranFlags)

if(CMAKE_BUILD_TYPE STREQUAL "RELEASE")
  message(STATUS "Compile Flags: ${CMAKE_Fortran_FLAGS} ${CMAKE_Fortran_FLAGS_RELEASE} ")
elseif(CMAKE_BUILD_TYPE STREQUAL "DEBUG")
  message(STATUS "Compile Flags: ${CMAKE_Fortran_FLAGS} ${CMAKE_Fortran_FLAGS_DEBUG}")
endif()

cmake_host_system_information(RESULT HOST_NAME QUERY HOSTNAME)
cmake_host_system_information(RESULT N_PHYSICAL_CORES QUERY NUMBER_OF_PHYSICAL_CORES)
cmake_host_system_information(RESULT N_LOGICAL_CORES QUERY NUMBER_OF_LOGICAL_CORES)

# Insert the git version information into the version.h header file See https://goo.gl/697j8v (short
# stackoverflow link)
include(GetGitRevisionDescription)
get_git_head_revision(GIT_REFSPEC GIT_SHA1)
git_local_changes(GIT_LOCAL_CHANGES)

# Include build info in the binary
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/include/version.h.in"
               "${CMAKE_CURRENT_BINARY_DIR}/generated/version.h" @ONLY)

# Include the newly generated version.h file
include_directories(${CMAKE_BINARY_DIR}/generated)

add_subdirectory(src)

if(ENABLE_TESTING)
  # find_package(PFUNIT REQUIRED)
  include(AddCoarrayCTest)
  enable_testing()
  add_subdirectory(tests)
endif()
