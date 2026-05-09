cmake_minimum_required(VERSION 3.20)

project(KdTree LANGUAGES Fortran)
find_package(OpenMP REQUIRED)

add_compile_options(-O3)
set(CMAKE_INTERPROCEDURAL_OPTIMIZATION ON)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)
include(AddKdTest)

enable_testing()
add_subdirectory(src)
add_subdirectory(tests)

