add_library(kdtreefortran
	KdTreeFortran.f90
	kdnode/NodeDistance.f90
	kdnode/NodeGetters.f90
	kdnode/NodeUtils.f90
	kdtree/BuildSubmod.f90
	kdtree/TreeGetters.f90
	kdtree/TreeUtils.f90
	kdtree/SearchSubmod.f90
	kdtree/RnnModule.f90
	kdtree/TreeModder.f90
)

target_include_directories(kdtreefortran PUBLIC
	$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
	$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

target_link_libraries(kdtreefortran PRIVATE OpenMP::OpenMP_Fortran)