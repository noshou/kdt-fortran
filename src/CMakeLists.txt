add_library(kdtree
	KdTree.f90
	node/NodeDistance/NodeDistance.f90
	node/NodeGetters/NodeGetters.f90
	node/NodeUtils/NodeUtils.f90
	tree/BuildSubmod/BuildSubmod.f90
	tree/TreeGetters/TreeGetters.f90
	tree/TreeUtils/TreeUtils.f90
	tree/SearchSubmod/SearchSubmod.f90
	tree/SearchSubmod/RnnModule.f90
	tree/TreeModder/TreeModder.f90
)

target_include_directories(kdtree PUBLIC
	$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
	$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

target_link_libraries(kdtree PRIVATE OpenMP::OpenMP_Fortran)