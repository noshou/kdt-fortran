add_library(kdtree
	KdTree.f90
	node/NodeDistance.f90
	node/NodeGetters.f90
	node/NodeUtils.f90
	tree/BuildSubmod.f90
	tree/TreeGetters.f90
	tree/TreeUtils.f90
	tree/search/SearchSubmod.f90
	tree/search/RnnModule.f90
)

target_include_directories(kdtree PUBLIC
	$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
)

target_link_libraries(kdtree PRIVATE OpenMP::OpenMP_Fortran)