add_library(kdtree
	node/KdNodeModule.f90
	tree/KdTreeModule.f90
	tree/HelpersSubmod.f90
	tree/BuildSubmod.f90
	tree/search/SearchSubmod.f90
	tree/search/RnnModule.f90
	KdTree.f90
)

target_include_directories(kdtree PUBLIC
	$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
)

target_link_libraries(kdtree PRIVATE OpenMP::OpenMP_Fortran)