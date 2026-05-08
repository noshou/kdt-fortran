add_executable(testv020_EMPTY_TREE_RNN_CENTROID Testv020_EMPTY_TREE_RNN_CENTROID.f90)                
target_link_libraries(testv020_EMPTY_TREE_RNN_CENTROID PRIVATE kdtree)                                      
add_test(NAME testv020_EMPTY_TREE_RNN_CENTROID COMMAND testv020_EMPTY_TREE_RNN_CENTROID)
set_tests_properties(testv020_EMPTY_TREE_RNN_CENTROID PROPERTIES WILL_FAIL TRUE)

add_executable(testv020_EMPTY_TREE_RNN_NODE Testv020_EMPTY_TREE_RNN_NODE.f90)                
target_link_libraries(testv020_EMPTY_TREE_RNN_NODE PRIVATE kdtree)                                      
add_test(NAME testv020_EMPTY_TREE_RNN_NODE COMMAND testv020_EMPTY_TREE_RNN_NODE)
set_tests_properties(testv020_EMPTY_TREE_RNN_NODE PROPERTIES WILL_FAIL TRUE)
