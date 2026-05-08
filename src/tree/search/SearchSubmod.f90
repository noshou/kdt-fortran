submodule (KdTreeModule) SearchSubmod
    implicit none 
    
    ! interface for helper functions 
    interface 
        
        !=======================================================!
        !=================== RnnModule.f90  ====================!
        !=======================================================!

        !> Radius Nearest Neighbour search. Walks the kd-tree from curr,
        !! appending matching nodes to res and pruning subtrees whose
        !! splitting hyperplane lies further than radius from target.
        !! @param[in] target  the target node
        !! @param[in] curr    the root of the current subtree
        !! @param[in] res     the list of nodes within the search radius
        !! @param[in] arrSize the number of nodes found within the search radius
        module recursive subroutine rNN(target, curr, radius, res, arrSize)
            type(node), intent(in), pointer             :: target, curr
            real(kind=real64), intent(in)               :: radius
            integer, intent(inout)                      :: arrSize
            type(nodePtr), allocatable, intent(inout)   :: res(:)
        end subroutine rNN
        
        !=======================================================!

    
        end interface 
    
    contains 
        module procedure rNN_Node
            
            integer :: arrSize, is
            
            if (.not. associated(this%root)) error stop "rNN_Node: tree is empty (call build first?)"
            if (.not. associated(target))    error stop "rNN_Node: target is null"
            if (radius .lt. 0.0_real64)      error stop "rNN_Node: negative radius"
            if (.not. this%isMember(target)) error stop "rNN_Node: target is not a member of tree"

            if(.not. present(initialSize)) then 
                is = 1000
            else 
                is = initialSize 
            end if
            
            ! track array size for resizing later on
            arrSize = 0 
            allocate(res(is))

            call rNN(target, this%root, radius, res, arrSize)
            
            !... resizing later on
            if (arrSize .eq. 0) then  
                deallocate(res)
                allocate(res(0))
            else 
                res = res(1:arrSize)
            end if 

        end procedure rNN_Node


        module procedure rNN_Centroid

            integer            :: arrSize, is 
            type(node), target :: dummyNode

            if (.not. associated(this%root))    error stop "rNN_Centroid: tree is empty (call build first?)"
            if (size(centroid) .ne. this%dim)   error stop "rNN_Centroid: dimension of centroid must match dimension of tree"
            if (radius .lt. 0.0_real64)         error stop "rNN_Centroid: negative radius"

            if(.not. present(initialSize)) then 
                is = 1000
            else 
                is = initialSize 
            end if

            ! track array size for resizing later on
            arrSize = 0 
            allocate(res(is))

            ! create a dummy node
            allocate(dummyNode%coords(this%dim), source=centroid)
            
            call rNN(dummyNode, this%root, radius, res, arrSize)

            !... resizing later on
            if (arrSize .eq. 0) then  
                deallocate(res)
                allocate(res(0))
            else 
                res = res(1:arrSize)
            end if 

        end procedure rNN_Centroid

end submodule SearchSubmod

