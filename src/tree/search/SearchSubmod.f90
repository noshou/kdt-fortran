submodule (KdTree) SearchSubmod
    implicit none 
    
    ! interface for helper functions 
    interface 
        
        !=======================================================!
        !=================== RnnModule.f90  ====================!
        !=======================================================!

        !> Radius Nearest Neighbour search. Walks the kd-tree from curr,
        !! appending matching nodes to res and pruning subtrees whose
        !! splitting hyperplane lies further than radius from target.
        !! @param[in]    target  the query node (used as the search centre)
        !! @param[in]    curr    root of the current subtree (null terminates recursion)
        !! @param[in]    radius  search radius
        !! @param[inout] res     result buffer; doubles in size when full
        !! @param[inout] arrSize number of results written into res so far
        !! @param[in]    metric  'euclidean', 'manhattan', 'chebyshev'
        module recursive subroutine rNN(target, curr, radius, res, arrSize, metric)
            type(node), intent(in), pointer             :: target, curr
            real(kind=real64), intent(in)               :: radius
            integer, intent(inout)                      :: arrSize
            type(nodePtr), allocatable, intent(inout)   :: res(:)
            character(len=*), intent(in)                :: metric
        end subroutine rNN
        
        !=======================================================!

    
        end interface 
    
    contains 
        module procedure rNN_Node
            
            integer          :: arrSize, is, i
            character(len=9) :: m

            if (.not. associated(this%root)) error stop "rNN_Node: tree is empty (call build first?)"
            if (.not. associated(target))    error stop "rNN_Node: target is null"
            if (radius .lt. 0.0_real64)      error stop "rNN_Node: negative radius"
            if (.not. this%isMember(target)) error stop "rNN_Node: target is not a member of tree"

            if(.not. present(bufferSize)) then
                is = 1000
            else
                if (bufferSize .le. 0) error stop "rNN_Node: invalid bufferSize"
                is = bufferSize
            end if
            
            if (.not. present(metric)) then 
                m = 'euclidean'
            else  
                select case (metric)
                case ('euclidean')
                    m = 'euclidean'
                case ('manhattan')
                    m = 'manhattan'
                case ('chebyshev')
                    m = 'chebyshev'
                case default
                    error stop "rNN_Node: unknown metric"
                end select
            end if

            ! track array size for resizing later on
            arrSize = 0 
            
            allocate(res(is))

            call rNN(target, this%root, radius, res, arrSize, m)
            
            ! trim res to the number of results actually found
            if (arrSize .eq. 0) then  
                deallocate(res)
                allocate(res(0))
            else 
                res = res(1:arrSize)
            end if 

            ! remove target node from list of found nodes
            if (present(excludeTarget) .and. excludeTarget) then
                do i = 1, arrSize
                    if (associated(res(i)%p, target)) then
                        res(i:arrSize-1) = res(i+1:arrSize)
                        arrSize = arrSize - 1
                        exit
                    end if
                end do
                if (arrSize .eq. 0) then
                    deallocate(res)
                    allocate(res(0))
                else
                    res = res(1:arrSize)
                end if
            end if

        end procedure rNN_Node


        module procedure rNN_Centroid

            integer            :: arrSize, is
            character(len=9)   :: m
            type(node), target :: dummyNode

            if (.not. associated(this%root))    error stop "rNN_Centroid: tree is empty (call build first?)"
            if (size(centroid) .ne. this%dim)   error stop "rNN_Centroid: dimension of centroid must match dimension of tree"
            if (radius .lt. 0.0_real64)         error stop "rNN_Centroid: negative radius"

            if(.not. present(bufferSize)) then
                is = 1000
            else
                if (bufferSize .le. 0) error stop "rNN_Centroid: invalid bufferSize"
                is = bufferSize
            end if

            if (.not. present(metric)) then
                m = 'euclidean'
            else
                select case (metric)
                case ('euclidean')
                    m = 'euclidean'
                case ('manhattan')
                    m = 'manhattan'
                case ('chebyshev')
                    m = 'chebyshev'
                case default
                    error stop "rNN_Centroid: unknown metric"
                end select
            end if

            ! track array size for resizing later on
            arrSize = 0 
            allocate(res(is))

            ! wrap centroid in a node so rNN can call distance methods uniformly
            allocate(dummyNode%coords(this%dim), source=centroid)
            
            call rNN(dummyNode, this%root, radius, res, arrSize, m)

            ! trim res to the number of results actually found
            if (arrSize .eq. 0) then  
                deallocate(res)
                allocate(res(0))
            else 
                res = res(1:arrSize)
            end if 

        end procedure rNN_Centroid

end submodule SearchSubmod

