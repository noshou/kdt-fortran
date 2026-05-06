submodule (KdTreeModule) SearchSubmod
    implicit none 
    contains 
        
    module procedure radialSearch
        
        integer :: arrSize, is
        
        if (.not. associated(this%root)) error stop "radialSearch: tree is empty (call build first?)"
        if (.not. associated(target))    error stop "radialSearch: target is null"
        if (radius .lt. 0.0_real64)      error stop "radialSearch: negative radius"
        if (.not. this%isMember(target)) error stop "radialSearch: target is not a member of tree"

        if(.not. present(initialSize)) then 
            is = 1000
        else 
            is = initialSize 
        end if

        arrSize = 0 
        allocate(res(is))
        call radialSearchAux(target, this%root, radius, res, arrSize)
        if (arrSize .eq. 0) then  
            deallocate(res)
            allocate(res(0))
        else 
            res = res(1:arrSize)
        end if 

    end procedure radialSearch


    !> Recursive helper for radialSearch. Walks the kd-tree from curr,
    !! appending matching nodes to res and pruning subtrees whose
    !! splitting hyperplane lies further than radius from target.
    !! @param[in] target  the target node
    !! @param[in] curr    the root of the current subtree
    !! @param[in] res     the list of nodes within the search radius
    !! @param[in] arrSize the number of nodes found within the search radius
    recursive subroutine radialSearchAux(target, curr, radius, res, arrSize)
        type(node), intent(in), pointer             :: target, curr
        real(kind=real64), intent(in)               :: radius
        real(kind=real64)                           :: delta
        integer, intent(inout)                      :: arrSize
        type(nodePtr), allocatable, intent(inout)   :: res(:)
        type(nodePtr), allocatable                  :: tmp(:)
        integer                                     :: axis
        
        if (associated(curr)) then 
            if (target%eucDist(curr) .le. radius) then 
                arrSize = arrSize + 1
                if (size(res) .eq. arrSize) then 
                    allocate(tmp(2*size(res)))
                    tmp(1:size(res)) = res
                    call move_alloc(from=tmp, to=res)
                end if 
                res(arrSize)%p => curr
            end if
            axis  = curr%splitAxis
            delta = target%coords(axis) - curr%coords(axis)
            if (delta < 0) then 
                call radialSearchAux(target, curr%leftChild, radius, res, arrSize)
                if (abs(delta) .le. radius) call radialSearchAux(target, curr%rightChild, radius, res, arrSize)
            else 
                call radialSearchAux(target, curr%rightChild, radius, res, arrSize)
                if (abs(delta) .le. radius) call radialSearchAux(target, curr%leftChild, radius, res, arrSize)
            end if
        end if 
    end subroutine radialSearchAux

end submodule SearchSubmod