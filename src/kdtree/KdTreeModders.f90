submodule(KdTreeFortran) KdTreeModders
    use iso_fortran_env, only: int64, real64
    implicit none
    contains

        module procedure setRebuildRatio
            if (ratio .le. 0.0_real64) then 
                error stop "setRebuildRatio: rebuildRatio must be greater than zero!"
            else if (ratio .ge. 1.0_real64) then 
                error stop "setRebuildRatio: rebuildRatio must be less than 1!"
            else 
                this%rebuildRatio = ratio
            end if
        end procedure setRebuildRatio

        !> rebuilds a tree after node pool has been modified. 
        !!
        !! must ensure that tree and node state 
        !! invariants are preserved BEFORE calling rebuild
        subroutine rebuild(t)
            type(KdTree), intent(inout) :: t
            integer(int64), allocatable :: indices(:)
            integer(int64)              :: i
            
            ! allocate indices; that way we don't have to modify the list of nodes
            allocate(indices(t%pop))
            indices = [(i, i=1_int64, t%pop)]
            t%rootIdx = 0_int64

            ! build tree
            call buildSubtree(t, t%rootIdx, 0_int64, indices, 1_int64, t%pop)

            ! set number of modifications to zero
            t%modifications = 0_int64

        end subroutine rebuild

        module procedure addNodes

            logical                 :: isInit, hasData, rootHasData, hasRoot
            integer(int64)          :: dataListSize, i, dim, pop, numNodeToAdd, tid, currIdx
            type(KdNode), pointer   :: nodePoolTmp(:)

            ! initialize state variables
            call this%associatedRoot(hasRoot)
            if (hasRoot) rootHasData = this%nodePool(this%rootIdx)%hasData
            call this%getInitState(isInit)
            hasData = present(dataList)
            numNodeToAdd = size(coordsList, 2)
            dim = size(coordsList, 1)

            ! assertion checks
            if (.not. isInit) then
                error stop "addNodes: tree uninitialized (call this%build() first?)"
            else if (dim .ne. this%dim) then
                error stop "addNodes: dimension of coordinates must match dimension of tree!"
            else if (hasData) then
                dataListSize = size(dataList)
                if (dataListSize .eq. 0_int64) then
                    error stop "addNodes: size of data list must be greater than zero!"
                else if (dataListSize .ne. numNodeToAdd) then
                    error stop "addNodes: number of data points must match number of coordinates!"
                else if (hasRoot) then
                    if (.not. rootHasData) then
                        error stop "addNodes: tree takes no data input!"
                    else if (.not. same_type_as(dataList(1), this%nodePool(this%rootIdx)%data)) then
                        error stop "addNodes: data mismatch between tree and dataList"
                    end if
                end if
            else
                if (hasRoot .and. rootHasData) then
                    error stop "addNodes: tree requires data input!"
                end if
            end if

            ! realloc nodePool — serialized: nodePool pointer, pop, 
            ! and currNodeId are all shared state
            !$OMP CRITICAL (tree_mutate)
            pop = numNodeToAdd + this%pop
            tid = this%getTreeId()
            allocate(nodePoolTmp(pop))
            nodePoolTmp(1:this%pop) = this%nodePool(1:this%pop)
            do i = this%pop + 1, pop
                allocate(nodePoolTmp(i)%coords(dim))
                !$OMP ATOMIC CAPTURE
                this%currNodeId       = this%currNodeId + 1_int64
                nodePoolTmp(i)%nodeId = this%currNodeId
                !$OMP END ATOMIC
                nodePoolTmp(i)%numRemovesSnapshot   = this%numRemoves
                nodePoolTmp(i)%coords(:)            =  coordsList(:, i-this%pop)
                nodePoolTmp(i)%hasData              =  hasData
                nodePoolTmp(i)%lch                  =  0_int64
                nodePoolTmp(i)%rch                  =  0_int64
                nodePoolTmp(i)%treeId               = tid
                if (nodePoolTmp(i)%hasData) then 
                    nodePoolTmp(i)%data = dataList(i-this%pop)
                end if
            end do
            deallocate(this%nodePool)
            this%nodePool => nodePoolTmp
            this%pop = pop
            !$OMP END CRITICAL (tree_mutate)

            ! rebuild decision and tree mutation 
            ! serialized: modifications, rootIdx, and lch/rch are shared state
            !$OMP CRITICAL (tree_mutate)
            if (this%modifications + numNodeToAdd                     &
                .gt.                                                  &
                this%rebuildRatio * (this%pop - numNodeToAdd)         &
            ) then
                call rebuild(this)
            
            else

                ! no need for rebuild; insert new nodes at leaves.
                ! rootIdx=0 (empty tree) can never reach this branch: the rebuild condition
                ! simplifies to numNodeToAdd > 0, which is always true when pop was zero.
                do i = this%pop-numNodeToAdd + 1, this%pop
                    currIdx = this%rootIdx
                    do

                        ! search left subtree
                        if (this%nodePool(i)%coords(this%nodePool(currIdx)%splitAxis)       & 
                            .le.                                                            &
                            this%nodePool(currIdx)%coords(this%nodePool(currIdx)%splitAxis) &
                        ) then
                            if (this%nodePool(currIdx)%lch .eq. 0_int64) then
                                this%nodePool(currIdx)%lch = i
                                this%nodePool(i)%splitAxis = saxs(this%nodePool(currIdx)%splitAxis, dim)
                                exit
                            else
                                currIdx = this%nodePool(currIdx)%lch
                            end if

                        ! search right subtree
                        else
                            if (this%nodePool(currIdx)%rch .eq. 0_int64) then
                                this%nodePool(currIdx)%rch = i
                                this%nodePool(i)%splitAxis = saxs(this%nodePool(currIdx)%splitAxis, dim)
                                exit
                            else
                                currIdx = this%nodePool(currIdx)%rch
                            end if
                        end if
                    end do
                end do

                this%modifications = this%modifications + numNodeToAdd
            end if
            !$OMP END CRITICAL (tree_mutate)

        end procedure addNodes

end submodule KdTreeModders
