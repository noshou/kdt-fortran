submodule(KdTree) TreeModder
    use iso_fortran_env, only: int64, real64
    implicit none
    contains

        module procedure setRebuildRatio
            if (ratio .le. 0.0_real64) error stop "setRebuildRatio: rebuildRatio must be greater than zero!"
            if (ratio .ge. 1.0_real64) error stop "setRebuildRatio: rebuildRatio must be lesser than or equal to 1!"
            this%rebuildRatio = ratio
        end procedure setRebuildRatio

        module procedure addNodes

            logical                 :: isInit, hasNodePool, hasData, rootHasData, hasRoot
            integer(int64)          :: dataListSize, i, dim, pop, numNodeToAdd, tid
            type(Node), allocatable :: nodePoolTmp(:)

            return  ! no-op: addNodes is under active development

            ! initialize state variables
            call this%associatedNodePool(hasNodePool)
            call this%associatedRoot(hasRoot)
            rootHasData = this%nodePool(this%rootIdx)%hasData
            call this%getInitState(isInit)
            hasData = present(dataList)
            dataListSize = size(dataList)
            numNodeToAdd = size(coordsList, 2)
            dim = size(coordsList, 1)

            ! assertion checks
            if (.not. isInit) then
                error stop "addNodes: tree uninitialized (call this%build() first?)"
            else if (dim .ne. this%dim) then
                error stop "addNodes: dimension of coordinates must match dimension of tree!"
            else if (hasData) then
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

            ! realloc nodePool
            ! nodes in nodePool[this%pop+1:pop] need to be assigned to tree
            ! note: nodes will be added to leaves, so lch/rch will be 0
            pop = numNodeToAdd + this%pop
            tid = this%getTreeId()
            allocate(nodePoolTmp(pop))
            nodePoolTmp(1:this%pop) = this%nodePool(1:this%pop)
            do i = this%pop + 1, pop
                allocate(nodePoolTmp(i)%coords(dim))
                nodePoolTmp(i)%coords(:)  =  coordsList(:, i-this%pop)
                nodePoolTmp(i)%hasData    =  hasData
                nodePoolTmp(i)%lch        =  0_int64
                nodePoolTmp(i)%rch        =  0_int64
                if (nodePoolTmp(i)%hasData) nodePoolTmp(i)%data = dataList(i-this%pop)
                nodePoolTmp(i)%treeId     = tid
            end do
            deallocate(this%nodePool)
            this%nodePool = nodePoolTmp

        end procedure addNodes

end submodule TreeModder
