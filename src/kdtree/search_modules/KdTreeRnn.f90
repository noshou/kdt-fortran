submodule(KdTreeFortran) KdTreeRnn
    implicit none
    contains

        module procedure rNN
            integer(int64), allocatable  :: stack(:), stmp(:)
            integer(int64)               :: stackTop, stackSize, node
            real(kind=real64)            :: delta
            type(KdNodePtr), allocatable :: tmp(:)
            integer(int64)               :: axis
            integer                      :: i
            type(KdNode), pointer        :: copy
            logical                      :: withinRadius

            if (currIdx .eq. 0_int64) return

            stackSize = 64_int64
            allocate(stack(stackSize))
            stackTop  = 1_int64
            stack(1)  = currIdx

            do while (stackTop > 0_int64)
                node     = stack(stackTop)
                stackTop = stackTop - 1_int64

                select case (metric)
                    case ('euclidean')
                        withinRadius = target%euclideanDist(nodePool(node)) .le. radius
                    case ('manhattan')
                        withinRadius = target%manhattanDist(nodePool(node)) .le. radius
                    case ('chebyshev')
                        withinRadius = target%chebyshevDist(nodePool(node)) .le. radius
                    case default
                        error stop "rNN: unknown metric"
                end select

                if (withinRadius) then
                    if (size(res) .eq. arrSize) then
                        allocate(tmp(2*size(res)))
                        do i = 1, arrSize
                            tmp(i)%p => res(i)%p
                            res(i)%p => null()
                        end do
                        call move_alloc(from=tmp, to=res)
                    end if
                    arrSize = arrSize + 1
                    allocate(copy, source=nodePool(node))
                    res(arrSize)%p => copy
                end if

                axis  = nodePool(node)%splitAxis
                delta = target%coords(axis) - nodePool(node)%coords(axis)

                ! grow stack if needed before pushing up to 2 children
                if (stackTop + 2 > stackSize) then
                    stackSize = stackSize * 2_int64
                    allocate(stmp(stackSize))
                    stmp(1:stackTop) = stack(1:stackTop)
                    call move_alloc(from=stmp, to=stack)
                end if

                if (delta < 0.0_real64) then
                    if (nodePool(node)%lch .ne. 0_int64) then
                        stackTop         = stackTop + 1_int64
                        stack(stackTop)  = nodePool(node)%lch
                    end if
                    if (-delta .le. radius .and. nodePool(node)%rch .ne. 0_int64) then
                        stackTop         = stackTop + 1_int64
                        stack(stackTop)  = nodePool(node)%rch
                    end if
                else
                    if (nodePool(node)%rch .ne. 0_int64) then
                        stackTop         = stackTop + 1_int64
                        stack(stackTop)  = nodePool(node)%rch
                    end if
                    if (delta .le. radius .and. nodePool(node)%lch .ne. 0_int64) then
                        stackTop         = stackTop + 1_int64
                        stack(stackTop)  = nodePool(node)%lch
                    end if
                end if
            end do

        end procedure rNN

        module procedure rNN_Node

            integer                    :: arrSize, is, i, j
            character(len=9)           :: m
            type(KdNodePtr), allocatable :: tmp(:)

            if (this%rootIdx .eq. 0_int64) then
                error stop "rNN_Node: tree is empty (call build first?)"
            else if (.not. associated(target%p)) then
                error stop "rNN_Node: target is null"
            else if (radius .lt. 0.0_real64) then
                error stop "rNN_Node: negative radius"
            else if (.not. this%isMember(target%p)) then
                error stop "rNN_Node: target is not a member of tree"
            end if

            if(.not. present(bufferSize)) then
                is = 1000
            else
                if (bufferSize .le. 0) then
                    error stop "rNN_Node: invalid bufferSize"
                else
                    is = bufferSize
                end if
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

            arrSize = 0
            allocate(res(is))

            call rNN(target%p, this%rootIdx, this%nodePool, radius, res, arrSize, m)

            ! trim to actual result count
            if (arrSize .eq. 0) then
                deallocate(res)
                allocate(res(0))
            else
                allocate(tmp(arrSize))
                do i = 1, arrSize
                    tmp(i)%p => res(i)%p
                    res(i)%p => null()
                end do
                call move_alloc(from=tmp, to=res)
            end if

            ! remove target node from list of found nodes
            if (present(excludeTarget) .and. excludeTarget) then
                do i = 1, arrSize
                    if (res(i)%p%nodeId .eq. target%p%nodeId) then
                        call res(i)%destroy()
                        do j = i, arrSize - 1
                            res(j)%p   => res(j+1)%p
                            res(j+1)%p => null()
                        end do
                        arrSize = arrSize - 1
                        exit
                    end if
                end do
                if (arrSize .eq. 0) then
                    deallocate(res)
                    allocate(res(0))
                else
                    allocate(tmp(arrSize))
                    do i = 1, arrSize
                        tmp(i)%p => res(i)%p ; res(i)%p => null()
                    end do
                    call move_alloc(from=tmp, to=res)
                end if
            end if

            ! stamp each dispatched copy with the current removal counter so
            ! the fast path in isMember fires on subsequent calls
            do i = 1, size(res)
                if (associated(res(i)%p)) res(i)%p%numRemovesSnapshot = this%numRemoves
            end do

        end procedure rNN_Node

        module procedure rNN_Centroid

            integer                      :: arrSize, is, i
            character(len=9)             :: m
            type(KdNode)                 :: dummyNode
            type(KdNodePtr), allocatable :: tmp(:)

            if (this%rootIdx .eq. 0_int64) then
                error stop "rNN_Centroid: tree is empty (call build first?)"
            else if (size(centroid) .ne. this%dim) then
                error stop "rNN_Centroid: dimension of centroid must match dimension of tree"
            else if (radius .lt. 0.0_real64) then
                error stop "rNN_Centroid: negative radius"
            end if

            if(.not. present(bufferSize)) then
                is = 1000
            else
                if (bufferSize .le. 0) then
                    error stop "rNN_Centroid: invalid bufferSize"
                else
                    is = bufferSize
                end if
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

            arrSize = 0
            allocate(res(is))

            ! wrap centroid in a node so rNN can call distance methods uniformly
            allocate(dummyNode%coords(this%dim), source=centroid)

            call rNN(           &
                dummyNode,      &
                this%rootIdx,   &
                this%nodePool,  &
                radius,         &
                res,            &
                arrSize,        &
                m               &
            )

            ! trim to actual result count
            if (arrSize .eq. 0) then
                deallocate(res)
                allocate(res(0))
            else
                allocate(tmp(arrSize))
                do i = 1, arrSize
                    tmp(i)%p => res(i)%p
                    res(i)%p => null()
                end do
                call move_alloc(from=tmp, to=res)
            end if

            ! stamp each dispatched copy with the current removal counter
            do i = 1, size(res)
                if (associated(res(i)%p)) then
                    res(i)%p%numRemovesSnapshot = this%numRemoves
                end if
            end do

        end procedure rNN_Centroid

        subroutine assert_rNN(t, coords, epsilon, metric, name, bufferSize, ids)
            type(KdTree),      intent(in)           :: t
            real(real64),      intent(in)           :: coords(:,:)
            character(len=*),  intent(in)           :: name
            character(len=*),  intent(in), optional :: metric
            real(real64),      intent(in), optional :: epsilon
            integer(int64),    intent(in), optional :: ids(:)
            integer,           intent(in), optional :: bufferSize

            if (.not. t%initialized) then
                write(*, '(*(A))') name, ': tree is not initialized (call build first)'
                stop 1
            end if
            if (t%dim .ne. size(coords, 1)) then
                write(*, '(*(A))') name, ': dimension mismatch!'
                stop 1
            end if
            if (present(epsilon)) then
                if (epsilon .lt. 0.0_real64) then
                    write(*, '(*(A))') name, ': epsilon must be >= 0!'
                    stop 1
                end if
            end if
            if (present(ids)) then
                if (size(ids) .ne. size(coords, 2)) then
                    write(*, '(*(A))') name, ': number of ids must match number of coordinates'
                    stop 1
                end if
            end if
            if (present(metric)) then
                select case (metric)
                    case ('euclidean')
                    case ('manhattan')
                    case ('chebyshev')
                    case default
                        write(*, '(*(A))') name, ": unknown metric: '", metric, "'"
                        stop 1
                end select
            end if
            if (present(bufferSize)) then
                if (bufferSize .le. 0) then
                    write(*, '(*(A))') name, ': invalid buffer size!'
                    stop 1
                end if
            end if
        end subroutine assert_rNN

        module procedure rNN_Coords
            integer                      :: i, bs
            real(real64)                 :: e
            type(KdNodePtr), allocatable :: nptrs(:)

            call assert_rNN(   &
                this,               &
                coords,             &
                epsilon,            &
                metric,             &
                'rNN_Coords', &
                bufferSize          &
            )

            if (present(epsilon)) then
                e = epsilon
            else
                e = 1e-15_real64
            end if

            if (present(bufferSize)) then
                bs = bufferSize
            else
                bs = 1000
            end if

            allocate(res(size(coords, 2)))

            ! empty tree: return empty buckets without descending
            if (this%pop .eq. 0_int64) then
                do i = 1, size(coords, 2)
                    allocate(res(i)%nodes(0))
                end do
                return
            end if

            do i = 1, size(coords, 2)
                nptrs = this%rNN_Centroid(  &
                    coords(:,i),            &
                    e,                      &
                    bs,                     &
                    metric                  &
                )
                allocate(res(i)%nodes, source=nptrs)
            end do
        end procedure rNN_Coords

        module procedure rNN_Ids
            integer                      :: i, j, bs, currSize
            real(real64)                 :: e
            type(KdNodePtr), allocatable :: nptrsTmp(:)

            call assert_rNN(   &
                this,               &
                coords,             &
                epsilon,            &
                metric,             &
                'rNN_Ids',    &
                bufferSize,         &
                ids                 &
            )

            if (present(epsilon)) then
                e = epsilon
            else
                e = 1e-15_real64
            end if

            if (present(bufferSize)) then
                bs = bufferSize
            else
                bs = 1000
            end if

            res = this%rNN_Coords(coords, metric, e, bs)

            ! filter res by id
            do i = 1, size(coords, 2)
                currSize = 0
                allocate(nptrsTmp(size(res(i)%nodes)))
                do j = 1, size(res(i)%nodes)
                    if (res(i)%nodes(j)%p%nodeId == ids(i)) then
                        currSize = currSize + 1
                        nptrsTmp(currSize) = res(i)%nodes(j)
                    end if
                end do
                deallocate(res(i)%nodes)
                allocate(res(i)%nodes, source=nptrsTmp(1:currSize))
                deallocate(nptrsTmp)
            end do

        end procedure rNN_Ids

        module procedure rNN_Rad
            
            integer                      :: i, bs
            type(KdNodePtr), allocatable :: nptrs(:)

            ! assertion checks
            call assert_rNN(this, coords=coords, metric=metric, name='rNN_Rad', bufferSize=bufferSize)
            if (size(radii) .ne. size(coords, 2)) then 
                error stop "rNN_Rad: number of radii must match number of coordinates"
            end if

            if (present(bufferSize)) then
                bs = bufferSize
            else
                bs = 1000
            end if

            allocate(res(size(coords, 2)))
            if (this%pop .eq. 0_int64) then 
                do i = 1, size(coords, 2)
                    allocate(res(i)%nodes(0))
                end do
                return
            end if
            
            do i = 1, size(coords, 2)
                nptrs = this%rNN_Centroid(  &
                    coords(:,i),            &
                    radii(i),               &
                    bs,                     &
                    metric                  &
                )
                allocate(res(i)%nodes, source=nptrs)
            end do
        end procedure rNN_Rad

        module procedure rNN_RadIds
            integer                      :: i, j, k, nMatch
            type(KdNodeBucket), allocatable :: resTmp(:)
            type(KdNodePtr),    allocatable :: nptrTmp(:)
            ! assertion checks
            call assert_rNN(          &
                this,                 &
                coords=coords,        &
                metric=metric,        &
                name='rNN_RadIds',    &
                bufferSize=bufferSize &
            )

            if (size(radii) .ne. size(coords, 2)) then
                error stop "rNN_RadIds: number of radii must match number of coordinates"
            else if (size(ids) .eq. 0) then
                error stop "rNN_RadIds: ids must not be empty"
            end if
            
            ! spatial query: find all nodes within radii(i) of coords(:,i)
            resTmp = this%rNN_Rad(coords, radii, metric, bufferSize)
            allocate(res(size(resTmp)))
            do i = 1, size(resTmp)
                
                ! filter bucket i: keep only nodes whose id appears in ids
                allocate(nptrTmp(size(resTmp(i)%nodes)))
                nMatch = 0
                do j = 1, size(resTmp(i)%nodes)
                    do k = 1, size(ids)
                        if (resTmp(i)%nodes(j)%p%nodeId .eq. ids(k)) then
                            nMatch = nMatch + 1
                            nptrTmp(nMatch) = resTmp(i)%nodes(j)
                            exit
                        end if
                    end do
                end do
                
                ! pack matched nodes into result bucket; empty if none matched
                allocate(res(i)%nodes(nMatch))
                if (nMatch .gt. 0) then 
                    res(i)%nodes = nptrTmp(1:nMatch)
                end if
                deallocate(nptrTmp)
            end do
        end procedure rNN_RadIds
end submodule KdTreeRnn
