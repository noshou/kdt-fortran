submodule(KdTreeFortran) KdTreeDBSCAN
    implicit none
    contains 

        module procedure DBSCAN
            type(KdNodePtr), allocatable :: nodes(:), neighbourhood(:)
            integer                      :: bs
            integer(int64)               :: i, j, clusterIdx = 0_int64
            character(len=9)             :: m
            logical                      :: isInit
            logical,         allocatable :: visited(:) 
            integer(int64),  allocatable :: cluster(:)  
            integer(int64),  parameter   :: UNASSIGNED = -1_int64, NOISE = 0_int64  

            ! scaffolded 
            return

            ! assertion checks + initializations
            call this%getInitState(isInit)
            if (.not. isInit) then 
                error stop "DBSCAN: tree is uninitialized (call build first?)"
            else if (minPts .lt. 0) then 
                error stop "DBSCAN: invalid minimum points"
            else if (radius .lt. 0_real64) then 
                error stop "DBSCAN: invalid radius"
            end if

            if (present(metric)) then 
                select case (metric)
                    case ('euclidean'); m = 'euclidean'
                    case ('manhattan'); m = 'manhattan'
                    case ('chebyshev'); m = 'chebyshev'
                    case default;       error stop "DBSCAN: unknown metric"
                end select 
            else
                m = DEFAULT_METRIC
            end if

            if (present(bufferSize)) then
                if (bufferSize .le. 0) then
                    error stop "DBSCAN: invalid bufferSize"
                end if
            else
                bs = DEFAULT_BUFFER_SIZE
            end if

            ! tree is empty; return "empty" result
            if (this%pop .eq. 0) then 
                allocate(res(2))
                allocate(res(1)%nodes(0))
                allocate(res(2)%nodes(0))
            else 
                
                ! parallel boolean array of visited nodes
                ! visited(i) -> nodePool(i) = true iff node visited
                allocate(visited(this%pop))
                visited = .false.

                ! parallel integer array of node assignments
                ! cluster(i) -> nodePool(i) = cluster ID
                allocate(cluster(this%pop))
                cluster = UNASSIGNED

                ! get list of nodes to iterate thru
                allocate(nodes, source=this%getAllNodes())

                ! iterate over all nodes
                do i = 1_int64, this%pop
                    
                    ! if we haven't visited, get neighbourhood
                    if (.not. visited(i)) then 
                        neighbourhood = this%rNN_Node( &
                            nodes(i),               &
                            radius,                 &
                            bs,                     &
                            m,                      &
                            .true.                  &
                        )

                        ! set this node to visited
                        visited(i) = .true.

                        ! if size < minPts, then tentatively add to "noise"
                        ! else, explore
                        if (size(neighbourhood) .lt. minPts) then 
                            cluster(i) = NOISE
                        else 
                            ! this node must belong to a new cluster
                            clusterIdx = clusterIdx + 1_int64
                            cluster(i) = clusterIdx
                        end if

                        ! since j tracks position in neighbourhood (1->#neighbours),
                        ! we need to make a shadow variable "iPrime" 

                    end if
                end do

            end if
        end procedure DBSCAN

end submodule KdTreeDBSCAN