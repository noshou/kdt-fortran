submodule(KdTreeFortran) KdTreeDBSCAN
    implicit none
    contains 

        module procedure DBSCAN
            type(KdNodePtr), allocatable :: nodes(:)
            integer                      :: j, idx, bs
            character(len=9)             :: m
            logical                      :: isInit
            
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

            allocate(nodes, source=this%getAllNodes())


        end procedure DBSCAN

end submodule KdTreeDBSCAN