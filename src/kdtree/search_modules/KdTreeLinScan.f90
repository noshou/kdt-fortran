submodule (KdTreeFortran) KdTreeLinScan
    implicit none
    contains
        module procedure linScan
            type(KdNode), pointer :: copy
            integer(int64)        :: i, j, numFound

            if (.not. this%initialized) then
                error stop "linScan: tree is not initialized (call build first)"
            end if
            if (size(ids) .eq. 0 .or. this%pop .eq. 0_int64) then
                allocate(res(0))
                return
            end if

            ! Count exact matches to determine final array size
            numFound = 0_int64
            do i = 1_int64, this%pop
                do j = 1_int64, size(ids)
                    if (this%nodePool(i)%nodeId == ids(j)) then
                        numFound = numFound + 1_int64
                        exit
                    end if
                end do
            end do

            allocate(res(numFound))

            ! Populate result, stamping each copy with current removal counter
            numFound = 0_int64
            do i = 1_int64, this%pop
                do j = 1_int64, size(ids)
                    if (this%nodePool(i)%nodeId == ids(j)) then
                        numFound = numFound + 1_int64
                        allocate(copy, source=this%nodePool(i))
                        copy%numRemovesSnapshot = this%numRemoves
                        res(numFound)%p => copy
                        exit
                    end if
                end do
            end do
        end procedure linScan

end submodule KdTreeLinScan
