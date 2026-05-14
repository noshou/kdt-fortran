submodule(KdTreeFortran:SearchSubmod) RnnModule
    implicit none
    contains
        module procedure rNN
            real(kind=real64)            :: delta
            type(KdNodePtr), allocatable :: tmp(:)
            integer(int64)               :: axis
            integer                      :: i
            type(KdNode), pointer        :: copy
            logical                      :: withinRadius

            if (currIdx .ne. 0_int64) then

                select case (metric)
                    case ('euclidean')
                        withinRadius = target%euclideanDist(nodePool(currIdx)) .le. radius
                    case ('manhattan')
                        withinRadius = target%manhattanDist(nodePool(currIdx)) .le. radius
                    case ('chebyshev')
                        withinRadius = target%chebyshevDist(nodePool(currIdx)) .le. radius
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
                    allocate(copy, source=nodePool(currIdx))
                    res(arrSize)%p => copy
                end if

                axis  = nodePool(currIdx)%splitAxis
                delta = target%coords(axis) - nodePool(currIdx)%coords(axis)
                if (delta < 0) then
                    call rNN(target, nodePool(currIdx)%lch, nodePool, radius, res, arrSize, metric)
                    if (-delta .le. radius) then 
                        call rNN(target, nodePool(currIdx)%rch, nodePool, radius, res, arrSize, metric)
                    end if
                else
                    call rNN(target, nodePool(currIdx)%rch, nodePool, radius, res, arrSize, metric)
                    if (delta .le. radius) then   
                        call rNN(target, nodePool(currIdx)%lch, nodePool, radius, res, arrSize, metric)
                    end if
                end if
            end if
        end procedure rNN

end submodule RnnModule
