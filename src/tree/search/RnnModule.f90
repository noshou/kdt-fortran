submodule(KdTree:SearchSubmod) RnnModule
    implicit none
    contains
        module procedure rNN
            real(kind=real64)          :: delta
            type(nodePtr), allocatable :: tmp(:)
            integer(int64)             :: axis
            integer                    :: i
            type(node), pointer        :: copy
            logical                    :: withinRadius

            if (associated(curr)) then

                select case (metric)
                    case ('euclidean')
                        withinRadius = target%euclideanDist(curr) .le. radius
                    case ('manhattan')
                        withinRadius = target%manhattanDist(curr) .le. radius
                    case ('chebyshev')
                        withinRadius = target%chebyshevDist(curr) .le. radius
                    case default
                        error stop "rNN: unknown metric"
                end select

                if (withinRadius) then
                    if (size(res) .eq. arrSize) then
                        allocate(tmp(2*size(res)))
                        do i = 1, arrSize
                            tmp(i)%p    => res(i)%p    
                            res(i)%p    => null()
                            tmp(i)%src_ => res(i)%src_ 
                            res(i)%src_ => null()
                        end do
                        call move_alloc(from=tmp, to=res)
                    end if
                    arrSize = arrSize + 1
                    allocate(copy, source=curr)
                    res(arrSize)%p    => copy
                    res(arrSize)%src_ => curr
                end if

                axis  = curr%splitAxis
                delta = target%coords(axis) - curr%coords(axis)
                if (delta < 0) then
                    call rNN(target, curr%leftChild,  radius, res, arrSize, metric)
                    if (-delta .le. radius) call rNN(target, curr%rightChild, radius, res, arrSize, metric)
                else
                    call rNN(target, curr%rightChild, radius, res, arrSize, metric)
                    if (delta .le. radius)  call rNN(target, curr%leftChild,  radius, res, arrSize, metric)
                end if
            end if
        end procedure rNN

end submodule RnnModule
