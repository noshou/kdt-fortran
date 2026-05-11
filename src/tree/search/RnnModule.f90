submodule(KdTree:SearchSubmod) RnnModule
    implicit none 
    contains 
        module procedure rNN
            real(kind=real64)           :: delta
            type(nodePtr), allocatable  :: tmp(:)
            integer(int64)              :: axis
            
            if (associated(curr)) then 
                
                ! choose metric
                select case (metric)
                    
                    case ('euclidean')
                        if (target%euclideanDist(curr) .le. radius) then 
                            if (size(res) .eq. arrSize) then 
                                allocate(tmp(2*size(res)))
                                tmp(1:size(res)) = res
                                call move_alloc(from=tmp, to=res)
                            end if 
                            arrSize = arrSize + 1
                            res(arrSize)%p => curr
                        end if
                    
                    case ('manhattan')
                        if (target%manhattanDist(curr) .le. radius) then 
                            if (size(res) .eq. arrSize) then 
                                allocate(tmp(2*size(res)))
                                tmp(1:size(res)) = res
                                call move_alloc(from=tmp, to=res)
                            end if 
                            arrSize = arrSize + 1
                            res(arrSize)%p => curr
                        end if
                    
                    
                    case ('chebyshev')
                        if (target%chebyshevDist(curr) .le. radius) then 
                            if (size(res) .eq. arrSize) then 
                                allocate(tmp(2*size(res)))
                                tmp(1:size(res)) = res
                                call move_alloc(from=tmp, to=res)
                            end if 
                            arrSize = arrSize + 1
                            res(arrSize)%p => curr
                        end if
                    
                    case default
                        error stop "rNN: unknown metric"
                end select
                
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
