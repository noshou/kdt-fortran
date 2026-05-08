submodule(KdTreeModule:SearchSubmod) RnnModule
    implicit none 
    contains 
        module procedure rNN
            real(kind=real64)                           :: delta
            type(nodePtr), allocatable                  :: tmp(:)
            integer                                     :: axis
            
            if (associated(curr)) then 
                if (target%eucDist(curr) .le. radius) then 
                    if (size(res) .eq. arrSize) then 
                        allocate(tmp(2*size(res)))
                        tmp(1:size(res)) = res
                        call move_alloc(from=tmp, to=res)
                    end if 
                    arrSize = arrSize + 1
                    res(arrSize)%p => curr
                end if
                axis  = curr%splitAxis
                delta = target%coords(axis) - curr%coords(axis)
                if (delta < 0) then 
                    call rNN(target, curr%leftChild, radius, res, arrSize)
                    if (-delta .le. radius) call rNN(target, curr%rightChild, radius, res, arrSize)
                else 
                    call rNN(target, curr%rightChild, radius, res, arrSize)
                    if (delta .le. radius) call rNN(target, curr%leftChild, radius, res, arrSize)
                end if
            end if 
        end procedure rNN

end submodule RnnModule
