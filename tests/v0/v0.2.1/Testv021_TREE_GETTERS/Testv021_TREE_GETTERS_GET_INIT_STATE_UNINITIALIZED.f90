program Testv021_TREE_GETTERS_GET_INIT_STATE_UNINITIALIZED
    use KdTree
    implicit none
    call getInitStateUninitialized()
    contains

        !> getInitState must return .false. before build is called.
        subroutine getInitStateUninitialized()
            type(Tree) :: t
            logical    :: isInit

            call t%getInitState(isInit)

            if (isInit) then
                write(*, '(A)') '--- Testv021_TREE_GETTERS_GET_INIT_STATE_UNINITIALIZED ---'
                write(*, '(A)') 'expected: isInit = F'
                write(*, '(A)') 'got:      isInit = T'
                stop 1
            end if

        end subroutine getInitStateUninitialized

end program Testv021_TREE_GETTERS_GET_INIT_STATE_UNINITIALIZED
