submodule(KdTree) TreeUtils
    use iso_fortran_env, only: output_unit, int64
    implicit none
    contains
        
        module procedure printTree
            integer :: u
            u = output_unit
            if (present(unit)) u = unit
            if (associated(this%root)) then
                call this%root%printNode(0_int64, u)
            else
                write(u, '(A)') '**empty tree**'
            end if
        end procedure printTree

        module procedure isMember
            if (.not. associated(target)) then 
                res = .false.
            else 
                res = this%treeId .eq. target%treeId
            end if
        end procedure isMember

        !> Returns the substring of s from the first '(' onward, or
        !! adjustl(s) if no '(' is present (e.g. '**empty tree**').
        function stripPrefix(s) result(t)
            character(len=*), intent(in) :: s
            character(len=64)            :: t
            integer                      :: pos
            pos = index(s, '(')
            if (pos.eq.0) then
                t = adjustl(s)
            else
                t = s(pos:)
            end if
        end function stripPrefix

        !> Insertion sort, lexicographic, in place.
        subroutine sortLines(arr)
            character(len=*), intent(inout) :: arr(:)
            integer                         :: i, j
            character(len=len(arr))         :: tmp
            do i = 2, size(arr)
                tmp = arr(i)
                j = i - 1
                do while (j .ge. 1)
                    if (.not. (arr(j) .gt. tmp)) exit
                    arr(j+1) = arr(j)
                    j = j - 1
                end do
                arr(j+1) = tmp
            end do
        end subroutine sortLines

        !> Prints a labeled block of lines for diagnostics.
        subroutine dumpLines(label, lines)
            character(len=*), intent(in) :: label
            character(len=*), intent(in) :: lines(:)
            integer                      :: i
            write(*, '(A)') label
            do i = 1, size(lines)
                write(*, '(A)') '    "' // trim(lines(i)) // '"'
            end do
        end subroutine dumpLines

        module procedure assert

            integer                        :: u, ios, i
            character(len=64)              :: line
            character(len=64), allocatable :: actual(:), expCopy(:)

            ! capture per-node lines from printTree, stripped to coord-tuples
            allocate(actual(0))
            open(newunit=u, status='scratch', action='readwrite')
            call this%printTree(unit=u)
            rewind(u)
            do
                read(u, '(A)', iostat=ios) line
                if (ios .ne. 0) exit
                actual = [actual, stripPrefix(line)]
            end do
            close(u)

            if (size(actual) .ne. size(expected)) then
                write(*, '(A,I0,A,I0)') '--- ' // testName // ' FAILED: node count mismatch &
                    &— expected ', size(expected), ', got ', size(actual)
                call dumpLines('  expected:', expected)
                call dumpLines('  got:     ', actual)
                stop 1
            end if

            ! make a sortable copy of expected, also stripped to coord-tuples
            allocate(expCopy(size(expected)))
            do i = 1, size(expected)
                expCopy(i) = stripPrefix(expected(i))
            end do

            call sortLines(actual)
            call sortLines(expCopy)

            do i = 1, size(actual)
                if (trim(actual(i)) .ne. trim(expCopy(i))) then
                    write(*, '(A)') '--- ' // testName // ' FAILED: node set mismatch'
                    call dumpLines('  expected (coords, sorted):', expCopy)
                    call dumpLines('  got      (coords, sorted):', actual)
                    stop 1
                end if
            end do
        end procedure assert
        
        module procedure associatedNodePool
            assoc = associated(this%nodePool)
        end procedure associatedNodePool

        module procedure associatedRoot
            assoc = associated(this%root)
        end procedure associatedRoot

        module procedure destroy
            if (associated(this%nodePool)) deallocate(this%nodePool)
            this%root        => null()
            this%dim         = 0_int64
            this%pop         = 0_int64
            this%treeId      = 0_int64
            this%initialized = .false.
        end procedure destroy

        module procedure finalizer
            call destroy(this)
        end procedure finalizer

end submodule TreeUtils