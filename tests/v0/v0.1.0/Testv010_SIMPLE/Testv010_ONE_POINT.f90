program Testv010_ONE_POINT
    use KdTree
    use iso_fortran_env, only: real64
    implicit none
    call onePoint()

    contains

        !> Single point → root only.
        subroutine onePoint()
            type(Tree)   :: t
            real(real64) :: coords(2, 1) = reshape([1.0_real64, 2.0_real64], [2, 1])
            character(len=*), parameter :: expected(*) = [character(len=64) :: &
                '[axis=1] (1.000, 2.000)']

            call t%build(coords)
            call t%assert('onePoint', expected)
        end subroutine onePoint
end program Testv010_ONE_POINT