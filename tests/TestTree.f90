program TestTree
    use KdTree
    use iso_fortran_env, only: real64
    implicit none

    call empty()
    call onePoint()
    call twoPoints()
    call oneAxisArray()
    call twoAxisArray()
    call threeAxisArray()
    call fourAxisArray()
    call collinearOne_AxisI()
    call collinearOne_AxisII()
    call collinearOne_AxisIII()
    call collinearTwo_AxisIAxisII()
    call collinearTwo_AxisIAxisIII()
    call collinearTwo_AxisIIAxisIII()
    call duplicatesOneAxis()
    call duplicatesTwoAxis()
    call duplicatesThreeAxis()
    call duplicatesFourAxis()

    contains


        subroutine empty()
            type(tree)   :: t
            real(real64) :: coords(2, 0)

            write(*, '(A)') '--- empty ---'
            call t%build(coords)
            call t%printTree()
            write(*, *)
        end subroutine empty

        subroutine onePoint()
            type(tree)   :: t
            real(real64) :: coords(2, 1) = reshape([1.0_real64, 2.0_real64], [2, 1])

            write(*, '(A)') '--- onePoint ---'
            call t%build(coords)
            call t%printTree()
            write(*, *)
        end subroutine onePoint

        subroutine twoPoints()
            type(tree)   :: t
            real(real64) :: coords(2, 2) = reshape( &
                [1.0_real64, 5.0_real64,    &
                3.0_real64, 2.0_real64], [2, 2])

            write(*, '(A)') '--- twoPoints ---'
            call t%build(coords)
            call t%printTree()
            write(*, *)
        end subroutine twoPoints

        subroutine oneAxisArray()
            type(tree)   :: t
            real(real64) :: coords(1, 7) = reshape( &
                [3.0_real64, 1.0_real64, 4.0_real64, 1.0_real64, &
                5.0_real64, 9.0_real64, 2.0_real64], [1, 7])

            write(*, '(A)') '--- oneAxisArray ---'
            call t%build(coords)
            call t%printTree()
            write(*, *)
        end subroutine oneAxisArray

        subroutine twoAxisArray()
            type(tree) :: t
            real(real64) :: coords(2, 7) = reshape( &
                [3.0_real64, 1.0_real64, &
                1.0_real64, 4.0_real64, &
                4.0_real64, 1.0_real64, &
                1.0_real64, 5.0_real64, &
                5.0_real64, 9.0_real64, &
                9.0_real64, 2.0_real64, &
                2.0_real64, 6.0_real64], [2, 7])

            write(*, '(A)') '--- twoAxisArray ---'
            call t%build(coords)
            call t%printTree()
            write(*, *)
        end subroutine twoAxisArray

        subroutine threeAxisArray()
            type(tree) :: t
            real(real64) :: coords(3, 7) = reshape(         &
                [3.0_real64, 1.0_real64, -13.31_real64,     &
                1.0_real64, 4.0_real64,  34.14_real64,     &
                4.0_real64, 1.0_real64, -1093.3139_real64, &
                1.0_real64, 5.0_real64,  0.013_real64,     &
                5.0_real64, 9.0_real64,  33.13_real64,     &
                9.0_real64, 2.0_real64,  734.348_real64,   &
                2.0_real64, 6.0_real64, -93.131_real64], [3, 7])

            write(*, '(A)') '--- threeAxisArray ---'
            call t%build(coords)
            call t%printTree()
            write(*, *)
        end subroutine threeAxisArray

        subroutine fourAxisArray()
            type(tree) :: t
            real(real64) :: coords(4, 7) = reshape(                                &
                [3.0_real64, 1.0_real64, -13.31_real64,        0.92_real64,         &
                1.0_real64, 4.0_real64,  34.14_real64,       92.0_real64,          &
                4.0_real64, 1.0_real64, -1093.3139_real64,  312.0_real64,          &
                1.0_real64, 5.0_real64,  0.013_real64,    13112.0_real64,          &
                5.0_real64, 9.0_real64,  33.13_real64,       -5.13192_real64,      &
                9.0_real64, 2.0_real64,  734.348_real64,      0.00000092_real64,   &
                2.0_real64, 6.0_real64, -93.131_real64,       0.9412412_real64], [4, 7])

            write(*, '(A)') '--- fourAxisArray ---'
            call t%build(coords)
            call t%printTree()
            write(*, *)
        end subroutine fourAxisArray

        subroutine collinearOne_AxisI()
            type(tree)   :: t
            real(real64) :: coords(3, 6) = reshape(             &
                [5.0_real64, 1.0_real64,  0.92_real64,          &
                5.0_real64, 2.0_real64,  0.42_real64,          &
                5.0_real64, 3.0_real64,  0.00003_real64,       &
                5.0_real64, 4.0_real64,  93291.0_real64,       &
                5.0_real64, 5.0_real64, -93131913.0_real64,    &
                5.0_real64, 6.0_real64,  0.0_real64], [3, 6])

            write(*, '(A)') '--- collinearOne_AxisI ---'
            call t%build(coords)
            call t%printTree()
            write(*, *)
        end subroutine collinearOne_AxisI

        subroutine collinearOne_AxisII()
            type(tree)   :: t
            real(real64) :: coords(3, 6) = reshape(           &
                [  1.0_real64, 4.0_real64,  0.92_real64,      &
                52.0_real64, 4.0_real64,  0.42_real64,      &
                13.0_real64, 4.0_real64,  0.00003_real64,   &
                87.0_real64, 4.0_real64,  93291.0_real64,   &
                -98.0_real64, 4.0_real64, -93131913.0_real64,&
                121.0_real64, 4.0_real64,  0.0_real64], [3, 6])

            write(*, '(A)') '--- collinearOne_AxisII ---'
            call t%build(coords)
            call t%printTree()
            write(*, *)
        end subroutine collinearOne_AxisII

        subroutine collinearOne_AxisIII()
            type(tree)   :: t
            real(real64) :: coords(3, 6) = reshape(             &
                [  1.0_real64,  0.92_real64,       4.0_real64,  &
                52.0_real64,  0.42_real64,       4.0_real64,  &
                13.0_real64,  0.00003_real64,    4.0_real64,  &
                87.0_real64,  93291.0_real64,    4.0_real64,  &
                -98.0_real64, -93131913.0_real64, 4.0_real64,  &
                121.0_real64,  0.0_real64,        4.0_real64], [3, 6])

            write(*, '(A)') '--- collinearOne_AxisIII ---'
            call t%build(coords)
            call t%printTree()
            write(*, *)
        end subroutine collinearOne_AxisIII

        subroutine collinearTwo_AxisIAxisII()
            type(tree)   :: t
            real(real64) :: coords(3, 6) = reshape(  &
                [5.0_real64, 4.0_real64,  2.0_real64,    &
                5.0_real64, 4.0_real64,  1.0_real64,    &
                5.0_real64, 4.0_real64, -131.0_real64,  &
                5.0_real64, 4.0_real64,  31313.0_real64,&
                5.0_real64, 4.0_real64, -31.0_real64,   &
                5.0_real64, 4.0_real64,  432.419_real64], [3, 6])

            write(*, '(A)') '--- collinearTwo_AxisIAxisII ---'
            call t%build(coords)
            call t%printTree()
            write(*, *)
        end subroutine collinearTwo_AxisIAxisII

        subroutine collinearTwo_AxisIAxisIII()
            type(tree)   :: t
            real(real64) :: coords(3, 6) = reshape(  &
                [5.0_real64,  2.0_real64,    4.0_real64, &
                5.0_real64,  1.0_real64,    4.0_real64, &
                5.0_real64, -131.0_real64,  4.0_real64, &
                5.0_real64,  31313.0_real64,4.0_real64, &
                5.0_real64, -31.0_real64,   4.0_real64, &
                5.0_real64,  432.419_real64,4.0_real64], [3, 6])

            write(*, '(A)') '--- collinearTwo_AxisIAxisIII ---'
            call t%build(coords)
            call t%printTree()
            write(*, *)
        end subroutine collinearTwo_AxisIAxisIII

        subroutine collinearTwo_AxisIIAxisIII()
            type(tree)   :: t
            real(real64) :: coords(3, 6) = reshape(  &
                [  2.0_real64,    5.0_real64, 4.0_real64, &
                1.0_real64,    5.0_real64, 4.0_real64, &
                -131.0_real64,   5.0_real64, 4.0_real64, &
                31313.0_real64, 5.0_real64, 4.0_real64, &
                -31.0_real64,    5.0_real64, 4.0_real64, &
                432.419_real64, 5.0_real64, 4.0_real64], [3, 6])

            write(*, '(A)') '--- collinearTwo_AxisIIAxisIII ---'
            call t%build(coords)
            call t%printTree()
            write(*, *)
        end subroutine collinearTwo_AxisIIAxisIII

        subroutine duplicatesOneAxis()
            type(tree)   :: t
            real(real64) :: coords(1, 9) = reshape( &
                [5.0_real64, &
                5.0_real64, &
                5.0_real64, &
                5.0_real64, &
                5.0_real64, &
                5.0_real64, &
                5.0_real64, &
                5.0_real64, &
                5.0_real64], [1, 9])

            write(*, '(A)') '--- duplicatesOneAxis ---'
            call t%build(coords)
            call t%printTree()
            write(*, *)
        end subroutine duplicatesOneAxis

        subroutine duplicatesTwoAxis()
            type(tree)   :: t
            real(real64) :: coords(2, 9) = reshape( &
                [5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64], [2, 9])

            write(*, '(A)') '--- duplicatesTwoAxis ---'
            call t%build(coords)
            call t%printTree()
            write(*, *)
        end subroutine duplicatesTwoAxis

        subroutine duplicatesThreeAxis()
            type(tree)   :: t
            real(real64) :: coords(3, 9) = reshape( &
                [5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64], [3, 9])

            write(*, '(A)') '--- duplicatesThreeAxis ---'
            call t%build(coords)
            call t%printTree()
            write(*, *)
        end subroutine duplicatesThreeAxis

        subroutine duplicatesFourAxis()
            type(tree)   :: t
            real(real64) :: coords(4, 9) = reshape( &
                [5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64], [4, 9])

            write(*, '(A)') '--- duplicatesFourAxis ---'
            call t%build(coords)
            call t%printTree()
            write(*, *)
        end subroutine duplicatesFourAxis

end program TestTree