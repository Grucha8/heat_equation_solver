#define KIND 16

program main
    use gausEl 
    use matrixOpertations
    
    implicit none
    integer :: K = 4
    integer(kind = 4) :: N = 10, i
    real(kind  = KIND) :: h
    real(kind = KIND), allocatable, dimension(:, :) :: A(:, :)
    real(kind = KIND), allocatable, dimension(:) :: X(:), B(:), Points(:), Values(:)

    !distance between points
    h = 1.0 / N

    allocate(A(N, N))
    allocate(X(N))
    allocate(B(N))

    call fillMainMatrix(A, N, h)
    call fillRightSideMatrix(X, N)

    call gausEllimination(N, A, X)

    call equationSolver(A, B, X, N)

    allocate(Values(N+1))
    allocate(Points(N+1))

    !filling values 
    do i = 2, N+1
        Values(i) = B(i-1)
    end do
    Values(1) = 0

    !filling points
    do i = 1, N+1
        Points(i) = 1.0*(i - 1) / N
    end do

    open(unit = 20, file = "solve.txt")

    write (20,*) Values
    write (20,*) Points

    close(20)

end program main