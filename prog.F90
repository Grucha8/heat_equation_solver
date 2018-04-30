program main
    use gausEl 
    use matrixOpertations
    
    implicit none
    integer(kind = 4) :: N = 5
    real(kind  = 4) :: h
    real(kind = 4), allocatable, dimension(:, :) :: A(:, :)
    real(kind = 4), allocatable, dimension(:) :: X(:), B(:)

    !distance between points
    h = 1.0 / N

    print *, h

    allocate(A(N-1, N-1))
    allocate(X(N-1))
    allocate(B(N-1))

    call fillMainMatrix(A, N, h)
    call fillRightSideMatrix(X, N)

    print *, A

    call gausEllimination(N, A, X)

    print *, A

    print *, X

    call equationSolver(A, B, X, N)

    print *, "========================="
    print *, B 

end program main