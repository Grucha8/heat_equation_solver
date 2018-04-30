program main
    use gausEl
    
    implicit none
    integer(kind = 4) :: length, status, i, j
    integer(kind = 8) :: N = 5
    real(kind = 4), allocatable, dimension(:, :) :: A(:, :)
    real(kind = 4), allocatable, dimension(:) :: X(:)

    allocate(A(5, 5))
    allocate(X(5))

    call random_number(A)

    A = A * 10

    call random_number(X)

    X = X * 10

    write (*,*) A

    call gausEllimination(N, A, X)

    write (*,*) A



end program main