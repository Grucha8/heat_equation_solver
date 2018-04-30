#define KIND 8

module matrixOpertations

    implicit none

    contains
    subroutine fillMainMatrix(A, N, h)
        integer(kind = 4) :: N, i
        real(kind = KIND) :: A(:, :), h
        real(kind = KIND) :: P1, P2, P3

        P1 = 1 / h**2
        P2 = -2 / h**2
        P3 = 1 / h**2

        A(1, 1) = P2

        do i = 2, N
            A(i, i) = P2
            A(i-1, i) = P1
            A(i, i-1) = P3
        end do 

        A(N, N) = 1
        A(N-1, N) = 0

    end subroutine  

    subroutine fillRightSideMatrix(X, N)
        integer(kind = 4) :: N
        real(kind = KIND) :: X(:)

        X(N) = 1
    end subroutine

    subroutine equationSolver(A, B, X, N)
        real(kind = KIND) :: A(:, :), B(:), X(:)
        integer(kind = 4) :: N, i

        B(N) = X(N)
        do i = N-1, 1, -1
            B(i) = X(i) - A(i-1, i) * B(i+1)
        end do

    end subroutine
    

end module matrixOpertations
