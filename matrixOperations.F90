module matrixOpertations

    implicit none

    contains
    subroutine fillMainMatrix(A, N, h)
        integer(kind = 4) :: N, i
        real(kind = 4) :: A(:, :), h
        real(kind = 4) :: P1, P2, P3

        P1 = 1 / h**2
        P2 = -2 / h**2
        P3 = 1 / h**2

        A(1, 1) = P2

        do i = 2, N-1
            A(i, i) = P2
            A(i-1, i) = P1
            A(i, i-1) = P3
        end do 
    end subroutine  

    subroutine fillRightSideMatrix(X, N)
        integer(kind = 4) :: N, i
        real(kind = 4) :: X(:)

        do i = 1, N-1
            X(i) = 2 + 10*((1.0*i)/N)
        end do
    end subroutine

    subroutine equationSolver(A, B, X, N)
        real(kind = 4) :: A(:, :), B(:), X(:)
        integer(kind = 4) :: N, i

        B(N-1) = X(N-1)
        do i = N-2, 1, -1
            B(i) = X(i) - A(i-1, i) * B(i+1)
        end do

    end subroutine
    

end module matrixOpertations
