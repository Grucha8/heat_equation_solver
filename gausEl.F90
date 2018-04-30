#define KIND 8

module gausEl


    contains
    subroutine gausEllimination (N, A, X)
        integer(kind = 4) :: N, i, j
        real(kind = KIND) :: A(:, :), X(:), c

        do i = 1, N
            do j = 1, N
                if (i.NE.j) then
                    c = A(i, j) / A(i, i)
                    A(:, j) = A(:, j) - c * A(:, i)
                    X(j) = X(j) - c * X(i)
                    X(i) = X(i) / A(i, i)
                    A(:, i) = A(:, i) / A(i, i)
                end if
            end do
        end do
    end subroutine
end module

!dwa tygodnie od 12.04.2018