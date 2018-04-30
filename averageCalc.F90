#define KIND 8

module averageCalc
    contains
        subroutine average(B, N, avg)
            implicit none
            integer :: N, i
            real(kind = 16) :: sum = 0.0, avg
            real(kind = KIND) :: B(:)

            do i = 1, N+1
                sum = sum + B(i)
            end do
            sum = sum / (N+1)

            do i = 1, N+1
                avg = avg + (B(i) - sum) ** 2
            end do

            avg = sqrt(avg / ((N+1)*N))

        end subroutine

end module