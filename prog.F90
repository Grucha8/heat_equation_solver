#define KIND 8

program main
    use gausEl 
    use matrixOpertations
    use averageCalc
    
    implicit none
    integer(kind = 4) :: N = 100, i
    real(kind = 16) :: avg = 0.0, pointsAvg = 0.0
    real(kind  = KIND) :: h
    real(kind = KIND), allocatable, dimension(:, :) :: A(:, :)
    real(kind = KIND), allocatable, dimension(:) :: X(:), B(:), Points(:), Values(:)

    !distance between points
    h = 1.0 / N

    if (allocated(A)) deallocate(A)
    allocate(A(N, N))

    if(allocated(X)) deallocate(X)
    allocate(X(N))

    if(allocated(B)) deallocate(B)
    allocate(B(N))

    call fillMainMatrix(A, N, h)
    call fillRightSideMatrix(X, N)

    call gausEllimination(N, A, X)

    call equationSolver(A, B, X, N)

    if(allocated(Points)) deallocate(Points)
    allocate(Points(N+1))

    !filling points
    do i = 1, N+1
        Points(i) = 1.0*(i-1) / N
    end do

    if(allocated(Values)) deallocate(Values)
    allocate(Values(N+1))
    
    !filling values
    do i = 2, N+1
        Values(i) = B(i-1)
    end do
    Values(1) = 0

    !input results to file 
    open(unit = 10, file = "Results.txt")

    write(10,*) "Values: "
    write(10,*) Values
    write(10,*) Points

    call average(Values, N, avg)
    call average(Points, N, pointsAvg)
    
    write(10,*) "n = ", N, " Kind = ", KIND
    write(10,*) "Average from calc = ", avg
    write(10,*) "Average from result = ", pointsAvg
    write(10,*) "Difference = ", abs(avg - pointsAvg)

    close(10)

end program main