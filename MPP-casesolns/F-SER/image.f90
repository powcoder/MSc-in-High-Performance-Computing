!
! A serial solution to the Case Study exercise from the MPP course.
!

program casestudy

  use pgmio
 
  implicit none

  integer, parameter :: M=192, N=128

  integer, parameter :: MAXITER   = 1500
  integer, parameter :: PRINTFREQ =  200

  double precision, dimension(0:M+1, 0:N+1) :: new, old, edge

  double precision, dimension(M,N) :: buf

  integer, parameter :: maxlen = 32

  character*(maxlen) :: filename

  integer :: i, j, iter

  write(*,*) 'Processing ', M, ' x ' , N, ' image'
  write(*,*) 'Number of iterations = ', MAXITER

  filename = 'edge192x128.pgm'

  write(*,*)
  write(*,*) 'Reading ', filename
  write(*,*)

  call pgmread(filename, buf)

  do j = 1, N
    do i = 1, M

      edge(i,j) = buf(i,j)

    end do
  end do

  do j = 0, N+1
    do i = 0, M+1

      old(i,j) = 255.0

    end do
  end do


  do iter = 1, MAXITER
    
    if (mod(iter,PRINTFREQ) .eq. 0)  then

      write(*,*) 'Iteration ', iter

    end if

    do j = 1, N
      do i = 1, M

        new(i,j) = 0.25*(old(i+1,j)+old(i-1,j)+old(i,j+1)+old(i,j-1) &
                         - edge(i,j))

      end do
    end do

    do j = 1, N
      do i = 1, M

        old(i,j) = new(i,j)

      end do
    end do

  end do

  write(*,*)
  write(*,*) 'Finished ', iter-1, ' iterations'

!  Gather data

  do j = 1, N
    do i = 1, M

      buf(i,j) = old(i,j)

    end do
  end do

  filename='image192x128.pgm'
  write(*,*)
  write(*,*) 'Writing ', filename
  call pgmwrite(filename, buf)

end program casestudy
