            module aleatorio
      implicit none
      integer, save :: n=6677
      end module aleatorio

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      program gna
      implicit none
      integer, parameter :: n = 500000!tamanho para o array
      real :: media
      integer :: i,j
      integer :: isemente
      integer :: nran
      real :: soma
      integer :: array(n), array2(n), x             !array
      integer :: t, count_max2
      INTEGER :: count, count2, count_rate, count_rate2, count_max
      CALL SYSTEM_CLOCK(count, count_rate, count_max)
     !! WRITE(*,*) count, count_rate, count_max
      isemente = count_rate2

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !grava os numeors aleatorios no array e no txt
      write(*,*) 'entre com a semente: '
      call semente(isemente)
      write(*,*)
      open (Unit=1,File="test.txt",status="replace")
      do i=1,n
         call num_aleatorio(nran)
         array(i)= nran
         write(*,*)nran
         write(1,*) nran
      end do
      close(Unit=1)
      write(*,*)'LOOP 1'

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !PEGA DO TXT E COLOCA NO VETOR

      open (Unit=10,File="test.txt", status='old', action='read')
      do i=1,n
         read (10,*) array(i)
      end do
      close(Unit=10)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !ordena
      do i = 2, n
         x = array(i)
         j = i - 1
         do while (j >= 1)
            if (array(j) <= x) exit
            array(j + 1) = array(j)
            j = j - 1
        end do
        array(j + 1) = x
      end do

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !mostrnado numeros retornado do txt
      write (*,*) 'do text'
       do i = 2,n
      write(*,*)array(i)
      end do

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !gravando no txt ordenado

      do i=100,n
         write(100,*) array(i)
      end do
      close(Unit=100)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      CALL SYSTEM_CLOCK(count2, count_rate2, count_max2)
      !WRITE(*,*) count2, count_rate2, count_max2
      WRITE(*,*)'tempo de compila‡Æo em milesimos'
      WRITE(*,*)count2 - count


      pause
      end program gna


      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine num_aleatorio(nran)
      use aleatorio
      implicit none

      real, intent(out) :: nran
      n = mod( 87*n+28417 , 134453)
      nran = (real(n) / 134453.0)/10000000

      end subroutine num_aleatorio


      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine semente(isemente)
      use aleatorio
      implicit none
      integer , intent(in) :: isemente

      n=abs(isemente)

      end subroutine semente


      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


