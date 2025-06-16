program test_auto_seed
  use xoroshiro128plus
  implicit none
  integer :: i

  call xoroshiro_auto_seed()  ! ← ¡Semilla automática!

  do i = 1, 5
    print *, xoroshiro_rand()
  end do
end program test_auto_seed