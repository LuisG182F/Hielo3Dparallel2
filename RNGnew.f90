module xoroshiro128plus
  use iso_fortran_env, only: int32, int64, real64
  implicit none
  private
  public :: xoroshiro_seed, xoroshiro_seed_from_time, xoroshiro_auto_seed
  public :: xoroshiro_next, xoroshiro_rand

  integer(int64) :: s(2)  ! Estado interno del generador
integer(int32), parameter :: hi = int(Z'9E3779B9', int32)
  integer(int32), parameter :: lo = int(Z'7F4A7C15', int32)

  ! Combinar los dos para formar el int64
  integer(int64), parameter :: GOLDEN_RATIO_64 = shiftl(int(hi, int64), 32) + int(lo, int64)

contains

  function rotl(x, k) result(res)
    integer(int64), intent(in) :: x
    integer, intent(in) :: k
    integer(int64) :: res
    res = ior(ishft(x, k), ishft(x, k - 64))
  end function rotl

  subroutine xoroshiro_seed(seed1, seed2)
    integer(int64), intent(in) :: seed1, seed2
    if (seed1 == 0_int64 .and. seed2 == 0_int64) stop "Semillas no pueden ser ambas cero"
    s(1) = seed1
    s(2) = seed2
  end subroutine xoroshiro_seed

  subroutine xoroshiro_seed_from_time()
    integer(int64) :: count, rate, seed1, seed2
    call system_clock(count, rate)
    seed1 = int(count, int64)
    seed2 = seed1 * 6364136223846793005_int64 + 1_int64
    call xoroshiro_seed(seed1, seed2)
  end subroutine xoroshiro_seed_from_time

  subroutine xoroshiro_auto_seed()
    integer :: values(8)
    integer(int64) :: hash, i

    call date_and_time(values=values)
    ! values: [year, month, day, time zone, hour, minute, second, msec]

    hash = 0_int64
    do i = 1, 8
      hash = ieor(hash, int(values(i), int64))
      hash = hash * 6364136223846793005_int64 + 1_int64
    end do

call xoroshiro_seed(hash, hash * GOLDEN_RATIO_64 + 1_int64)
  end subroutine xoroshiro_auto_seed

  function xoroshiro_next() result(r)
    integer(int64) :: r
    integer(int64) :: s0, s1
    s0 = s(1)
    s1 = s(2)
    r = s0 + s1

    s1 = ieor(s1, s0)
    s(1) = ieor(rotl(s0, 55), ieor(s1, ishft(s1, 14)))
    s(2) = rotl(s1, 36)
  end function xoroshiro_next

  function xoroshiro_rand() result(r)
    real(real64) :: r
    integer(int64) :: x
    x = xoroshiro_next()
    r = real(ior(ishft(x, -11), z'3FF0000000000000'), real64) - 1.0_real64
  end function xoroshiro_rand

end module xoroshiro128plus
