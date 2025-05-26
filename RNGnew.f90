!****************************************************************
!                         MODULE RandomState
!    Módulo con estado por hilo para xoroshiro128+
!****************************************************************
module RandomState
  use omp_lib
  implicit none
  integer(8), dimension(:,:), allocatable :: state  ! [2, num_threads]
  integer :: num_threads

contains

  subroutine init_random_state()
    integer :: i, count
    real(8) :: r(2)
    call system_clock(count)
    call random_seed()
    call random_number(r)

    num_threads = omp_get_max_threads()
    allocate(state(2, num_threads))

    do i = 1, num_threads
       state(1,i) = ieor(int(count + i*1009,8), int(r(1) * real(huge(1_8)),8))
       state(2,i) = ieor(int(count + i*337,8),  int(r(2) * real(huge(1_8)),8))
       if (state(1,i) == 0_8 .and. state(2,i) == 0_8) state(2,i) = 1_8
    end do
  end subroutine init_random_state

  function rotl(x, k) result(res)
    integer(8), intent(in) :: x, k
    integer(8) :: res
    res = ior(shiftl(x, k), shiftr(x, 64_8 - k))
  end function rotl

  function xoroshiro128p(thread_id) result(randval)
    integer, intent(in) :: thread_id
    real(8) :: randval
    integer(8) :: s0, s1, result_
    integer(8) :: t0, t1

    s0 = state(1, thread_id)
    s1 = state(2, thread_id)
    result_ = s0 + s1

    t0 = s0
    t1 = s1
    t1 = ieor(t1, t0)
    state(1, thread_id) = ieor(rotl(t0, 55_8), ieor(t1, shiftl(t1, 14_8)))
    state(2, thread_id) = rotl(t1, 36_8)

    randval = abs(result_ * 5.42101086242752217E-20_8)
  end function xoroshiro128p

end module RandomState

