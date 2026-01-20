module precision

  implicit none

  integer, parameter :: sp = selected_real_kind(6,30)
  integer, parameter :: dp = selected_real_kind(14,100)

  ! Working precision used by some provided utility code.
  integer, parameter :: wp = dp

  integer, parameter :: lp = selected_int_kind(18)

end module precision
