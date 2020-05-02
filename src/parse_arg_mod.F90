module parse_arg_mod

  implicit none

  private

  public :: parse_arg, parse_arg_array


contains

  subroutine parse_arg(id, value)
    
    integer     , intent(in)   :: id
    character(*), intent(in) :: value

    call getarg(id+1, value)

  end subroutine parse_arg

  subroutine parse_arg_array(id, next_id, value)
    
    integer                  , intent(in)  :: id
    integer                  , intent(in)  :: next_id
    character(*), allocatable, intent(out) :: value(:)
    
    integer :: i, ndim

    if (next_id /=0) then
      ndim = next_id - id -1
    else
      ndim = iargc() - id -1
    end if

    allocate(value(ndim))

    do i=1, ndim
      call getarg(id+i, value(i))
    end do

  end subroutine parse_arg_array

end module parse_arg_mod