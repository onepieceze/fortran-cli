module cli_type_mod

  use string
  use parse_arg_mod
  use linked_list_mod

  implicit none

  private

  public command_line_interface

  type :: argument_type
    character(:)  , allocatable :: switch
    character(:)  , allocatable :: switch_ab
    logical                     :: require = .False.
    character(:)  , allocatable :: help
    class(*)      , pointer     :: default => null()
    integer                     :: id = 0
    integer                     :: next_id = 0
  end type

  type :: command_line_interface
    character(:)  , allocatable     :: description
    character(:)  , allocatable     :: author
    character(512), allocatable     :: examples(:)
    type(linked_list_type), pointer :: arguments => null()
    logical      , private          :: is_init = .False.
  contains
    procedure, public  :: init
    procedure, public  :: add
    procedure, public  :: parser
    procedure, private :: get_string
    procedure, private :: get_integer 
    procedure, private :: get_float
    procedure, private :: get_double
    procedure, private :: get_string_array
    procedure, private :: get_integer_array 
    procedure, private :: get_float_array
    procedure, private :: get_double_array
    generic            :: get => &
                          get_string, &
                          get_integer, & 
                          get_float, &
                          get_double, &
                          get_string_array, &
                          get_integer_array, &
                          get_float_array, &
                          get_double_array
    procedure, private :: usage
    final              :: end

  end type

contains

  subroutine init(this, author, description, examples)

    class(command_line_interface), intent(inout) :: this
    character(*),   optional,      intent(in)    :: description
    character(*),   optional,      intent(in)    :: author
    character(*), optional,      intent(in)    :: examples(1:)

    integer :: i
    
    if (present(author)) this%author = author
    if (present(description)) this%description = description

    if (present(examples)) then
      allocate (this%examples(1:size(examples)))
      do i=1, size(examples)
      this%examples(i) = trim(examples(i))
      end do
    end if

    this%is_init = .True.

  end subroutine init


  subroutine add(this, switch, switch_ab, require, help, default)

    class(command_line_interface)        , intent(inout) :: this
    character(*)                         , intent(in)    :: switch
    character(*), optional               , intent(in)    :: switch_ab
    logical     , optional               , intent(in)    :: require
    character(*), optional               , intent(in)    :: help
    class(*)    , optional       , target, intent(in)    :: default

    type(argument_type) , pointer :: argument

    if (.not. this%is_init) stop 'Error: have not init command line interface.'

    allocate(argument)

    argument%switch = switch
    if (present(switch_ab)) then
      argument%switch_ab = switch_ab
    else
      argument%switch_ab = '  '
    end if
    if (present(require))   argument%require   = require
    if (present(help))      argument%help      = help
    if (present(default))   argument%default  => default

    if (.not. associated(this%arguments)) allocate(this%arguments)

    call this%arguments%append_ptr(switch, argument)

  end subroutine


  subroutine parser(this)

    class(command_line_interface), intent(inout) :: this

    class(*)      , pointer      :: argument
    character(512)               :: string
    integer                      :: next_id
    integer                      :: tail_id
    logical                      :: is_exist(128)
    integer                      :: iarg_number
    integer                      :: require_number

    integer :: i, j

    if (.not. this%is_init) stop 'Error: have not init command line interface.'
    if (.not. associated(this%arguments)) stop 'Error: have not add any arguments.'

    if (iargc()==1) then
      call getarg(1, string)
      if (string == "-h" .or. string == "--help") call this%usage
    end if

    iarg_number = 0
    require_number = 0
    next_id = 0
    is_exist = .False.
    do j=iargc(), 1, -1
    call getarg(j, string)
    do i=1, this%arguments%size
      argument => this%arguments%value_at(i)
      select type (argument)
      type is (argument_type)
        if (argument%switch == string .or. argument%switch_ab == string) then
          argument%id = j
          if (iarg_number /= 0) then 
            argument%next_id = next_id
            if ((next_id-j) <2) call this%usage()
          end if
          next_id = j
          is_exist(i) = .True.
          iarg_number = iarg_number + 1
        end if
        if (argument%require .and. .not. is_exist(i) .and. j==1) call this%usage()
        if (argument%require .and. j == iargc()) require_number = require_number + 1
      end select
    end do
    end do

    if (iarg_number < require_number) call this%usage()

  end subroutine parser


  subroutine get_string(this, key, value)

    class(command_line_interface), intent(inout) :: this
    character(*)                 , intent(in)    :: key
    character(*)                 , intent(out)   :: value

    class(*), pointer :: argument

    argument => this%arguments%value(key)
    if (.not. associated(argument)) then
      stop 'Error: get command line function should use argument switch.'
    end if
    select type (argument)
    type is (argument_type)
      if (argument%id == 0) then
        if (associated(argument%default)) then
          select type (default_value => argument%default)
          type is (character(*))
            value = default_value
          class default
            stop ' Error: default data type and out put value not match.'
          end select
        end if
      else
        call parse_arg(argument%id, value)
      end if
    end select

  end subroutine get_string


  subroutine get_integer(this, key, value)

    class(command_line_interface), intent(inout) :: this
    character(*)                 , intent(in)    :: key
    integer                      , intent(out)   :: value

    class(*), pointer :: argument
    character(512)    :: string

    argument => this%arguments%value(key)
    if (.not. associated(argument)) then
      stop 'Error: get command line function should use argument switch.'
    end if
    select type (argument)
    class is (argument_type)
    print*, "get_integer arg"
      if (argument%id == 0) then
        if (associated(argument%default)) then
          select type (default_value => argument%default)
          type is (integer(4))
            value = default_value
          class default
            stop ' Error: default data type and out put value not match.'
          end select
        else
          print*, 'Error: '//trim(key)//' have not default value.'
        end if
      else
        call parse_arg(argument%id, string)
        value = to_integer(string)
      end if
    end select

  end subroutine get_integer


  subroutine get_float(this, key, value)

    class(command_line_interface), intent(inout) :: this
    character(*)                 , intent(in)    :: key
    real(4)                      , intent(out)   :: value

    class(*), pointer :: argument
    character(512)    :: string

    argument => this%arguments%value(key)
    if (.not. associated(argument)) then
      stop 'Error: get command line function should use argument switch.'
    end if
    select type (argument)
    type is (argument_type)
      if (argument%id == 0) then
        if (associated(argument%default)) then
          select type (default_value => argument%default)
          type is (real(4))
            value = default_value
          class default
            stop ' Error: default data type and out put value not match.'
          end select
        else
          print*, 'Error: '//trim(key)//' have not default value.'
        end if
      else
        call parse_arg(argument%id, string)
        value = to_float(string)
      end if
    end select

  end subroutine get_float


  subroutine get_double(this, key, value)

    class(command_line_interface), intent(inout) :: this
    character(*)                 , intent(in)    :: key
    real(8)                      , intent(out)   :: value

    class(*), pointer :: argument
    character(512)    :: string

    argument => this%arguments%value(key)
    if (.not. associated(argument)) then
      stop 'Error: get command line function should use argument switch.'
    end if
    select type (argument)
    type is (argument_type)
      if (argument%id == 0) then
        if (associated(argument%default)) then
          select type (default_value => argument%default)
          type is (real(8))
            value = default_value
          class default
            stop ' Error: default data type and out put value not match.'
          end select
        else
          print*, 'Error: '//trim(key)//' have not default value.'
        end if
      else
        call parse_arg(argument%id, string)
        value = to_double(string)
      end if
    end select

  end subroutine get_double


  subroutine get_string_array(this, key, value)

    class(command_line_interface), intent(inout) :: this
    character(*)                 , intent(in)    :: key
    character(*), allocatable    , intent(out)   :: value(:)

    class(*), pointer :: argument

    argument => this%arguments%value(key)
    if (.not. associated(argument)) then
      stop 'Error: get command line function should use argument switch.'
    end if
    select type (argument)
    type is (argument_type)
      if (argument%id == 0) return
      call parse_arg_array(argument%id, argument%next_id, value)
    end select

  end subroutine get_string_array


  subroutine get_integer_array(this, key, value)

    class(command_line_interface), intent(inout) :: this
    character(*)                 , intent(in)    :: key
    integer     , allocatable    , intent(out)   :: value(:)

    class(*)      , pointer     :: argument
    character(512), allocatable :: string(:)

    integer :: i

    argument => this%arguments%value(key)
    if (.not. associated(argument)) then
      stop 'Error: get command line function should use argument switch.'
    end if
    select type (argument)
    type is (argument_type)
      call parse_arg_array(argument%id, argument%next_id, string)
    end select

    allocate(value(size(string)))

    do i=1, size(string)
      value(i) = to_integer(string(i))
    end do

    deallocate(string)

  end subroutine get_integer_array


  subroutine get_float_array(this, key, value)

    class(command_line_interface), intent(inout) :: this
    character(*)                 , intent(in)    :: key
    real(4)     , allocatable    , intent(out)   :: value(:)

    class(*)      , pointer     :: argument
    character(512), allocatable :: string(:)

    integer :: i

    argument => this%arguments%value(key)
    if (.not. associated(argument)) then
      stop 'Error: get command line function should use argument switch.'
    end if
    select type (argument)
    type is (argument_type)
      call parse_arg_array(argument%id, argument%next_id, string)
    end select

    allocate(value(size(string)))

    do i=1, size(string)
      value(i) = to_float(string(i))
    end do

    deallocate(string)

  end subroutine get_float_array


  subroutine get_double_array(this, key, value)

    class(command_line_interface), intent(inout) :: this
    character(*)                 , intent(in)    :: key
    real(8)     , allocatable    , intent(out)   :: value(:)

    class(*)      , pointer     :: argument
    character(512), allocatable :: string(:)

    integer :: i

    argument => this%arguments%value(key)
    if (.not. associated(argument)) then
      stop 'Error: get command line function should use argument switch.'
    end if
    select type (argument)
    type is (argument_type)
      if (argument%id == 0) return
      call parse_arg_array(argument%id, argument%next_id, string)
    end select

    allocate(value(size(string)))

    do i=1, size(string)
      value(i) = to_double(string(i))
    end do

    deallocate(string)

  end subroutine get_double_array


  subroutine end(this)

    type(command_line_interface), intent(inout) :: this

    class(*), pointer :: argument
    integer           :: i

    if (.not. this%is_init) stop 'Error: have not init command line interface.'
    if (.not. associated(this%arguments)) stop 'Error: have not add any arguments.'
    do i=1, this%arguments%size
      argument => this%arguments%value_at(i)
      select type (argument)
      type is (argument_type)
        if (associated(argument%default)) deallocate(argument%default)
      end select
    end do

    deallocate(this%arguments)

  end subroutine


  subroutine usage(this)

    class(command_line_interface), intent(inout) :: this

    class(*), pointer           :: argument
    integer                     :: i

    if (.not. this%is_init) stop 'Error: have not init command line interface.'
    if (.not. associated(this%arguments)) stop 'Error: have not add any arguments.'

    write(*, '(g0)') "                "
    if (allocated(this%description)) then
      write(*, '(g0)') this%description, ' '
    end if
    if (allocated(this%author)) then
      write(*, '(g0)') 'author: ', '  '//this%author
    end if
    write(*, '(g0)') 'usage:'
    if (allocated(this%examples)) then
      do i=1, size(this%examples)
        write(*, '(g0)') '  '//trim(this%examples(i))
      end do
    end if
    write(*, '(g0)') 'help:'
    do i=1, this%arguments%size
      argument => this%arguments%value_at(i)
      select type (argument)
      type is (argument_type)
        if (allocated(argument%help)) then
          if (argument%require) then
            write(*, '(g0)') '  '//argument%switch//'['//argument%switch_ab//']: '//argument%help//' (required).'
          else
            write(*, '(g0)') '  '//argument%switch//'['//argument%switch_ab//']: '//argument%help//' (optional).'
          end if
        end if
      end select
    end do
    write(*, '(g0)') "                "

    stop

  end subroutine

end module cli_type_mod