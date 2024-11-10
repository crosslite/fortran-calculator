module calculator
    implicit none
    integer, private :: index
    character(100), private :: input

contains
    real function calculate(input__)
        character(100) :: input__
        character :: ch
        integer :: num

        call out('output stream of the calculator module')

        calculate = 0
        index = 1
        input = input__

        do while (index <= len_trim(input))
                ch = input(index:index)

                select case(ch)
                case ('+')
                        index = index + 1
                        calculate = calculate + get_num_from_str(input(index:100))
                case ('-')
                        index = index + 1
                        calculate = calculate - get_num_from_str(input(index:100))
                case ('*')
                        index = index + 1
                        calculate = calculate * get_num_from_str(input(index:100))
                case ('/')
                        index = index + 1
                        calculate = calculate / get_num_from_str(input(index:100))
                case ('0':'9')
                        num = get_num_from_str(input(index:100))
                        calculate = calculate + num
                        index = index - 1
                case default
                        call out('syntax error')
                        call exit(1)
                end select
                index = index + 1
        end do
    end function

    integer function get_num_from_str(string__)
        character(*) :: string__
        integer :: index__ = 0
        get_num_from_str = 0
        do while (index <= len_trim(input) .and. is_digit(input(index:index)))
            get_num_from_str = get_num_from_str * 10 + char_to_int(input(index:index))
            index = index + 1
            index__ = index__ + 1
        end do
    end function

    integer function char_to_int(ch__)
        character :: ch__
        char_to_int = (ichar(ch__) - ichar('0'))
    end function

    logical function is_digit(ch__)
        character :: ch__
        is_digit = ch__ >= '0' .and. ch__ <= '9'
    end function

    subroutine out(string__)
        character(*) :: string__
        write(2, *) string__
        call flush(2)
    end subroutine
end module calculator

program main
    use calculator
    implicit none
    character(100) :: input

    do 
        write(*, '(A)', advance='no') '> '
        read(*, '(A)') input
        print *, calculate(input)
    end do
end program main
