module f
    interface
        subroutine get_cptr(x,y) bind(C)
            use, intrinsic :: iso_c_binding, only : c_ptr
            implicit none
            type(*), dimension(..), intent(in) :: x
            type(c_ptr), intent(out) :: y
        end subroutine
    end interface
end module f

module i
    abstract interface
        subroutine M_User_function(i, o, n, d)
            use, intrinsic :: iso_c_binding, only : c_ptr
            implicit none
            type(*), dimension(..), intent(in) :: i
            type(*), dimension(..), intent(inout) :: o
            integer, intent(in) :: n
            type(c_ptr), intent(in) :: d
        end subroutine
    end interface
end module i

module x
    contains
    subroutine X_function(i, o, n, d)
        use, intrinsic :: iso_c_binding, only : c_ptr, c_f_pointer, c_loc
        use f, only : get_cptr
        implicit none
        type(*), dimension(..), target, intent(in) :: i
        type(*), dimension(..), target, intent(inout) :: o
        integer, intent(in) :: n
        type(c_ptr), intent(in) :: d
        !
        type(c_ptr) :: cpi, cpo
        integer, dimension(:), pointer :: fpi, fpo
#if 0
        call get_cptr(i,cpi)
        call get_cptr(o,cpo)
        call c_f_pointer(cpi,fpi,[size(i)])
        call c_f_pointer(cpo,fpo,[size(o)])
#elif 0
        call c_f_pointer(i,fpi,[size(i)])
        call c_f_pointer(o,fpo,[size(o)])
#else
        cpi = c_loc(i)
        cpo = c_loc(o)
        call c_f_pointer(cpi,fpi,[size(i)])
        call c_f_pointer(cpo,fpo,[size(o)])
#endif
    end subroutine

end module x

program main
    use i
    use x
    implicit none
    procedure(M_User_function), pointer :: fp => NULL()
    fp => X_function
end program main
