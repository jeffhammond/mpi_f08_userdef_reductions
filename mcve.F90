module f
    interface
        subroutine get_cptr(x,y) bind(C)
            use, intrinsic :: iso_c_binding, only : c_ptr
            type(*), dimension(..), intent(in) :: x
            type(c_ptr), intent(out) :: y
        end subroutine
    end interface
end module f

module i
    abstract interface
        subroutine M_User_function(invec, inoutvec, len, datatype)
            use, intrinsic :: iso_c_binding, only : c_ptr
            implicit none
            type(*), dimension(..), intent(in) :: invec
            type(*), dimension(..), intent(inout) :: inoutvec
            integer, intent(in) :: len
            type(c_ptr), intent(in) :: datatype
        end subroutine
    end interface
end module i

module m
    use i
    contains
        subroutine M_Allreduce(sendbuf, recvbuf, count, datatype, ierror)
            use, intrinsic :: iso_c_binding, only : c_ptr
            TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
            TYPE(*), DIMENSION(..) :: recvbuf
            INTEGER, INTENT(IN) :: count
            TYPE(c_ptr), INTENT(IN) :: datatype
            INTEGER, OPTIONAL, INTENT(OUT) :: ierror
        end subroutine M_Allreduce
end module m

module x
    contains
    subroutine X_function(invec, inoutvec, len, datatype)
        use, intrinsic :: iso_c_binding, only : c_ptr, c_f_pointer
        use f, only : get_cptr
        implicit none
        type(*), dimension(..), intent(in) :: invec
        type(*), dimension(..), intent(inout) :: inoutvec
        integer, intent(in) :: len
        type(c_ptr), intent(in) :: datatype
        !
        type(c_ptr) :: cpi, cpo
        integer, dimension(:), pointer :: fpi, fpo
        call get_cptr(invec,cpi)
        call get_cptr(inoutvec,cpo)
        call c_f_pointer(cpi,fpi,[size(invec)])
        call c_f_pointer(cpo,fpo,[size(inoutvec)])
    end subroutine

end module x

program main
    use m
    use x
    procedure(M_User_function), pointer :: fp => NULL()
    fp => X_function
end program main
