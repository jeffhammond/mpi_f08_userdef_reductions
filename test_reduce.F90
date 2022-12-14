module i

    abstract interface

    subroutine M_User_function(invec, inoutvec, len, datatype)
        use, intrinsic :: iso_c_binding, only : c_ptr
        use mpi_f08, only : MPI_Datatype
        implicit none
        !type(c_ptr), value :: invec, inoutvec
        type(*), dimension(..), target, intent(in) :: invec
        type(*), dimension(..), target, intent(inout) :: inoutvec
        integer, intent(in) :: len
        type(MPI_Datatype), intent(in) :: datatype
    end subroutine

    end interface

end module i

module m

    contains

        subroutine M_Allreduce(sendbuf, recvbuf, count, datatype, op, comm, ierror)
            use mpi_f08, only : MPI_Datatype, MPI_Op, MPI_Comm
            TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
            TYPE(*), DIMENSION(..) :: recvbuf
            INTEGER, INTENT(IN) :: count
            TYPE(MPI_Datatype), INTENT(IN) :: datatype
            TYPE(MPI_Op), INTENT(IN) :: op
            TYPE(MPI_Comm), INTENT(IN) :: comm
            INTEGER, OPTIONAL, INTENT(OUT) :: ierror
        end subroutine M_Allreduce

end module m

module x

    contains

    subroutine X_function(invec, inoutvec, len, datatype)
        use, intrinsic :: iso_c_binding, only : c_ptr, c_loc, c_f_pointer
        use mpi_f08, only : MPI_Datatype
        implicit none
        type(*), dimension(..), target, intent(in) :: invec
        type(*), dimension(..), target, intent(inout) :: inoutvec
        integer, intent(in) :: len
        type(MPI_Datatype), intent(in) :: datatype
        type(c_ptr) :: cpi, cpo
        integer, dimension(:), pointer :: fpi, fpo
        cpi = c_loc(invec)
        cpo = c_loc(inoutvec)
        call c_f_pointer(cpi,fpi,[size(invec)])
        call c_f_pointer(cpo,fpo,[size(inoutvec)])
        fpo = fpo + fpi
    end subroutine

    subroutine Y_function(invec, inoutvec, len, datatype)
        use mpi_f08, only : MPI_Datatype
        implicit none
        real, dimension(:), target, intent(in) :: invec
        real, dimension(:), target, intent(inout) :: inoutvec
        integer, intent(in) :: len
        type(MPI_Datatype), intent(in) :: datatype
        inoutvec = inoutvec +  invec
    end subroutine

end module x

program main
    use mpi_f08, only : MPI_INT
    use i, only : M_User_function
    use x, only : X_function, Y_function
    integer, dimension(:), allocatable, target :: fpi, fpo
    procedure(M_User_function), pointer :: fp => NULL()
    allocate( fpi(10), fpo(10) )
    fp => Y_function
    print*,'LOC: ',loc(fpi),loc(fpo)
    call fp( fpi, fpo, 10, MPI_INT )
end program main
