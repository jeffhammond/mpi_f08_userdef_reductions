module i

    abstract interface

    subroutine M_User_function(invec, inoutvec, len, datatype)
        use, intrinsic :: iso_c_binding, only : c_ptr
        use mpi_f08, only : MPI_Datatype
        implicit none
        !type(c_ptr), value :: invec, inoutvec
        type(*), dimension(..), intent(in) :: invec
        type(*), dimension(..), intent(inout) :: inoutvec
        integer, intent(in) :: len
        type(MPI_Datatype), intent(in) :: datatype
    end subroutine

    end interface

end module i

module m

    use mpi_f08
    use i

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

    use mpi_f08

    contains

    subroutine X_function(invec, inoutvec, len, datatype)
        use, intrinsic :: iso_c_binding, only : c_ptr
        use mpi_f08, only : MPI_Datatype
        implicit none
        type(*), dimension(..), intent(in) :: invec
        type(*), dimension(..), intent(inout) :: inoutvec
        !integer, dimension(:), intent(in) :: invec
        !integer, dimension(:), intent(inout) :: inoutvec
        integer, intent(in) :: len
        type(MPI_Datatype), intent(in) :: datatype
        print*,rank(invec),rank(inoutvec)
    end subroutine

end module x

program main
    use m
    use x
    procedure(M_User_function), pointer :: fp => NULL()
    fp => X_function
end program main
