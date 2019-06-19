module import_graph

    use iso_c_binding
    implicit none
    private

    public :: read_graph

contains

    logical function whitechar(char) ! white character
            ! returns .true. if char is space (32) or tab (9), .false. otherwise
            character, intent(in) :: char
            if (iachar(char) == 32 .or. iachar(char) == 9) then
                whitechar = .true.
            else
                whitechar = .false.
            end if
        end function

        integer function count_columns(unit,stat) result(ncol)
            integer, intent(in) :: unit
            integer, intent(out) :: stat
            
            character(len=1) :: c
            logical :: lastwhite

            ncol = 0
            lastwhite = .true.
            do
                read(unit,'(a)',advance='no',iostat=stat) c
                if (stat /= 0) exit
                if (lastwhite .and. .not. whitechar(c)) ncol = ncol + 1
                lastwhite = whitechar(c)
            end do
            backspace(unit,iostat=stat)
        end function

        subroutine read_graph(unit,xadj,adjncy,vwgt,adjwgt)
            implicit none
            integer, intent(in) :: unit
            integer, intent(out), allocatable :: xadj(:)
            integer, intent(out), allocatable :: adjncy(:)
            integer, intent(out), allocatable, optional :: vwgt(:)
            integer, intent(out), allocatable, optional :: adjwgt(:)

            character(len=1) :: c
            integer :: ncol, ios, i, rowcol, j
            logical :: lastwhite

            character(len=3) :: cfmt
            integer :: nvtxs, nedgs, ncon, fmt

            ! Determine number of columns in header line
            ncol = 0
            lastwhite = .true.
            do
                read(unit,'(a)',advance='no',iostat=ios) c
                ! if (iachar(c) == 37) then
                !     read(unit,*) ! skipline
                !     print *, "Skipped line"
                !     cycle
                ! end if
                if (ios /= 0) exit
                if (lastwhite .and. .not. whitechar(c)) ncol = ncol + 1
                lastwhite = whitechar(c)
            end do

            print *, "Number of columns in header = ", ncol

            rewind(unit)
            ! do
            !     read(unit,'(a)',iostat=ios) c
            !     if (iachar(c) == 37) then
            !         print *, "Skipped line"
            !         cycle
            !     else
            !         backspace(unit)
            !         exit
            !     end if
            ! end do    

            ! Parse values in header line
            ncon = 1
            cfmt = '000'
            select case(ncol)
            case(2)
                read(unit,*,iostat=ios) nvtxs, nedgs
            case(3)
                read(unit,*,iostat=ios) nvtxs, nedgs, cfmt
            case default
                write(*,*) "[load_graph]: incorrect file"
                error stop 1
            end select
            read(cfmt,'(b3.3)') fmt
            print *, nvtxs, nedgs, cfmt, ncon
            write(*,'(A,B3.3)') "fmt = ", fmt

            ! Allocate necessary space
            allocate(xadj(nvtxs+1))
            allocate(adjncy(2*nedgs))

            if (btest(fmt,0)) allocate(adjwgt(2*nedgs))  ! edge weights
            if (btest(fmt,1)) allocate(vwgt(nvtxs*ncon)) ! node weights

            xadj(1) = 0 ! start C style
            select case(fmt)
            case (b'000')
                do i = 1, nvtxs
                    rowcol = count_columns(unit,stat=ios)
                    xadj(i+1) = xadj(i) + rowcol
                    read(unit,*) adjncy(xadj(i)+1:xadj(i+1))
                end do
            case(b'001')
                do i = 1, nvtxs
                    rowcol = count_columns(unit,stat=ios)/2
                    xadj(i+1) = xadj(i) + rowcol
                    read(unit,*) (adjncy(j),adjwgt(j),j=xadj(i)+1,xadj(i+1))
                end do
            case(b'010')
                do i = 1, nvtxs
                    rowcol = count_columns(unit,stat=ios) - ncon
                    xadj(i+1) = xadj(i) + rowcol
                    read(unit,*) vwgt((i-1)*ncon+1:(i-1)*ncon+ncon), adjncy(xadj(i)+1:xadj(i+1))
                end do
            case(b'011')
                do i = 1, nvtxs
                    rowcol = (count_columns(unit,stat=ios) - ncon)/2
                    xadj(i+1) = xadj(i) + rowcol
                    read(unit,*) vwgt((i-1)*ncon+1:(i-1)*ncon+ncon), (adjncy(j),adjwgt(j),j=xadj(i)+1,xadj(i+1))
                end do
            case default
                print *, "[read_graph] should not be here"
                stop
            end select

            adjncy = adjncy - 1 ! convert to C indexing

    end subroutine

end module

program main

    use, intrinsic :: iso_c_binding, only: c_int, c_double, c_bool
    use kahip_interface, only: kaffpa, ECO, STRONG
    use import_graph, only: read_graph
    implicit none

    integer(c_int) :: n
    integer(c_int), allocatable :: xadj(:)
    integer(c_int), allocatable :: adjncy(:)
    integer(c_int), allocatable :: part(:)

    real(c_double) :: imbalance
    integer(c_int) :: edge_cut, nparts, mode
    logical(c_bool) :: output

    character(len=96) :: cmd
    integer :: unit


    if (command_argument_count() > 0) then
        call get_command_argument(1,cmd)
        open(newunit=unit,file=trim(cmd),action='read')
        call read_graph(unit,xadj=xadj,adjncy=adjncy)
        close(unit)
        n = size(xadj) - 1
        allocate(part(n))
        mode = STRONG
    else
        write(*,*) "partitioning graph from the manual"
        n = 5
        allocate(xadj(n+1))
        allocate(adjncy(12))
        allocate(part(n))

        xadj = [ integer(c_int) :: 0, 2, 5, 7, 9, 12]
        adjncy = [integer(c_int) :: 1, 4, 0, 2, 4, 1, 3, 2, 4, 0, 1, 3]
        mode = ECO
    end if


    imbalance = 0.03_c_double

    edge_cut = 0
    nparts = 2
    output = .false.
    call kaffpa(n,xadj=xadj,adjncy=adjncy,nparts=nparts,imbalance=imbalance, &
        suppress_output=.false._c_bool,seed=0_c_int,mode=STRONG,edgecut=edge_cut,part=part)

    write(*,*) "edge_cut ", edge_cut

end program
