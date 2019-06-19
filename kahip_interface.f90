module kahip_interface

    use, intrinsic :: iso_c_binding, only: c_int, c_double, c_bool, c_ptr
    implicit none
    private

    public :: kaffpa
    public :: kaffpa_balance_NE
    public :: node_separator

    integer(c_int), parameter, public :: FAST           = 0;
    integer(c_int), parameter, public :: ECO            = 1;
    integer(c_int), parameter, public :: STRONG         = 2;
    integer(c_int), parameter, public :: FASTSOCIAL     = 3;
    integer(c_int), parameter, public :: ECOSOCIAL      = 4;
    integer(c_int), parameter, public :: STRONGSOCIAL   = 5;

    interface

        ! same data structures as in metis 
        ! edgecut and part are output parameters
        ! part has to be an array of n ints

        ! void kaffpa(int* n, int* vwgt, int* xadj, 
        !                    int* adjcwgt, int* adjncy, int* nparts, 
        !                    double* imbalance,  bool suppress_output, int seed, int mode, 
        !                    int* edgecut, int* part);
        subroutine kaffpa(n,vwgt,xadj,adjcwgt,adjncy,nparts,imbalance,suppress_output,&
            seed,mode,edgecut,part) bind(C,name="kaffpa")
            import c_int, c_double, c_bool
            integer(c_int), intent(in) :: n
            integer(c_int), intent(in), optional :: vwgt(n)
            integer(c_int), intent(in) :: xadj(n+1)
            integer(c_int), intent(in), optional :: adjcwgt(*)
            integer(c_int), intent(in) :: adjncy(*)
            integer(c_int), intent(in) :: nparts
            real(c_double), intent(in) :: imbalance
            logical(c_bool), intent(in), value :: suppress_output
            integer(c_int), intent(in), value :: seed
            integer(c_int), intent(in), value :: mode
            integer(c_int), intent(out) :: edgecut
            integer(c_int), intent(out) :: part(n)
        end subroutine

        ! balance constraint on nodes and edges
        ! void kaffpa_balance_NE(int* n, int* vwgt, int* xadj, 
        !                 int* adjcwgt, int* adjncy, int* nparts, 
        !                 double* imbalance,  bool suppress_output, int seed, int mode,
        !                 int* edgecut, int* part);
        subroutine kaffpa_balance_NE(n,vwgt,xadj,adjcwgt,adjncy,nparts,imbalance,suppress_output,&
            seed,mode,edgecut,part) bind(C,name="kaffpa_balance_NE")
            import c_int, c_double, c_bool
            integer(c_int), intent(in) :: n
            integer(c_int), intent(in), optional :: vwgt(n)
            integer(c_int), intent(in) :: xadj(n+1)
            integer(c_int), intent(in), optional :: adjcwgt(*)
            integer(c_int), intent(in) :: adjncy(*)
            integer(c_int), intent(in) :: nparts
            real(c_double), intent(in) :: imbalance
            logical(c_bool), intent(in), value :: suppress_output
            integer(c_int), intent(in), value :: seed
            integer(c_int), intent(in), value :: mode
            integer(c_int), intent(out) :: edgecut
            integer(c_int), intent(out) :: part(n)
        end subroutine

        ! void node_separator(int* n, int* vwgt, int* xadj, 
        !                     int* adjcwgt, int* adjncy, int* nparts, 
        !                     double* imbalance,  bool suppress_output, int seed, int mode,
        !                     int* num_separator_vertices, int** separator); 
        subroutine node_separator(n,vwgt,xadj,adjcwgt,adjncy,nparts,imbalance,suppress_output,&
            seed,mode,num_separator_vertices,separator) bind(C,name="node_separator")
            import c_int, c_double, c_bool, c_ptr
            integer(c_int), intent(in) :: n
            integer(c_int), intent(in), optional :: vwgt(n)
            integer(c_int), intent(in) :: xadj(n+1)
            integer(c_int), intent(in), optional :: adjcwgt(*)
            integer(c_int), intent(in) :: adjncy(*)
            integer(c_int), intent(in) :: nparts
            real(c_double), intent(in) :: imbalance
            logical(c_bool), intent(in), value :: suppress_output
            integer(c_int), intent(in), value :: seed
            integer(c_int), intent(in), value :: mode
            integer(c_int), intent(out) :: num_separator_vertices
            type(c_ptr), intent(out) :: separator ! int** 
        end subroutine

    end interface
    
end module