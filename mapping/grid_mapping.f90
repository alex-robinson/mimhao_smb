
program grid_mapping 

    use coordinates 
    use planet 
    use oblimap_projection_module 
    use ncio 
    
    implicit none 

    type(grid_class)  :: grid0          ! Original grid information
    type(grid_class)  :: grid           ! New grid 
    character(len=32) :: varname        ! Name of the variable being mapped
    double precision, allocatable :: var0_full(:,:) ! Input variable (full resolution)
    double precision, allocatable :: var0(:,:)      ! Input variable
    double precision, allocatable :: var(:,:)       ! Output variable
    double precision, parameter  :: missing_value = -9999.d0  ! Points not included in mapping

    character(len=256) :: grid_name, outfldr 
    character(len=512) :: filename_in, filename_out

    ! =========================================================
    !
    ! USER DEFINITIONS
    !
    ! =========================================================
 
    grid_name = "ANT-40KM"
    outfldr   = "./"

    filename_in  = "../../Ant_MeltingRate.nc"
    filename_out = trim(outfldr)//trim(grid_name)//"_BMELT-R13_conservative.nc"

    ! =========================================================
    !
    ! OUTPUT GRID DEFINITION
    !
    ! =========================================================

    select case(trim(grid_name))
        case("ANT-40KM")
            call grid_init(grid,name="ANT-40KM",mtype="stereographic",units="kilometers", &
                           lon180=.TRUE.,dx=40.d0,nx=141,dy=40.d0,ny=141, &
                           lambda=0.d0,phi=-90.d0,alpha=19.0d0)

        case("ANT-20KM")
            call grid_init(grid,name="ANT-20KM",mtype="stereographic",units="kilometers", &
                           lon180=.TRUE.,dx=20.d0,nx=281,dy=20.d0,ny=281, &
                           lambda=0.d0,phi=-90.d0,alpha=19.0d0)

        case DEFAULT
            write(*,*) "gentopo:: error: grid name not recognized: "//trim(grid_name)
            stop 

    end select

    ! Define the input grid (eg, BEDMAP2/rignot)
    ! (for now defined at 10KM resolution for testing to go faster)
    call grid_init(grid0,name="rignot-10KM",mtype="polar_stereographic",units="kilometers",lon180=.TRUE., &
                   x0=-2800.d0,dx=10.d0,nx=561,y0=-2800.d0,dy=10.d0,ny=561, &
                   lambda=0.d0,phi=-90.d0,alpha=19.0d0)

    ! Allocate the input and output variable arrays 
    call grid_allocate(grid0,var0)
    call grid_allocate(grid,var)

    ! Load the input data field from the file 
    allocate(var0_full(5601,5601))
    call nc_read(filename_in,"melt_actual",var0_full,missing_value=missing_value)
    
    ! Get a 'thinned' version of the full resolution variable, eg 10km instead of 1km resolution 
    ! (to increase speed of program for testing)
    call thin(var0,var0_full,by=10)

    ! Flip the rows because they were written in reverse in the input file! 
    call flip_y(var0)

    ! Check that the variable has an interesting range of values
    write(*,*) "range var0: ", minval(var0), maxval(var0)

    ! Perform the interpolation
    call map_field_conservative(grid0,grid,"bm_actual",var0,var,missing_value)

    ! Perform some checks to see if conservative mapping worked as expected

    ! == TO DO == 


    ! Create the output file
    call nc_create(filename_out)
    call nc_write_dim(filename_out,"xc",x=grid%G%x,units="kilometers")
    call nc_write_dim(filename_out,"yc",x=grid%G%y,units="kilometers")

    ! Write the standard grid information (x2D,y2D,lon2D,lat2D,area,etc)
    call grid_write(grid,filename_out,xnm="xc",ynm="yc",create=.FALSE.)
    
    ! Write the interpolated field to the file 
    call nc_write(filename_out,"bm_actual",var,dim1="xc",dim2="yc",missing_value=missing_value)

    ! Done 
    write(*,*) "Conservative mapping program finished!"
    write(*,*) 

contains 

    subroutine map_field_conservative(grid0,grid,varname,var0,var,missing_value)

        implicit none 

        type(grid_class), intent(IN)  :: grid0  ! Original grid information
        type(grid_class), intent(IN)  :: grid   ! New grid 
        character(len=*), intent(IN)  :: varname        ! Name of the variable being mapped
        double precision, intent(IN)  :: var0(:,:)      ! Input variable
        double precision, intent(OUT) :: var(:,:)       ! Output variable
        double precision, intent(IN)  :: missing_value  ! Points not included in mapping

        double precision :: x1, y1, x2, y2 
        integer :: i, j 

        ! Helpful fields available from grid_class objects grid and grid0:
        ! grid%x    : 2D array of projected x-values [km]
        ! grid%y    : 2D array of projected y-values [km]
        ! grid%lon  : 2D array of lon-values [degrees]
        ! grid%lat  : 2D array of lat-values [degrees]
        ! grid%area : 2D array of grid-cell areas [m]
        ! grid%G%x  : vector of x-values that defines the x-axis [km]
        ! grid%G%y  : vector of y-values that defines the y-axis [km]
        ! grid%G%nx : length of x-axis 
        ! grid%G%ny : length of y-axis 

        ! Check if the grids are compatible for this mapping routine
        if (.not. same_projection(grid0%proj,grid%proj)) then 
            write(*,*) "map_field_conservative:: error:  &
                       &Currently this subroutine only interpolates between grids &
                       &on the same projection. Try again."
            stop 
        end if 

        ! From the coordinates package, there are some distance calculation routines
        ! already available:
        write(*,*) "Calculating distance..."

        x1 = grid0%G%x(1)
        y1 = grid0%G%y(1)
        x2 =  grid%G%x(5)
        y2 =  grid%G%y(5)
        write(*,"(a,2f10.2,a,2f10.2,a,f10.2)") "Distance [km]: ",x1, y1, " => ", x2, y2, " = ", &
                                               cartesian_distance(x1,y1,x2,y2)
        write(*,*) 

        ! A loop to get started 
        do j = 1, grid%G%ny 
            do i = 1, grid%G%nx 

                ! == TO DO == 

                var(i,j) = -10.d0   ! Just a fill value now for the example

            end do 
        end do 

        return 

    end subroutine map_field_conservative 

    ! === HELPER SUBROUTINES ===

        
    subroutine thin(var1,var,by)
        ! Extract a thinner version of an input array
        ! (new array should be a multiple of input array)

        implicit none

        double precision, dimension(:,:) :: var, var1 
        integer :: by 
        integer :: i,j, nx, ny 
        integer :: i1, j1

        nx = size(var,1)
        ny = size(var,2) 

        var1 = missing_value 

        i1 = 0
        do i = 1, nx, by 
            i1 = i1+1 
            j1 = 0 
            do j = 1, ny, by  
                j1 = j1 + 1 
                if (i1 .le. size(var1,1) .and. j1 .le. size(var1,2)) &
                    var1(i1,j1) = var(i,j)
            end do 
        end do 

        return
    end subroutine thin 

    subroutine flip_y(var)
        ! Flip the y-axis 

        implicit none 
        double precision :: var(:,:)
        integer :: i, ny 
        double precision :: tmp(size(var,1),size(var,2))

        ny = size(var,2)
        
        tmp = var 

        do i = 1, ny 
            var(:,i) = tmp(:,ny-i+1)
        end do 

        return 

    end subroutine flip_y 

end program 
