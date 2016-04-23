c Help
c        call ftc_sphere (d, iw, jw, cxt, cy, cxut, cyu, a, ruff
c     &,                imt, jmt, 2, imt-1, 1, jmt
c     &,                xt, yt, xu, yu, 1, work, lenw, 0, isigma)

c=======================================================================

      subroutine ftc_sphere (f, if1, jf, xf, yf, sxf, syf, c, sigma_c
     &,                      ic, jc, istart, iend, jstart, jend
     &,                      xc, yc, sxc, syc, init, work, lenw
     &,                      isc_vc, isigma_c)
c
c=======================================================================
c
c     "ftc" is an acronym for "fine to coarse".
c
c     obtain a coarse grid representation of a fine grid dataset by area
c     averaging grid box values on the fine grid which overlay coarse 
c     grid boxes over a sphere. note: the coarse grid boxes do not have 
c     to contain an integral number of fine grid boxes. coordinates are 
c     assumed to be longitude and latitude on a sphere. the routine has
c     been tested only in very simple cases, so watch for bugs. the way
c     the routine works is very simple, and is based on the idea area
c     of a surface bounded by two parallels and two meridians on a sphere
c     is proportional to the zonal separation of the meridians (given
c     in degrees, for example) times the meridional separation of the
c     two parallels given as the difference between the sines of the 
c     latitudes of both parallels. this is why the coordinates of the
c     "edges" of a grid box, sxf, syf, and sxc, syc are needed.
c
c
c     inputs:
c
c     f        = data on fine grid
c     if1      = inner dimension of "f"
c     jf       = outer dimension of "f"
c     xf       = coordinates for inner dimension of "f" (id: longitudes)
c     yf       = coordinates for outer dimension of "f" (id: latitudes)
c     sxf      = coordinates for edges of fine grid cells in longitude
c     syf      = coordinates for edges of fine grid cells in latitude
c
c     ic       = inner dimension of coarse grid "c"
c     jc       = outer dimension of coarse grid "c"
c     istart   = starting index along inner dimension of "c" for which
c                averaged values are desired
c     iend     = ending index along inner dimension of "c" for which
c                averaged values are desired
c     jstart   = starting index along outer dimension of "c" for which
c                averaged values are desired
c     jend     = ending index along outer dimension of "c" for which
c                averaged values are desired
c     xc       = coordinates for inner dimension of "c" (id: longitudes)
c     yc       = coordinates for outer dimension of "c" (id: latitudes)
c     sxc      = coordinates for edges of coarse grid cells in longitude
c     syc      = coordinates for edges of coarse grid cells in latitude
c     init     = initialize the averaging factors
c                "init" should be set = 1 on the first call. 
c                "init" <> 1 uses the previously computed factors stored
c                in "work" array.
c     work     = work array of averaging factors when "init" <> 1
c                (previously calculated by "ftc" when "init" = 1)
c     lenw     = size of work array. lenw should be >= 9*max(if1,jf)
c     isc_vc   = 0/non-0 (scalar/vector field)
c     isigma_c = 1/non-1 (compute/do not compute standard deviation of c)
c       
c     output:
c
c     c       = coarse grid average of "f" defined over
c               ((c(i,j),i=istart,iend),j=jstart,jend)
c     sigma_c = standard deviation of c
c     work    = work array of averaging factors when "init" = 1
c
c     restrictions:
c
c     fine and coarse grids are assumed spherical with "xf", "xc",
c     "sxf", and "sxc" in degrees. "yf", "yc", "syf", and "syc" must 
c     also be in degrees. the coarse domain xc(istart)...xc(iend) must 
c     be within the fine domain xf(1)...xf(if1). 
c     similarly, yc(jstart)...yc(jend) must be within yf(1)...yf(jf). 
c     all coordinates must be strictly monotonically increasing.
c     a distinction is made between scalar and vector fields. the edge 
c     coordinates of a scalar field are the grid-centre coordinates of 
c     a vector field, and viceversa...
c 
c     author:      m. a. m. maqueda      e-mail=> maqueda@pik-potsdam.de 
c     (based on the original routine ftc by r. c. pacanowski. following 
c     a remark from Marisa Montoya, montoya@pik-potsdam.de, it became 
c     apparent that the original routine was actually not apt for 
c     area-weighted interpolation over a sphere)
c=======================================================================
c
!====================== include file "stdunits.h" ======================
!
!     stdin  = unit number for standard input.
!     stdout = unit number for standard output.
!     stderr = unit number for standard error.
!
      integer stdin, stdout, stderr
      parameter (stdin = 5, stderr = 0)
      common /stdunit/ stdout

      logical error, show_coord
      parameter (len=10000, p5=0.5, c0=0.0)
      dimension iso(0:len), ieo(0:len), jso(0:len), jeo(0:len)
     &,         dx(0:len,2), dy(0:len,2), edgecx(0:len), edgecy(0:len)
     &,         edgefx(0:len), edgefy(0:len)
      dimension f(if1,jf), xf(if1), yf(jf), sxf(if1), syf(jf)
      dimension c(ic,jc), sigma_c(ic,jc)
      dimension xc(ic), yc(jc), sxc(ic), syc(jc)
      dimension work(lenw)
c
        write (stdout,*) ' '
        write (stdout,*)
     & '            Averaging data from "fine" to "coarse" grid'
c
c-----------------------------------------------------------------------
c     initialize weights or use previously calculated weights
c-----------------------------------------------------------------------
c
      if (init .eq. 1) then
c
c       cdr_l, conversion factor from degrees to radians
c
        cdr_l = acos(-1.0)/180.0
        error = .false.
        write (stdout,*)
     & '              (initializing the averaging weights)'
        write (stdout,*) ' '
c
c       test to verify that array sizes do not exceed limits
c
        if (if1 .gt. len .or. jf .gt. len) then
          i = max(if1,jf)
          write (stdout,*) '=>Error: increase "len" in "ftc" to ',i
          call abort()
        endif
        if (lenw .lt. 9*max(if1,jf)) then
          write (stdout,*) '=>Error: increase size of "work" array',
     &      ' to at least ',9*max(if1,jf),' for calls to "ftc"'
          error = .true.
        endif
c
c       verify that the "coarse" grid lies within the "fine" grid
c
        if (xf(1) .gt. xc(istart) .or. xf(if1) .lt. xc(iend)) then
          write (stdout,*)
     &     '=>Error: Coarse grid "xc" is outside "fine" grid "xf".'
          if (xf(1) .gt. xc(istart)) then
            write (stdout,*) '  xc(',istart,')  .lt.  xf(1)'  
          endif
          if (xf(if1) .lt. xc(iend)) then
            write (stdout,*) '  xc(',iend,')  .gt.  xf(',if1,')'  
          endif
	  stop 'in ftc'
        endif 
        if (yf(1) .gt. yc(jstart) .or. yf(jf) .lt. yc(jend)) then
          write (stdout,*)
     &     '=>Error: Coarse grid "yc" is outside "fine" grid "yf".'
          if (yf(1) .gt. yc(jstart)) then
            write (stdout,*) '  yc(',jstart,') .lt. yf(1)' 
          endif
          if (yf(jf) .lt. yc(jend)) then
            write (stdout,*) '  yc(',jend,')  .gt.  yf(',jf,')'  
          endif
	  stop 'in ftc'
        endif
c
c       construct edges of "coarse" grid boxes assuming
c       MOM's convention for the indexing of scalar
c       and vector grids.
c       edgefx and edgecx are in degrees, say, while
c       edgefy and edgecy are initially in degrees, but
c       are converted into sines of latitude, which 
c       allows for a proper area-averaged interpolation
c       over a sphere
c
        if ( isc_vc .eq. 0 ) then
          edgecx(0) = sxc(1) - (sxc(2) - sxc(1))
          do i=1,ic 
            edgecx(i) = sxc(i)
          enddo
        else
          do i=0,ic-1
            edgecx(i) = sxc(i+1)
          enddo
          edgecx(ic) = sxc(ic) + (sxc(ic) - sxc(ic-1))
        endif
c
        if ( isc_vc .eq. 0 ) then
          edgecy(0) = sin(cdr_l*(syc(1) - (syc(2) - syc(1))))
          do j=1,jc
            edgecy(j) = sin(cdr_l*syc(j))
          enddo
        else
          do j=0,jc-1
            edgecy(j) = sin(cdr_l*syc(j+1))
          enddo
          edgecy(jc) = sin(cdr_l*(syc(jc) + (syc(jc) - syc(jc-1))))
        endif
c
c       boundary points might not be monotonic, so make sure they are
c
        if (edgecy(0).gt.edgecy(1)) edgecy(0) = edgecy(1)
        if (edgecy(jc).lt.edgecy(jc-1)) edgecy(jc) = edgecy(jc-1)
c
c       construct edges of "fine" grid boxes
c
        if ( isc_vc .eq. 0 ) then
          edgefx(0) = sxf(1) - (sxf(2) - sxf(1))
          do i=1,if1
            edgefx(i) = sxf(i)
          enddo
        else
          do i=0,if1-1
            edgefx(i) = sxf(i+1)
          enddo
          edgefx(if1) = sxf(if1) + (sxf(if1) - sxf(if1-1))
        endif
c
        if ( isc_vc .eq. 0 ) then
          edgefy(0) = sin(cdr_l*(syf(1) - (syf(2) - syf(1))))
          do j=1,jf
            edgefy(j) = sin(cdr_l*syf(j))
          enddo
        else
          do j=0,jf-1
            edgefy(j) = sin(cdr_l*syf(j+1))
          enddo
          edgefy(jf) = sin(cdr_l*(syf(jf) + (syf(jf) - syf(jf-1))))
        endif
c
c       boundary points might not be monotonic, so make sure they are
c
        if (edgefy(0).gt.edgefy(1)) edgefy(0) = edgefy(1)
        if (edgefy(jf).lt.edgefy(jf-1)) edgefy(jf) = edgefy(jf-1)
c
c       calculate "dx" and "dy" for the "fine" grid boxes
c
        do i=1,if1
          dx(i,1) = edgefx(i) - edgefx(i-1)       
          dx(i,2) = dx(i,1)
        enddo
        dx(0,1) = dx(1,1)
        dx(0,2) = dx(1,2)
c
        do j=1,jf
          dy(j,1) = edgefy(j) - edgefy(j-1)
          dy(j,2) = dy(j,1)
        enddo
        dy(0,1) = dy(1,1)
        dy(0,2) = dy(1,2)
c
c       modify "dx" and "dy" for possibly partial "fine" grid boxes 
c       near the edges of each coarse grid box.
c       "ii" is the index of the fine grid box which contains the
c       eastern edge of coarse grid box with index "i".
c       dx(ii,1) is the portion of the fine grid box to the west of the
c       edge and dx(ii,2) is the portion to the east. similarly,
c       dy(jj,1) is to the south and dy(jj,2) is to the north of the
c       northern edge of coarse box with index "j".
c        
c       note: edgefx and edgefy are zero based and need the -1 when
c       using "indp" 
c
        do i=0,ic
          ii   = indp (edgecx(i), edgefx, if1+1) - 1
          frac = abs(edgecx(i) - edgefx(ii))
          if (edgefx(ii) .lt. edgecx(i)) then
            ii = ii + 1
            dx(ii,2) = (edgefx(min(if1,ii)) - edgefx(ii-1)) - frac
            dx(ii,1) = frac
          else
            dx(ii,2) = frac
            dx(ii,1) = (edgefx(ii) - edgefx(max(ii-1,0))) - frac
          endif
          ieo(i) = min(if1,max(1,ii))
        enddo
        do i=1,ic
          iso(i) = max(1,ieo(i-1))
        enddo
        iso(0) = ieo(0)
c
        do j=0,jc
          jj   = indp (edgecy(j), edgefy, jf+1) - 1
          frac = abs(edgecy(j) - edgefy(jj))
          if (edgefy(jj) .lt. edgecy(j)) then
            jj = jj + 1
            dy(jj,2) = (edgefy(min(jf,jj)) - edgefy(jj-1)) - frac
            dy(jj,1) = frac
          else
            dy(jj,2) = frac
            dy(jj,1) = (edgefy(jj) - edgefy(max(jj-1,0))) - frac
          endif
          jeo(j) = min(jf,max(1,jj))
        enddo
        do j=1,jc
          jso(j) = max(1,jeo(j-1))
        enddo
        jso(0) = jeo(0)
c
c       store the weights into the "work" array
c
        indx = 1
        do j=0,jc
          work(indx)   = jso(j)
          work(indx+1) = jeo(j)
          indx         = indx + 2
        enddo
c
        do i=0,ic
          work(indx)   = iso(i)
          work(indx+1) = ieo(i)
          indx         = indx + 2
        enddo
c
        do j=0,jf
          work(indx)   = dy(j,1)
          work(indx+1) = dy(j,2)
          indx         = indx + 2
        enddo
c
        do i=0,if1
          work(indx)   = dx(i,1)
          work(indx+1) = dx(i,2)
          indx         = indx + 2
        enddo
c
c       verify that coarse grid is coarser than the fine grid
c
        do j=jstart,jend
          if ((jso(j) .eq. jso(j+1)) .and. (yc(j) .ge. yf(1))
     &      .and. (yc(j) .le. yf(jf))) then
            write (stdout,*)
     &         '=>Warning: "Coarse" grid is finer than "fine" grid'
     &,     ' near yf(',jso(j),') =',yf(jso(j))
     &,     ' (average may not be accurate)'
          endif
        enddo
c
        do i=istart,iend
          if ((iso(i) .eq. iso(i+1)) .and. (xc(i) .ge. xf(1))
     &      .and. (xc(i) .le. xf(if1))) then
            write (stdout,*)
     &      '=>Warning: "Coarse" grid is finer than "fine" grid'
     &,     ' near xf(',iso(i),') = ',xf(iso(i))
     &,     ' (average may not be accurate)'
          endif
        enddo

        show_coord = .true.
        write (stdout,'(/a/)')
     &   'Use -Dskip_interp_details to not show the following'

        if (error .or. show_coord) then
          write (stdout,*)
     & ' Indices for averaging fine grid to coarse grid:'
          write (stdout,*)
     & ' (fractional grid boxes are accounted for)'
          write (stdout,8700)
          write (stdout,9000) (m,iso(m),ieo(m),m=istart,iend)
          write (stdout,*) ' '
          write (stdout,*) ' Coordinates for coarse grid points "xc" ='
          write (stdout,8500) xc
          write (stdout,*) ' Coordinates for fine grid points "xf" ='
          write (stdout,8500) xf
c
          write (stdout,8800)
          write (stdout,9000) (m,jso(m),jeo(m),m=jstart,jend)
          write (stdout,*) ' '
          write (stdout,*) ' Coordinates for coarse grid points "yc" ='
          write (stdout,8500) yc
          write (stdout,*) ' Coordinates for fine grid points "yf" ='
          write (stdout,8500) yf
        endif
        if (error) call abort()
      else
        write (stdout,*)
     & '              (using previously initialized averaging weights)'
        write (stdout,*) ' '
c
c       extract the weights from the "work" array
c
        indx = 1
        do j=0,jc
          jso(j) = nint(work(indx))   
          jeo(j) = nint(work(indx+1))
          indx   = indx + 2
        enddo
c
        do i=0,ic
          iso(i) = nint(work(indx))
          ieo(i) = nint(work(indx+1))
          indx   = indx + 2
        enddo
c
        do j=0,jf
          dy(j,1) = work(indx)
          dy(j,2) = work(indx+1)
          indx    = indx + 2
        enddo
c
        do i=0,if1
          dx(i,1) = work(indx)
          dx(i,2) = work(indx+1)
          indx    = indx + 2
        enddo
      endif
c
c-----------------------------------------------------------------------
c     average the "fine" grid to the "coarse" grid
c-----------------------------------------------------------------------
c
      do m=jstart,jend
        do i=istart,iend
          weight = c0
          sum    = c0
          do j=jso(m),jeo(m)
            indy = 2
            if (j .eq. jeo(m)) indy = 1
            wty = dy(j,indy)
            do ii=iso(i),ieo(i)
              indx = 2
              if (ii .eq. ieo(i)) indx = 1
              area   = dx(ii,indx)*wty
              weight = weight + area
              sum    = sum + f(ii,j)*area
            enddo
          enddo
          c(i,m) = sum/max(1.0e-20,weight)
        enddo
      enddo
      if (isigma_c .eq. 1) then
        do m=jstart,jend
          do i=istart,iend
            weight = c0
            sum    = c0
            do j=jso(m),jeo(m)
              indy = 2
              if (j .eq. jeo(m)) indy = 1
              wty = dy(j,indy)
              do ii=iso(i),ieo(i)
                indx = 2
                if (ii .eq. ieo(i)) indx = 1
                area   = dx(ii,indx)*wty
                weight = weight + area
                sum    = sum + (f(ii,j)-c(i,m))**2*area
              enddo
            enddo
            sigma_c(i,m) = sqrt(sum/max(1.0e-20,weight))
          enddo
        enddo
      endif
      return
8500  format (1x,10g11.4)
8700  format (/' Along the 1st dimension, the form is (Coarse grid'
     &,' point "xc": range of fine grid points "xf" to average)'/)
8800  format (/' Along the 2nd dimension, the form is (Coarse grid'
     &,' point "yc": range of fine grid points "yf" to average)'/)
9000  format (5(1x,'(',i4,': ',i4,' to ',i4,')'),/)
      end

      function indp (value, array, ia)
c
c=======================================================================
c
c     indp = index of nearest data point within "array" corresponding to
c            "value".
c
c     inputs:
c
c     value  = arbitrary data...same units as elements in "array"
c     array  = array of data points  (must be monotonically increasing)
c     ia     = dimension of "array"
c
c     output:
c
c     indp =  index of nearest data point to "value"
c             if "value" is outside the domain of "array" then indp = 1
c             or "ia" depending on whether array(1) or array(ia) is
c             closest to "value"
c
c             note: if "array" is dimensioned array(0:ia) in the calling
c                   program, then the returned index should be reduced
c                   by one to account for the zero base.
c
c     author:      r. c. pacanowski      e-mail=> rcp@gfdl.gov
c
c     example:
c
c     let model depths be defined by the following:
c     parameter (km=5)
c     dimension z(km)
c     data z /5.0, 10.0, 50.0, 100.0, 250.0/
c
c     k1 = indp (12.5, z, km)
c     k2 = indp (0.0, z, km)
c
c     k1 would be set to 2, & k2 would be set to 1 so that
c     z(k1) would be the nearest data point to 12.5 and z(k2) would
c     be the nearest data point to 0.0
c
c=======================================================================
c
!====================== include file "stdunits.h" ======================
!
!     stdin  = unit number for standard input.
!     stdout = unit number for standard output.
!     stderr = unit number for standard error.
!
      integer stdin, stdout, stderr
      parameter (stdin = 5, stderr = 0)
      common /stdunit/ stdout

      dimension array(ia)
c
      logical keep_going
c
      do i=2,ia
        if (array(i) .lt. array(i-1)) then
          write (stdout,*)
     &   ' => Error: array must be monotonically increasing in "indp"' 
     &,  '           when searching for nearest element to value=',value
          write (stdout,*) '           array(i) < array(i-1) for i=',i 
          write (stdout,*) '           array(i) for i=1..ia follows:'
          do ii=1,ia
            write (stdout,*) 'i=',ii, ' array(i)=',array(ii)
          enddo
          call abort()
        endif
      enddo
      if (value .lt. array(1) .or. value .gt. array(ia)) then
        if (value .lt. array(1))  indp = 1
        if (value .gt. array(ia)) indp = ia
      else
        i=1
        keep_going = .true.
        do while (i .le. ia .and. keep_going)
          i = i+1
          if (value .le. array(i)) then
            indp = i
            if (array(i)-value .gt. value-array(i-1)) indp = i-1
            keep_going = .false.
          endif
        enddo
      endif
      return
      end
