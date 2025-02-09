  program read

  implicit none

  integer                        :: ndsl, ipts, iloop, ierr, npts
  character(len=256)             :: tmpline, test
  character(len=1)               :: comstr
  character(len=10)              :: pn
  character(len=100)              :: chead
  integer                        :: id
  real                           :: xx,yy,dpt
  character(len=40), allocatable :: pnames(:) !< @public point names
  real, allocatable              :: x(:)      !< @public x locations for point output
  real, allocatable              :: y(:)      !< @public y locations for point output
  real, allocatable              :: dw(:)     !< @public depth for point output
  COMSTR = "$"


  open (newunit=ndsl, file='ww3_points.list', &
       form='formatted', status='old', iostat=ierr)

  ! first loop to count the number of points
  ! second loop to allocate the array and store the points
  ipts = 0

  do ipts = 1,240
     read (ndsl,'(2f12.3,a)',iostat=ierr) xx,yy,chead
     read(chead(2:8),*)id
     read(chead(35:40),*)dpt
     print *,xx,yy,id,dpt
     !print *,xx,yy,trim(chead(2:8)),trim(chead(35:40))
  end do

#ifdef test
  do iloop=1,2
     rewind (ndsl)

     if ( iloop.eq.2) then
        npts = ipts
        if ( npts.gt.0 ) then
           allocate ( x(npts), y(npts), pnames(npts) )
           ipts = 0 ! reset counter to be reused for next do loop
        else
           allocate ( x(1), y(1), pnames(1) )
           goto 2054
        end if
     end if

     do
        read (ndsl,'(a)',iostat=ierr) tmpline
        !print *,trim(tmpline)
        ! if end of file or stopstring, then exit
        if ( ierr.ne.0 .or. index(tmpline,"STOPSTRING").ne.0 ) exit

        ! leading blanks removed and placed on the right
        !test = adjustl ( tmpline )
        !print *,trim(tmpline),'  X  ',trim(test)
        !if ( test(1:1).eq.comstr .or. len_trim(test).eq.0 ) then
           ! if comment or blank line, then skip
         !  cycle
        !else
           ! otherwise, backup to beginning of line
           backspace ( ndsl, iostat=ierr)
           read (ndsl,*,iostat=ierr) xx, yy, chead
           print *,xx,yy,'  ',trim(chead)
        !end if
        ipts = ipts + 1
        if ( iloop .eq. 1 ) cycle
        if ( iloop .eq. 2 ) then
           x(ipts)      = xx
           y(ipts)      = yy
           !pnames(ipts) = trim(pn)
           !print *,xx,yy,trim(chead)
           ! if ( iaproc .eq. napout ) then
           !    if ( flagll ) then
           !       if ( ipts .eq. 1 ) then
           !          write (ndso,2945) factor*xx, factor*yy, pn
           !       else
           !          write (ndso,2946) ipts, factor*xx, factor*yy, pn
           !       end if
           !    else
           !       if ( ipts .eq. 1 ) then
           !          write (ndso,2955) factor*xx, factor*yy, pn
           !       else
           !          write (ndso,2956) ipts, factor*xx, factor*yy, pn
           !       end if
           !    end if
           ! end if
        end if ! iloop.eq.2
     end do ! end of file
  end do ! iloop
#endif
2054 continue

  close(ndsl)

  end program read
