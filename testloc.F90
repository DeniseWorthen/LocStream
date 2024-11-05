program testloc
  use ESMF
  use wav_shr_mod, only : write_meshdecomp

  implicit none

  integer, parameter :: npts = 240
  integer, parameter :: nele = 274659, ne1=137951, ne2=136708
  integer, parameter :: nsmall = 9

  type(ESMF_VM)        :: vm
  type(ESMF_Mesh)      :: Emesh, Emesh2
  type(ESMF_DistGrid)  :: distGrid
  type(ESMF_Field)     :: doffld
  type(ESMF_LocStream) :: LS, LS2, ptsLS

  integer                  :: petCount, localPet
  integer                  :: i,n,nn,iounit,iostat,iaproc
  integer                  :: rc
  integer                  :: elb, eub,ecnt
  real(kind=4)             :: x(npts),y(npts),ids(npts)
  real(kind=8), pointer    :: xx(:), yy(:), buoyid(:)
  real(kind=8), pointer    :: lsxx(:), lsyy(:), lsid(:)
  integer(kind=4), pointer :: dof(:)
  character(len=20)        :: cvalue
  integer, allocatable     :: gindex(:)

  call ESMF_Initialize(vm=vm, logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
  !print *,petcount,localpet
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (petCount /= 2) then
     if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  end if

  iaproc = localpet+1

  !-------------------------------------------------------

  cvalue = 'mesh.global_270k.nc'
  ! recreate ww3 internal decomp
  if (iaproc == 1) then
     allocate(gindex(ne1))
     do i = 1,ne1
        gindex(i) = i
     end do
  end if
  if (iaproc == 2) then
     allocate(gindex(ne2))
     do i = 1,ne2
        gindex(i) = i+ne1
     end do
  end if
  !do i = 1,size(gindex)
  !   write(10+iaproc,*)iaproc,i,gindex(i)
  !end do
  DistGrid = ESMF_DistGridCreate(arbSeqIndexList=gindex, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  EMesh = ESMF_MeshCreate(filename='mesh.global_270k.nc', fileformat=ESMF_FILEFORMAT_ESMFMESH, &
       elementDistgrid=Distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  doffld = ESMF_FieldCreate(EMesh, typekind=ESMF_TYPEKIND_I4, meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldGet(field=doffld, farrayPtr=dof, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldRead(doffld, 'emesh.decomp.nc', variableName='dof', rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !do i = 1,size(dof)
  !   write(20+iaproc,*)iaproc,i,dof(i)
  !end do
  DistGrid = ESMF_DistGridCreate(arbSeqIndexList=dof, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  EMesh2 = ESMF_MeshCreate(EMesh, elementdistgrid=Distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! check
  call write_meshdecomp(Emesh2, 'emesh2', iaproc, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !-------------------------------------------------------

  open(newunit=iounit, file='pts.dat', status='old')
  do n = 1,npts
     read(iounit,*)x(n),y(n),ids(n)
  end do
  close(iounit)

  !use first nsmall locations
  allocate(xx(nsmall))
  allocate(yy(nsmall))
  allocate(buoyid(nsmall))
  do i = 1,nsmall
     xx(i) = real(x(i),8)
     yy(i) = real(y(i),8)
     buoyid(i) = real(ids(i),8)
  end do

  LS = ESMF_LocStreamCreate(maxIndex=nsmall, indexflag=ESMF_INDEX_GLOBAL, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_LocStreamGetBounds(LS, exclusiveLBound=elb, exclusiveUBound=eub, exclusiveCount=ecnt,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  print *,'pet=',iaproc,' elb,eub,ecnt= ',elb,eub,ecnt

  LS2 = ESMF_LocStreamCreate(localcount=ecnt, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  if (ecnt > 0) then
     call ESMF_LocStreamAddKey(LS2, keyName="ESMF:Lat", farray=yy(elb:eub), &
          datacopyflag=ESMF_DATACOPY_VALUE, keyUnits="Degrees", keyLongName="Latitude", rc=rc)
     if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

     call ESMF_LocStreamAddKey(LS2, keyName="ESMF:Lon", farray=xx(elb:eub), &
          datacopyflag=ESMF_DATACOPY_VALUE, keyUnits="Degrees", keyLongName="Longitude", rc=rc)
     if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

     call ESMF_LocStreamAddKey(LS2, keyName="ESMF:ID", farray=buoyid(elb:eub), &
          datacopyflag=ESMF_DATACOPY_VALUE, keyUnits="1", keyLongName="BuoyID", rc=rc)
     if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  end if

  ! project locstream onto mesh
  ptsLS = ESMF_LocstreamCreate(LS2, background=Emesh2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_LocStreamGetBounds(ptsLS, exclusiveLBound=elb, exclusiveUBound=eub, exclusiveCount=ecnt,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  print *,'bkgrd: pet=',iaproc,' elb,eub,ecnt= ',elb,eub,ecnt

  call ESMF_LocStreamGetKey(ptsLS, keyName="ESMF:Lat", farray=lsyy, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_LocStreamGetKey(ptsLS, keyName="ESMF:Lon", farray=lsxx, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_LocStreamGetKey(ptsLS, keyName="ESMF:ID", farray=lsid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  do i = elb,eub
     print '(2i6,3f12.2)',iaproc,i,lsxx(i),lsyy(i),lsid(i)
  end do

end program testloc
