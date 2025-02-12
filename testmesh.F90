program testmesh
  use ESMF
  use wav_shr_mod , only : write_meshdecomp

  implicit none

  type(ESMF_VM)        :: vm
  type(ESMF_Mesh)      :: Emesh
  type(ESMF_DistGrid)  :: distGrid
  type(ESMF_Field)     :: gindexfld
  integer              :: petCount, localPet
  integer              :: i,rc,iam,n,ncnt
  integer, parameter   :: nx = 1440, ny = 720, nsea = nx*ny

  !integer(ESMF_KIND_I4), pointer :: dof(:)
  !integer(ESMF_KIND_I4), allocatable :: gindex(:)
  integer(ESMF_KIND_I4), pointer :: gindex(:)

  call ESMF_Initialize(vm=vm, logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !print *,petcount,localpet
  if (petCount /= 121) then
     print *,'must run on 121 pe'
     if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  end if
  iam = localPet+1

  !call ESMF_VMBarrier (vm=vm)
  ! create a field to contain the gindex that matches exactly the
  ! ww3 run
  if (iam .le. 17)then
     ncnt = 8570
     allocate(gindex(1:ncnt))
     do n = 1,ncnt
        read(200+iam,*)i,gindex(n)
     end do
     close(200+iam)
  end if
  if (iam .ge. 18 .and. iam .le. 55) then
     ncnt = 8569
     allocate(gindex(1:ncnt))
     do n = 1,ncnt
        read(200+iam,*)i,gindex(n)
     end do
     close(200+iam)
  end if
  if (iam .ge. 56) then
     ncnt = 8568
     allocate(gindex(1:ncnt))
     do n = 1,ncnt
        read(200+iam,*)i,gindex(n)
     end do
     close(200+iam)
  end if
  !call ESMF_VMBarrier (vm=vm)
  print *,'done reading ',iam,size(gindex)
  !do n = 1,size(gindex)
  !   write(500+iam,'(2i10)')n,gindex(n)
  !end do
  !print *,'done writing'

  ! create distGrid from global index array
  DistGrid = ESMF_DistGridCreate(arbSeqIndexList=gindex, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! read in the mesh with the the DistGrid
  EMesh = ESMF_MeshCreate(filename='mesh.fix.glo_025.nc', fileformat=ESMF_FILEFORMAT_ESMFMESH, &
       elementDistgrid=Distgrid,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !EMesh = ESMF_MeshCreate(filename='mesh.fix.glo_025.nc', fileformat=ESMF_FILEFORMAT_ESMFMESH, rc=rc)
  !if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !gindexfld = ESMF_FieldCreate(EMesh,farrayptr=gindex,meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
  !call ESMF_LogWrite("here 00", ESMF_LOGMSG_INFO)
  !if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !gindexfld = ESMF_FieldCreate(EMesh,typekind=ESMF_TYPEKIND_I4,indexflag=ESMF_INDEX_GLOBAL,&
  !     meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
  !if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !call ESMF_FieldGet(gindexfld, farrayPtr=dof, rc=rc)
  !call ESMF_LogWrite("here 0", ESMF_LOGMSG_INFO)
  !if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !call ESMF_FieldRead(gindexfld, 'gindex.nc', variablename='gindex', rc=rc)
  !if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !print *,'YYY ',iam,size(dof),lbound(dof),ubound(dof)

  ! gindexfld = ESMF_FieldCreate(EMesh, ESMF_TYPEKIND_I4, name='gindex', meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
  ! if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! call ESMF_FieldGet(gindexfld, farrayPtr=gindex, rc=rc)
  ! if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! call ESMF_FieldRead(gindexfld, 'gindex.nc', variablename='gindex', rc=rc)
  ! if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! print *,'YYY ',iam,size(gindex),lbound(gindex),ubound(gindex)

  ! if(iam == 1) then
  !    do i = 5300,5310
  !       print *,'XXX0 ',i,gindex(i)
  !    end do
  !    do i = 8560,8580
  !       print *,'XXX1 ',i,gindex(i)
  !    end do
  ! end if
  !do i = 1,size(gindex)
  !  print *,'XXX:',iam,i,gindex(i)
  !end do
  !EMesh = ESMF_MeshCreate(filename='mesh.glo_025.nc', fileformat=ESMF_FILEFORMAT_ESMFMESH, &
  !     elementDistgrid=Distgrid, rc=rc)
  !if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !call ESMF_Finalize(endflag=ESMF_END_ABORT)
end program testmesh
