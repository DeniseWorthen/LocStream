program testmesh
  use ESMF
  use wav_shr_mod , only : write_meshdecomp

  implicit none

  type(ESMF_VM)        :: vm
  type(ESMF_Mesh)      :: Emesh
  type(ESMF_DistGrid)  :: distGrid
  type(ESMF_Field)     :: doffield
  integer              :: petCount, localPet
  integer              :: i,rc
  integer, parameter   :: nx = 1440, ny = 721, nsea = nx*ny
  integer(kind=4), pointer :: dof(:)

  call ESMF_Initialize(vm=vm, logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
  !if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !print *,petcount,localpet
  !if (petCount /= 121) then
  !   print *,'must run on 121 pe'
  !   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !end if

  !EMesh = ESMF_MeshCreate(filename='mesh.glo_025.fix.nc', fileformat=ESMF_FILEFORMAT_ESMFMESH, rc=rc)
  !if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !doffield = ESMF_FieldCreate(EMesh, ESMF_TYPEKIND_I4, name='dof', meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
  !if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !call ESMF_FieldRead(doffield, 'emesh.decomp.nc', variablename='dof', rc=rc)
  !if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !do i = 1,size(dof)
  !   print *,i,dof(i)
  !end do
  !EMesh = ESMF_MeshCreate(filename='mesh.glo_025.nc', fileformat=ESMF_FILEFORMAT_ESMFMESH, &
  !     elementDistgrid=Distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Finalize(endflag=ESMF_END_ABORT)
end program testmesh
