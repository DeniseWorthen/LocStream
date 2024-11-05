module wav_shr_mod

  use ESMF

  implicit none

  public :: write_meshdecomp
  public :: chkerr
  ! Module data
  character(len=*), parameter :: u_FILE_u = &          !< a character string for an ESMF log message
       __FILE__

contains

  subroutine write_meshdecomp(EMeshIn, mesh_name, iaproc, rc)

    !use ESMF          , only : ESMF_Mesh, ESMF_DistGrid, ESMF_Field, ESMF_FieldBundle, ESMF_FieldBundleAdd
    !use ESMF          , only : ESMF_DistGridGet, ESMF_FieldBundleCreate, ESMF_FieldCreate, ESMF_FieldBundleGet
    !use ESMF          , only : ESMF_MESHLOC_ELEMENT, ESMF_TYPEKIND_R8, ESMF_TYPEKIND_I4, ESMF_LOGMSG_Info
    !use ESMF          , only : ESMF_FieldBundleWrite, ESMF_FieldBundleDestroy

    ! input/output variables
    type(ESMF_Mesh) , intent(in)  :: EMeshIn
    character(len=*), intent(in)  :: mesh_name
    integer         , intent(out) :: rc
    integer         , intent(in)  :: iaproc
    ! local variables
    type(ESMF_FieldBundle)         :: FBTemp
    type(ESMF_Field)               :: lfield
    type(ESMF_DistGrid)            :: distgrid
    type(ESMF_Field)               :: doffield
    character(len=6), dimension(4) :: lfieldlist
    integer                        :: i,ndims,nelements
    real(8), pointer              :: fldptr1d(:)
    integer(4), allocatable       :: dof(:)
    integer(4), pointer           :: dofptr(:)
    real(8), pointer              :: ownedElemCoords(:), ownedElemCoords_x(:), ownedElemCoords_y(:)
    character(len=*),parameter     :: subname = '(wav_shr_mod:write_meshdecomp) '
    !-------------------------------------------------------

    rc = ESMF_SUCCESS
    !if (dbug_flag  > 5) call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

    ! create a temporary FB to write the fields
    FBtemp = ESMF_FieldBundleCreate(rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    call ESMF_MeshGet(EMeshIn, spatialDim=ndims, numOwnedElements=nelements, elementDistgrid=distgrid, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    !print *,iaproc,ndims,nelements

    lfieldlist = (/'dof   ', 'coordx', 'coordy', 'decomp'/)
    ! index array
    doffield = ESMF_FieldCreate(EMeshIn, ESMF_TYPEKIND_I4, name=trim(lfieldlist(1)), &
         meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call ESMF_FieldBundleAdd(FBTemp, (/doffield/), rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    ! coords and decomp field
    do i = 2,size(lfieldlist)
       lfield = ESMF_FieldCreate(EMeshIn, ESMF_TYPEKIND_R8, name=trim(lfieldlist(i)), &
            meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       call ESMF_FieldBundleAdd(FBTemp, (/lfield/), rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
    end do

    ! Set element coordinates
    allocate(ownedElemCoords(ndims*nelements))
    allocate(ownedElemCoords_x(ndims*nelements/2))
    allocate(ownedElemCoords_y(ndims*nelements/2))
    call ESMF_MeshGet(EmeshIn, ownedElemCoords=ownedElemCoords, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    ownedElemCoords_x(1:nelements) = ownedElemCoords(1::2)
    ownedElemCoords_y(1:nelements) = ownedElemCoords(2::2)
    allocate(dof(1:nelements))
    call ESMF_DistGridGet(distgrid, localDE=0, seqIndexList=dof, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call ESMF_FieldBundleGet(FBtemp, fieldName='dof', field=doffield, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call ESMF_FieldGet(doffield, farrayPtr=dofptr, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    dofptr(:) = dof(:)

    call ESMF_FieldBundleGet(FBtemp, fieldName='coordx', field=lfield, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call ESMF_FieldGet(lfield, farrayPtr=fldptr1d, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    fldptr1d(:) = ownedElemCoords_x(:)

    call ESMF_FieldBundleGet(FBtemp, fieldName='coordy', field=lfield, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call ESMF_FieldGet(lfield, farrayPtr=fldptr1d, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    fldptr1d(:) = ownedElemCoords_y(:)

    call ESMF_FieldBundleGet(FBtemp, fieldName='decomp', field=lfield, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call ESMF_FieldGet(lfield, farrayPtr=fldptr1d, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    do i = 1,ndims*nelements/2
       fldptr1d(i) = iaproc
       !if (i.ge.137000.and.i.le.138200)print *,i,iaproc,fldptr1d(i)
    end do
    call ESMF_FieldBundleWrite(FBtemp, filename=trim(mesh_name)//'.decomp.nc', overwrite=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    deallocate(ownedElemCoords)
    deallocate(ownedElemCoords_x)
    deallocate(ownedElemCoords_y)
    deallocate(dof)

    call ESMF_FieldBundleDestroy(FBtemp, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    !if (dbug_flag  > 5) call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)
  end subroutine write_meshdecomp
  logical function chkerr(rc, line, file)

    integer, intent(in) :: rc
    integer, intent(in) :: line
    character(len=*), intent(in) :: file

    integer :: lrc

    chkerr = .false.
    lrc = rc
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=line, file=file)) then
      chkerr = .true.
    endif
  end function chkerr
end module wav_shr_mod
