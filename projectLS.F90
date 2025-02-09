module projectLS

  use ESMF
  use wav_shr_mod, only : chkerr

  implicit none

  public :: findmapped

  ! Module data
  character(len=*), parameter :: u_FILE_u = &          !< a character string for an ESMF log message
       __FILE__
contains

  subroutine findmapped(EMeshIn, LS, xx, yy, ipt, iaproc, ecnt, ismapped, rc)

    ! input/output variables
    type(ESMF_Mesh)        , intent(in)  :: EMeshIn
    type(ESMF_LocStream)   , intent(in)  :: LS
    integer, intent(in) :: ipt, iaproc, ecnt
    real(kind=8) , pointer , intent(in)  :: xx(:), yy(:)
    logical                , intent(inout) :: ismapped
    integer                , intent(out) :: rc

    type(ESMF_LocStream) :: ptsLS, LS2
    integer :: localrc,elb,eub
    character(len=100) :: msg

    rc = ESMF_SUCCESS
    ismapped = .true.

     !if (ecnt == 0) return
       ptsLS = ESMF_LocstreamCreate(LS, background=EmeshIn, rc=localrc)
       if (localrc /= ESMF_SUCCESS) ismapped = .false.
       if (ESMF_LocStreamIsCreated(ptsLS))call ESMF_LocStreamDestroy(ptsLS, noGarbage = .true., rc=rc)
    !end if
    !if (chkerr(localrc,__LINE__,u_FILE_u)) ismapped = .false.
    !print *,'X1 ',ipt,ismapped

    ! write(msg,'(a,i6)')'here 0 ',ipt
    ! call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)

    ! if (chkerr(localrc,__LINE__,u_FILE_u)) return
    ! write(msg,'(a,i6)')'here 1 ',ipt

    ! call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    ! write(msg,'(a,i6)')'here 2 ',ipt

    ! if (localrc /= ESMF_SUCCESS) ismapped = .false.

    ! write(msg,'(a,i6)')'here 3 ',ipt
    ! call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)

    ! if (ESMF_LocStreamIsCreated(ptsLS))call ESMF_LocStreamDestroy(ptsLS, noGarbage = .true., rc=rc)

    ! write(msg,'(a,i6)')'here 4 ',ipt
    ! call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)

    ! write(msg,'(a,i6)')'here 5 ',ipt
    ! if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! write(msg,'(a,i6)')'here 6 ',ipt
    ! call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)

    ! rc = ESMF_SUCCESS

  end subroutine findmapped
end module projectLS
