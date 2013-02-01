!---------------------------------------------------------------------------
!  
!   PowerManager.  
!
!---------------------------------------------------------------------------
!License
!    This file is part of PowerManager.
!
!    PowerManager is free software; you can redistribute it and/or modify it
!    under the terms of the GNU General Public License as published by the
!    Free Software Foundation; either version 2 of the License, or (at your
!    option) any later version.
!
!    PowerManager is distributed in the hope that it will be useful, but WITHOUT
!    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
!    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
!    for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with OpenFOAM; if not, write to the Free Software Foundation,
!    Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
!
!Application
!    The application
!
!Description
!    very simple description.
!Author 
!     Andrea Facci.
!
!---------------------------------------------------------------------------

subroutine allCombin(cm,imax,m,comb,nComb)

!---Declare Module usage---

implicit none
integer, intent(in)                                          :: m
integer, intent(in), dimension(m)                            :: imax
real(kind(1.d0)), dimension(maxval(imax),m), intent(in)      :: cm
integer, intent(out), optional                               :: nComb
real(kind(1.d0)),  dimension(maxval(imax)**m,m), intent(out) :: comb
!real(kind(1.d0)), allocatable, dimension(:,:), intent(out) :: comb
integer         , dimension(m)   :: j
integer                          :: k, i, h,n
logical                          :: fine

n=maxval(imax)
!---Declare Local Variables---
k = n**m        ! maximum number of combinations. The actual number
                ! will be lower, because m is the maximum number of 
                ! set points.
!allocate(comb(k,m))
comb(:,:) = -1.d0
i = 0
j(:) = 1
do 
    fine = .true.
    i = i + 1
    do h = 1,m
        comb(i,h) = cm(j(h),h)
    enddo
    do h =m,1,-1
        if(j(h).lt.imax(h)) then
            j(h) = j(h) + 1
            fine = .false.
            exit
        else
            j(h) = 1
        endif
    enddo
    if(fine) exit
enddo

if(present(nComb)) ncomb = i

end subroutine allCombin
