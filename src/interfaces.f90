
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
!    along with PowerManager; if not, write to the Free Software Foundation,
!    Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
!
!>\file interfaces.f90
!>\brief Collection of general pourpose interfaces.
!>\author 
!     Andrea Facci.
!
!---------------------------------------------------------------------------

!>\file interfaces.f90
!>\brief Collection of general pourpose interfaces.
!>\author 
!     Andrea Facci.
module interfaces

interface 
    subroutine abortExecution(i,j,line,word,r1,r2, iVec)
        use shared
        implicit none
        integer, intent(in), optional          :: i,j
        real(kind = prec), intent(in), dimension(*), optional :: iVec
        integer, intent(in), optional          :: line
        character(len=*), intent(in), optional :: word
        real(kind = prec), intent(in), optional :: r1, r2
    end subroutine abortExecution
end interface

interface 
    subroutine warning(i,j,k,line,word,r1,r2)
        use shared
        implicit none
        integer, intent(in), optional          :: i,j,k
        integer, intent(in), optional          :: line
        character(len=*), intent(in), optional :: word
        real(kind = prec), intent(in), optional :: r1, r2
    end subroutine warning
end interface

interface
    subroutine performances(c, cOld, equip, num,t, pEl, pTh, pCh, eIn, mf, cfu,cm,cOn) 
        use shared
        use inputVar
        use plantVar
        implicit none
        integer, intent(in) :: c, num, t
        integer, intent(in), optional :: cOld
        character(len=*), intent(in) :: equip
        real(kind = prec), intent(out), optional :: pEl, pTh, pCh, eIn, mf, cfu,cm,cOn
    end subroutine performances
end interface

end module interfaces
