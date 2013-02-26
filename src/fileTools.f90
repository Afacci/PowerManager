
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
!>\file fileTools.f90
!>\brief collection of proceture interfaces useful to read from files.
!>\author 
!  Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief 
!> Interfaces of procedures to read from file.
!>\details 
!> This module collects all the interfaces of the procedures useful to read data
!> from files.
!>\author 
!> Andrea Facci.

module fileTools

use shared

interface 
    subroutine rewUnit(theUnit,n)
        use shared
        implicit none
        integer, intent(in) :: theUnit, n
    end subroutine rewUnit
end interface

interface
    integer function vCount(theUnit,rew_, first_,last_)
        use shared
        implicit none
        integer, intent(in) :: theUnit
        logical , optional  :: rew_ 
        character(len=1), optional, intent(in) :: first_, last_
    end function vCount
end interface

interface
    integer function hCount(value_, first_, last_)
        use shared
        implicit none
        character(len=100), intent(in) :: value_
        character(len=1) , optional    :: first_,last_
    end function hCount
end interface

interface
    function dMatrixRead(theUnit,nline,ncol, first_, last_)
         use shared
         implicit none
         integer, intent(in)        :: theUnit, nline, ncol
         character(len=1), optional :: first_, last_
         real(kind = prec)           :: dMatrixRead(nline,ncol)
    end function dMatrixRead
end interface

interface
    function iMatrixRead(theUnit,nline,ncol, first_, last_)
         use shared
         implicit none
         integer, intent(in)        :: theUnit, nline, ncol
         character(len=1), optional :: first_, last_
         real(kind = prec)           :: iMatrixRead(nline,ncol)
    end function iMatrixRead
end interface

interface
    function cMatrixRead(theUnit,nline,ncol, first_, last_)
         use shared
         implicit none
         integer, intent(in)        :: theUnit, nline, ncol
         character(len=1), optional :: first_, last_
         character(len=100)         :: cMatrixRead(nline,ncol)
    end function cMatrixRead
end interface

interface
    function matrixRead(theUnit,nline, first_, last_)
         use shared
         implicit none
         integer, intent(in)        :: theUnit, nline
         character(len=1), optional :: first_, last_
         character(len=100)         :: matrixRead(nline)
    end function matrixRead
end interface

interface
    subroutine iFindEntry(entry,n,theUnit,rew,valore,isPresent,nRow)
        use shared
        implicit none
        integer, intent(in)            :: theUnit
        integer, intent(in)            :: n
        integer, intent(out), optional :: nRow
        integer, intent(out), optional :: valore(n)
        logical, intent(in), optional  :: rew
        logical, intent(out), optional :: isPresent
        character(len=*), intent(in)   :: entry
    end subroutine iFindEntry
end interface

interface
    subroutine dFindEntry(entry,n,theUnit,rew,valore,isPresent,nRow)
        use shared
        implicit none
        integer, intent(in)            :: theUnit
        integer, intent(in)            :: n
        integer, intent(out), optional :: nRow
        real(kind = prec), intent(out), optional :: valore(n)
        logical, intent(in), optional  :: rew
        logical, intent(out), optional :: isPresent
        character(len=*), intent(in)   :: entry
    end subroutine dFindEntry
end interface

interface
    subroutine cFindEntry(entry,n,theUnit,rew,valore,isPresent,nRow)
        use shared
        implicit none
        integer, intent(in)            :: theUnit
        integer, intent(in)            :: n
        integer, intent(out), optional :: nRow
        character(len=100), intent(out), optional :: valore(n)
        logical, intent(in), optional  :: rew
        logical, intent(out), optional :: isPresent
        character(len=*), intent(in)   :: entry
    end subroutine cFindEntry
end interface

interface
    subroutine findEntry(entry,theUnit,rew,valore,isPresent,nRow)
        use shared
        implicit none
        integer, intent(in)            :: theUnit
        integer, intent(out), optional :: nRow
        character(len=100), intent(out), optional :: valore
        logical, intent(in), optional  :: rew
        logical, intent(out), optional :: isPresent
        character(len=*), intent(in)   :: entry
    end subroutine findEntry
end interface

interface
    subroutine readKeyword (theUnit,rew,keyword,value,error,nRow)
        use shared
        implicit none
        character(len=100), intent(out):: Keyword, value
        integer, intent(out), optional :: error
        integer, intent(out), optional :: nRow
        integer, intent(in)            :: theUnit
        logical, intent(in), optional  :: rew
    end subroutine readKeyword
end interface

end module fileTools
