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

module graphVar

real(kind(1.d0)), allocatable, dimension(:,:) :: pointLoad, predCost 
integer,          allocatable, dimension(:,:) :: predList
real(kind(1.d0)), allocatable, dimension(:)   :: pointCost, nt, pointTime
integer,          allocatable, dimension(:)   :: nPre
integer                                       :: nPoint

interface
    real(kind(1.d0)) function objFunction(c,t,obj)
        use plantVar, only : nm
        implicit none
        integer, dimension(nm), intent(in)  :: c
        integer,                 intent(in) :: t
        character(len=100),      intent(in) :: obj 
    end function objFunction
end interface

interface
    logical function constraints(c,t)
        use plantVar
        use interfaces
        use inputVar
        implicit none
        integer, dimension(nm), intent(in) :: c
        integer,                intent(in) :: t
    end function constraints
end interface

contains


end module graphVar
