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
!>\file
!>\brief Count the number of elements of a vector in a text file.
!>\author 
!> Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief Count the number of elements of a vector in a text file.
!>\details 
!> Use This subroutine to determine the length of a vector, that is a series of
!> values encloesd between two delimiters, when reading from
!> text file. The vector must be specified as a single string input (max
!> length=100), while delimiters may be specified as single character input or
!> left to default that is "(" for opening and ")" for closing.
!>\param[in] value_ the string containing teh vector whose lenght is to be determined
!>\param[in] first_, last_ sigle character delimiters of the vector (open and
!> close respectively). If not provided "(" ad ")" will be assumed as defaults
!>\author 
!> Andrea Facci.

integer function hCount(value_, first_,last_)

!---Declare Unit usage---

!---Declare Local Variables---
implicit none
character(len=100), intent(in) :: value_
character(len=1), optional :: first_,last_
integer :: n1, n2, x
character(len=100) :: vector, elements
character(len=1)   :: first,last

if(.not.present(first_)) then 
    first = '('
else
    first = first_
endif 
if(.not.present(last_)) then 
    last = ')'
else
    last = last_
endif

hCount = 0
n1       = index(value_,first) + 1
n2       = index(value_,last)  - 1
vector   = adjustl(value_(n1:n2))
elements = trim(vector)//' stop'
do 
    x        = index(elements,' ')
    hCount   = hCount + 1
    elements = adjustl(elements(x+1:))
    if(elements.eq. 'stop') exit
enddo

end function hCount
