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
!    vCount.f90
!
!Description
!    Counts the lines of a vertical vector.
!Author 
!     Andrea Facci.
!
!---------------------------------------------------------------------------


function dMatrixRead(theUnit,nline,ncol,first_,last_)

!---Declare Module usage---

!---Declare Local Variables---
implicit none
integer, intent(in) :: theUnit, nline, ncol
real(kind(1.d0))    :: dMatrixRead(nline,ncol)
character(len=1), optional :: first_,last_
character(len=1)    :: first,last
character(len=100)  :: buffer,value, lastChar, firstChar
integer :: ifirst, ilast,n2, i, j

if(.not.present(first_)) then 
     first = '('
else
     first = first_
endif
if(.not.present(last_))  then 
     last  = ')'
else
     last = last_
endif

!---function body
lastChar  = ' '
firstChar = ' '

!---skip blank lines
do while (firstChar.ne.first)
    read(theUnit,100) buffer
    buffer    = adjustl(buffer)
    ifirst    = index(buffer,'|')
    ilast     = index(buffer(ifirst+1:),'|') + ifirst
    value     = adjustl(buffer(ifirst+1:ilast-1)) 
    firstChar = value(1:1)
enddo

call rewUnit(theUnit,1)
!--- read the matrix comprised between "(" and ")"
do j = 1,nline
    read(theUnit,100) buffer
    buffer   = adjustl(buffer)
    ifirst   = index(buffer,'|')
    ilast    = index(buffer(ifirst+1:),'|') + ifirst
    value    = adjustl(buffer(ifirst+1:ilast-1))
    if (j.eq.1) value = value(2:)
    if (j.eq.nline) then
       n2    = index(value,last)
       value =  value(1:n2-1)
    endif
    read(value,*) (dMatrixRead(j,i), i=1,ncol)
enddo

100 format(A100)

end function dMatrixRead


function iMatrixRead(theUnit,nline,ncol,first_,last_)

!---Declare Module usage---

!---Declare Local Variables---
implicit none
integer, intent(in) :: theUnit, nline, ncol
character(len=1), optional :: first_, last_
character(len=1)   :: first, last
integer             :: iMatrixRead(nline,ncol)
character(len=100)  :: buffer,value, lastChar, firstChar
integer :: ifirst, ilast,n2, i, j

if(.not.present(first_)) then 
     first = '('
else
     first = first_
endif
if(.not.present(last_))  then 
     last  = ')'
else
     last = last_
endif

!---function body
lastChar  = ' '
firstChar = ' '

!---skip blank lines
do while (firstChar.ne.first)
    read(theUnit,100) buffer
    buffer    = adjustl(buffer)
    ifirst    = index(buffer,'|')
    ilast     = index(buffer(ifirst+1:),'|') + ifirst
    value     = adjustl(buffer(ifirst+1:ilast-1)) 
    firstChar = value(1:1)
enddo

call rewUnit(theUnit,1)

!--- read the matrix comprised between "(" and ")"
do j = 1,nline
    read(theUnit,100) buffer
    buffer   = adjustl(buffer)
    ifirst   = index(buffer,'|')
    ilast    = index(buffer(ifirst+1:),'|') + ifirst
    value    = adjustl(buffer(ifirst+1:ilast-1))
    if (j.eq.1) value = value(2:)
    if (j.eq.nline) then
       n2    = index(value,last)
       value =  value(1:n2-1)
    endif
    read(value,*) (iMatrixRead(j,i), i=1,ncol)
enddo

100 format(A100)

end function iMatrixRead


function cMatrixRead(theUnit,nline,ncol, first_, last_)

!---Declare Module usage---

!---Declare Local Variables---
implicit none
integer, intent(in) :: theUnit, nline, ncol
character(len=1), optional:: first_, last_
character(len=1)    :: first,last
character(len=100)  :: cMatrixRead(nline,ncol)
character(len=100)  :: buffer,value, lastChar, firstChar
integer :: ifirst, ilast,n2, i, j

!---function body
lastChar  = ' '
firstChar = ' '

if(.not.present(first_)) then 
     first = '('
else
     first = first_
endif
if(.not.present(last_))  then 
     last  = ')'
else
     last = last_
endif

!---skip blank lines
do while (firstChar.ne.'(')
    read(theUnit,100) buffer
    buffer    = adjustl(buffer)
    ifirst    = index(buffer,'|')
    ilast     = index(buffer(ifirst+1:),'|') + ifirst
    value     = adjustl(buffer(ifirst+1:ilast-1)) 
    firstChar = value(1:1)
enddo

call rewUnit(theUnit,1)

!--- read the matrix comprised between "(" and ")"
do j = 1,nline
    read(theUnit,100) buffer
    buffer   = adjustl(buffer)
    ifirst   = index(buffer,'|')
    ilast    = index(buffer(ifirst+1:),'|') + ifirst
    value    = adjustl(buffer(ifirst+1:ilast-1))
    if (j.eq.1) value = value(2:)
    if (j.eq.nline) then
       n2    = index(value,')')
       value =  value(1:n2-1)
    endif
    read(value,*) (cMatrixRead(j,i), i=1,ncol)
enddo

100 format(A100)

end function cMatrixRead


function matrixRead(theUnit,nline, first_, last_)

!---Declare Module usage---

!---Declare Local Variables---
implicit none
integer, intent(in) :: theUnit, nline
character(len=1), optional:: first_, last_
character(len=1)    :: first,last
character(len=100)  :: matrixRead(nline)
character(len=100)  :: buffer,value, lastChar, firstChar
integer :: ifirst, ilast,n2, i, j

!---function body
lastChar  = ' '
firstChar = ' '

if(.not.present(first_)) then 
     first = '('
else
     first = first_
endif
if(.not.present(last_))  then 
     last  = ')'
else
     last = last_
endif

!---skip blank lines
do while (firstChar.ne.first)
    read(theUnit,100) buffer
    buffer    = adjustl(buffer)
    ifirst    = index(buffer,'|')
    ilast     = index(buffer(ifirst+1:),'|') + ifirst
    value     = adjustl(buffer(ifirst+1:ilast-1)) 
    firstChar = value(1:1)
enddo

call rewUnit(theUnit,1)

!--- read the matrix comprised between "(" and ")"
do j = 1,nline
    read(theUnit,100) buffer
    buffer   = adjustl(buffer)
    ifirst   = index(buffer,'|')
    ilast    = index(buffer(ifirst+1:),'|') + ifirst
    value    = adjustl(buffer(ifirst+1:ilast-1))
    if (j.eq.1) value = value(2:)
    if (j.eq.nline) then
       n2    = index(value,last)
       value =  value(1:n2-1)
    endif
!    read(value,*) matrixRead(j)
     matrixRead(j) = value
enddo

100 format(A100)

end function matrixRead
