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
!>\file vCount.f90
!>\brief  Counts the lines of a text matrix.
!>\author 
!  Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief  Counts the lines of a text matrix.
!>\details This subroutine determines the number of lines between the two array delimiters. The
!> unit must be already opened and associated to the desired file. All the
!> values to read must be written in "value" field of the text file that is
!> between the two "|" (see the following box for an example of input text) and
!> delimited by appropriate opening and closure delimiters. 
!> \verbatim
!> A Lot of usless stuff because only text between begin and end is read.
!>    begin 
!>       !Commented line
!>       KeywordField  | ValueField | !Comment two
!>    end
!>\endverbatim  
!> Moreover the cursor needs to be before the first line of the array to be read. The
!> procedure atomatically skeeps all the lines until the opening character.
!> Opening and closing character may be specified as single character input, or left to the defult
!> values that are "(" ande ")", respectively. Avoid blank or commented lines inside the text array.\n
!> The correct file syntax to read the 2x2 array "exArr": 
!>\f[
!> exArr = \left[ 
!> \begin{array}{c c}
!>      1.1 & 1.2 \\
!>      2.1 & 2.2
!> \end{array}
!> \right]  
!>\f]
!> is:\n
!> \verbatim
!>    begin
!>       !Comment one
!>       exArr  | (1.1 1.2 | !Comment two
!>              |  2.1 2.2)|
!>    end
!>\endverbatim  
!> or equivalently: \n
!> \verbatim
!>    begin
!>       !Comment one
!>       exArr  |        | !Comment two
!>              | (1.1 1.2 |
!>              |  2.1 2.2)|
!>    end
!>\endverbatim  
!> so that each line in the text represnts a row of the vector output and coluns
!> are space separated. 
!>\param[in] theUnit       The unit associated to the file to be read
!>\param[in] rew_          Weather to rewind or not the unit at the end. Default is false.
!>\param[in] first_, last_ Opening and closing characters of the array.
!>\author 
!  Andrea Facci.
integer function vCount(theUnit,rew_,first_,last_)

!---Declare Unit usage---
use shared

!---Declare Local Variables---
implicit none
integer, intent(in) :: theUnit
logical , optional :: rew_
character(len=100) :: buffer,value, lastChar, firstChar
integer :: ifirst, ilast,n2, i
logical :: rew
character(len=1) , optional, intent(in) :: first_, last_
character(len=1) :: first, last

if(.not.present(rew_)) then 
    rew = .false.
else
    rew = rew_
endif
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

!---function body
vCount = 1;
lastChar = ' '
firstChar= ' '

!---skip blank lines
do while (firstChar.ne.first)
    read(theUnit,100) buffer
    buffer    = adjustl(buffer)
    ifirst    = index(buffer,'|')
    ilast     = index(buffer(ifirst+1:),'|') + ifirst
    value     = adjustl(buffer(ifirst+1:ilast-1)) 
    firstChar = value(1:1)
enddo

!--- count the number of lines before vector closing character ")"
do while (lastChar.ne.last)
    read(theUnit,100) buffer
    buffer   = adjustl(buffer)
    ifirst   = index(buffer,'|')
    ilast    = index(buffer(ifirst+1:),'|') + ifirst
    value    = trim(buffer(ifirst+1:ilast-1))
    n2       = len_trim(value)
    lastChar = value(n2:)
    vCount   = vCount + 1
enddo

if(rew) call rewUnit(theUnit,vCount)

100 format(A100)

end function vCount
