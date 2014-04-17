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
!>\brief reads 2D array of values from a file
!>\details Collection of procedures to read 2D arrays from files. Each function
!> associates the values to a different data type (integer, double, character).
!> the first letter of the function name indicates the data type.
!>\author 
!> Andrea Facci.
!
!---------------------------------------------------------------------------


!>\brief reads 2D array of double precision values from a file.
!>\details This subroutine reads a 2D array of double precision values directly from a specified unit. The
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
!>\param[in] TheUnit the unit to be read. 
!>\param[in] nline   The number of rows of the 2D array
!>\param[in] ncol    The number of columns  of the 2D array
!>\param[in] first_  the opening delimiter of the array. Default is "("
!>\param[in] last_   the closing delimiter of the array. Default is ")"
!>\author Andrea Facci.

function dMatrixRead(theUnit,nline,ncol,first_,last_)

!---Declare Module usage---
use shared
!---Declare Local Variables---
implicit none

interface
    function matrixRead(theUnit,nline, first_, last_)
        use shared
        implicit none
        integer, intent(in) :: theUnit, nline
        character(len=1), optional:: first_, last_
        character(len=500)  :: matrixRead(nline)
    end function matrixRead
end interface

integer, intent(in) :: theUnit, nline, ncol
real(kind = prec )    :: dMatrixRead(nline,ncol)
character(len=1), optional :: first_,last_
character(len=1)    :: first,last
character(len=500)  :: buffer,value, lastChar, firstChar
integer :: ifirst, ilast,n2, i, j
character(len=500), dimension(nline) :: matrix

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
matrix = matrixRead(theUnit,nline, first, last)
do j=1,nline
    read(matrix(j),*) (dMatrixRead(j,i), i=1,ncol)
enddo

100 format(A100)

end function dMatrixRead

!======================================================================================

!>\brief reads 2D array of integer values from a file.
!>\details This subroutine reads a 2D array of integer values directly from a specified unit. The
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
!>      11 & 12 \\
!>      21 & 22
!> \end{array}
!> \right]  
!>\f]
!> is:\n
!> \verbatim
!>    begin
!>       !Comment one
!>       exArr  | (11 12 | !Comment two
!>              |  21 22)|
!>    end
!>\endverbatim  
!> or equivalently: \n
!> \verbatim
!>    begin
!>       !Comment one
!>       exArr  |        | !Comment two
!>              | (11 12 |
!>              |  21 22)|
!>    end
!>\endverbatim  
!> so that each line in the text represnts a row of the vector output and coluns
!> are space separated. 
!>\param[in] TheUnit the unit to be read. 
!>\param[in] nline   The number of rows of the 2D array
!>\param[in] ncol    The number of columns  of the 2D array
!>\param[in] first_  the opening delimiter of the array. Default is "("
!>\param[in] last_   the closing delimiter of the array. Default is ")"
!>\author Andrea Facci.



function iMatrixRead(theUnit,nline,ncol,first_,last_)

use shared
!---Declare Module usage---
implicit none

interface
    function matrixRead(theUnit,nline, first_, last_)
        use shared
        implicit none
        integer, intent(in) :: theUnit, nline
        character(len=1), optional:: first_, last_
        character(len=500)  :: matrixRead(nline)
    end function matrixRead
end interface


!---Declare Local Variables---
integer, intent(in) :: theUnit, nline, ncol
character(len=1), optional :: first_, last_
character(len=1)   :: first, last
integer             :: iMatrixRead(nline,ncol)
character(len=500)  :: buffer,value, lastChar, firstChar
integer :: ifirst, ilast,n2, i, j
character(len=500), dimension(nline) :: matrix

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

matrix = matrixRead(theUnit,nline, first, last)
do j=1,nline
    read(matrix(j),*) (iMatrixRead(j,i), i=1,ncol)
enddo

100 format(A100)

end function iMatrixRead

!==============================================================================

!>\brief reads 2D array of character values from a file.
!>\details This subroutine reads a 2D array of character values directly from a specified unit. The
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
!>      Scrudge & DonaldDuck \\
!>      Goofy   & MickeyMouse  
!> \end{array}
!> \right]  
!>\f]
!> is:\n
!> \verbatim
!>    begin
!>       !Comment one
!>       exArr  | (Scrudge DonadDuck | !Comment two
!>              |  Goofy MikeyMouse) |
!>    end
!>\endverbatim  
!> or equivalently: \n
!> \verbatim
!>    begin
!>       !Comment one
!>       exArr  |        | !Comment two
!>              | (Scrudge DonaldDuck |
!>              |  Goofy   MickeyMouse)|
!>    end
!>\endverbatim  
!> so that each line in the text represnts a row of the vector output and coluns
!> are space separated. 
!>\param[in] TheUnit the unit to be read. 
!>\param[in] nline   The number of rows of the 2D array
!>\param[in] ncol    The number of columns  of the 2D array
!>\param[in] first_  the opening delimiter of the array. Default is "("
!>\param[in] last_   the closing delimiter of the array. Default is ")"
!>\author Andrea Facci.


function cMatrixRead(theUnit,nline,ncol, first_, last_)

!---Declare Module usage---
use shared

implicit none
interface
    function matrixRead(theUnit,nline, first_, last_)
        use shared
        implicit none
        integer, intent(in) :: theUnit, nline
        character(len=1), optional:: first_, last_
        character(len=500)  :: matrixRead(nline)
    end function matrixRead
end interface

!---Declare Local Variables---
integer, intent(in) :: theUnit, nline, ncol
character(len=1), optional:: first_, last_
character(len=1)    :: first,last
character(len=500)  :: cMatrixRead(nline,ncol)
character(len=500)  :: buffer,value, lastChar, firstChar
character(len=500), dimension(nline) :: matrix
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

matrix = matrixRead(theUnit,nline, first, last)
do j=1,nline
    read(matrix(j),*) (cMatrixRead(j,i), i=1,ncol)
enddo

100 format(A100)

end function cMatrixRead

!>\brief reads 2D array from a file.
!>\details This subroutine reads a 2D array of directly from a specified unit. This procedure does not associate
!> the elements of the text array to any data type. On the contrary each line in the text array is associated to a row of
!> a character type row vector, as it is.
!> All the values to read must be written in "value" field of the text file that is
!> between the two "|" (see the following box for an example of input text) and
!> delimited by appropriate opening and closure delimiters. 
!> \verbatim
!> A Lot of usless stuff because only text between begin and end is read.
!>    begin 
!>       !Commented line
!>       KeywordField  | ValueField | !Comment two
!>    end
!>\endverbatim  

!> The unit must be already opened and associated to the desired file. Moreover the
!> cursor needs to be before the first line of the array to be read. The
!> procedure atomatically skeeps all the lines until the opening character.
!> Opening and closing character may be specified as single character input, or left to the defult
!> values that are "(" and ")", respectively. Avoid blank or commented lines inside the text array.
!>\param[in] TheUnit the unit to be read. 
!>\param[in] nline   The number of rows of the 2D array
!>\param[in] first_  the opening delimiter of the array. Default is "("
!>\param[in] last_   the closing delimiter of the array. Default is ")"
!>\author Andrea Facci.

function matrixRead(theUnit,nline, first_, last_)

!---Declare Module usage---
use shared

!---Declare Local Variables---
implicit none
integer, intent(in) :: theUnit, nline
character(len=1), optional:: first_, last_
character(len=1)    :: first,last
character(len=500)  :: matrixRead(nline)
character(len=500)  :: buffer,value, lastChar, firstChar
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
    matrixRead(j) = value
enddo

100 format(A500)

end function matrixRead
