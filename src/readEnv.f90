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
!>\file readLoad.f90
!>\brief Reads Load.inp file
!>\author 
!>     Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief Reads Load.inp file
!>\details 
!> This subroutine reads the file "Load.inp". The procedure looks for each specific entry
!> in the "keyword field", and associates the value in the "value" field, to the
!> corresponding variable. If the desired entry is not present returns an error
!> message and aborts the execution. The structure of the input file is
!> clarified in the following example along with the meaning of "keyword" and
!> "value" field.\n
!> \verbatim
!> A Lot of usless stuff because only text between begin and end is read.
!>    begin 
!>       !Commented line
!>       KeywordField  | ValueField | !Comment two
!>
!>       KeywordField  | ValueField |
!>       ScalarValue   | 1          |
!>       Vector        | 1 2 n      |
!>       VectorSeries  |(a b c ) (d e f)|
!>       Matrix        |(11 12 13  |
!>                     | 21 22 23  |
!>                     | 31 32 33) |
!>    end
!> A lof really usless text
!>\endverbatim
!> Note that only the text between "begin" and "end" is read. Blank lines are
!> automatically discarded, while any unrecognized entry is discarded returning
!> a warning code. Line beginning with "!" are considered comments and
!> discarded.
!>\author 
!>     Andrea Facci.

subroutine readEnv

!---Declare Unit usage---
use shared
use inputVar
use fileTools
use interfaces
use cmdVar

implicit none

!---Declare Local Variables---
integer              :: genUnit = 110, idummy
character(len=100)    :: inputFile = './Input/Environment.inp'
logical              :: filePresent
character(len=500)   :: buffer, keyword, value, cdummy
integer              :: i, j, nl, error, line, n
logical,dimension(2) :: isPresent = .false.
character(len=100),dimension(1,3)   :: env
real(kind = prec), allocatable, dimension(:,:) :: matrix

!---Check File Presence---
call openUnit(inputFile,genUnit,filePresent)
if(.not.filePresent) call abortExecution(1,7)

!---Skip all the lines before the keyword "begin".
Line = 0;
do 
    read(genUnit,100) buffer
    buffer = adjustl(buffer)
    Line = Line + 1
    if(buffer(1:5).eq.'begin') exit
enddo

do 
    call readKeyword(genUnit,.false., keyword,value,error, nl)
    if (error.eq.1) call abortExecution(0,6,line)
    line = line + nl
    select case(keyword)
       case('end')
          exit
       case('Climate')
          isPresent(1) = .true.
          call rewUnit(genUnit,1)
          env = cMatrixRead(theUnit=genUnit, nline=1, nCol=3)
          n = vCount(genUnit,.true.)
          if(n.ne.nTime) call abortExecution(17)
          allocate(matrix(nTime,3))
          matrix =  dmatrixRead(genUnit,n,3)
          call allocateVar(22)
       case('Altitude')
          isPresent(2) = .true.
          read(value,*) altitude
       case(' ') 
          if(verb) call warning(4,3,line=line)
       case default
          if(.not.silent) call warning(1,3,line=line,word=keyword)
    end select
enddo

do i=1,3
   buffer = env(1,i)
   select case(buffer)
       case('Time')
           iTime = i
           if(iTime.ne.1) call abortExecution(8)
       case('Temperature')
           tAmb = matrix(:,i)
       case('Pressure')
           pAmb = matrix(:,i)
   end select
end do

close(genUnit)
100 format(A500)

end subroutine readEnv
