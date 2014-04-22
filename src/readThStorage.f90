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
!>\file readBoiler.f90
!>\brief Reads ThermalStorage.inp file
!>\author 
!>     Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief Reads ThermalStorage.inp file
!>\details 
!> This subroutine reads the file "Boilers.inp". The procedure looks for each specific entry
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

subroutine readThStorage

!---Declare Unit usage---
use shared
use inputVar
use fileTools
use interfaces
use cmdVar
use myArithmetic

implicit none

!---Declare Local Variables---
integer              :: genUnit = 111
character(len=50)    :: inputFile = './Input/ThermalStorage.inp'
logical              :: filePresent
character(len=lword) :: buffer, keyword, value
integer              :: firstLine, line, i, nInp, nl
logical,dimension(5) :: isPresent = .false.
integer              :: error

!---Check File Presence---
inquire(file = inputFile, exist = filePresent)
if(.not.filePresent) then
   call abortExecution(1,3)
else
   open(unit = genUnit, file = inputFile)
endif

!---Skip all the lines befor the keyword "begin".
firstLine = 0;
do 
    read(genUnit,100) buffer
    buffer = adjustl(buffer)
    firstLine = firstLine + 1
    if(buffer(1:5).eq.'begin') exit
enddo

line = firstLine
!---read the input list---
do 
    call readKeyword(genUnit,.false., keyword,value,error, nl)
    if (error.eq.1) call abortExecution(0,7)
    line = line + nl
    select case(keyword)
       case('end')
          exit
       case('Power')
          read(value,*) pMaxTS
          isPresent(1) = .true.
       case('Capacity')
          read(value,*) CapacityTS
          isPresent(2) = .true.
       case('SetPoint')
          read(value,*) nSpTS
          isPresent(3) = .true.
       case('InputEfficiency')
              isPresent(4) = .true.
!              backspace(genUnit)
!              line         = line - 1
!              nEtaTsIn     = vCount(genUnit,.false.)
              nEtaTsIn = 1
!              call allocateVar(33)
!              call rewUnit(genUnit,nEtaTsIn)
!              etaTsIn(:,:) =  dmatrixRead(genUnit,nEtaTsIn,2)
              read(value,*) etaTsIn
       case('OutputEfficiency')
              isPresent(5)  = .true.
!              backspace(genUnit)
!              line          = line - 1
!              nEtaTsOut     = vCount(genUnit,.false.)
              nEtaTsOut     = 1
!              call allocateVar(34)
!              call rewUnit(genUnit,nEtaTsOut)
!              etaTsOut(:,:) =  dmatrixRead(genUnit,nEtaTsOut,2)
              read(value,*) etaTsOut
       case default
            if(.not.silent) call warning(1,6,line=line,word=keyword)
    end select
enddo

!---check if all the variablea were read---
nInp = size(isPresent)
do i = 1,nInp
    if(.not.isPresent(i)) call abortExecution(28,i)
enddo

close(genUnit)
100 format(A500)

end subroutine readThStorage
