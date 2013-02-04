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
!>\file readGeneral.f90
!>\brief Reads General.inp file
!>\author 
!>     Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief Reads Genearl.inp file
!>\details 
!> This subroutine reads the file "General.inp". The procedure looks for each specific entry
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
subroutine readGeneral

!---Declare Unit usage---
use inputVar
use fileTools
use interfaces
use cmdVar

implicit none

!---Declare Local Variables---
integer              :: genUnit = 105
character(len=20)    :: inputFile = './Input/General.inp'
logical              :: filePresent
character(len=100)   :: buffer, keyword, value, value_
integer              :: firstLine, i, nInp, line, n, n_
logical,dimension(8) :: isPresent = .false.
integer              :: error
character(len=50)    :: dummy


!---Check File Presence---
inquire(file = inputFile, exist = filePresent)
if(.not.filePresent) then
   call abortExecution(1,1)
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

!---read the input list---
line = firstLine
do 
    call readKeyword(genUnit,.false., keyword,value,error,n)
    line = line + n
    if (error.eq.1)  call abortExecution(0,1,line)
    select case(keyword)
       case('end')
          exit
       case('GridConnection')
          read(value,*) gridConnection
          gridConnection = trim(gridConnection)
          isPresent(1) = .true.
          if(gridConnection.ne.'NetMetering'.and.gridConnection               &
             .ne.'DedicatedRetire'.and.gridConnection.ne.'StandAlone')  then
              call abortExecution(2,1,line,gridConnection)
          endif
       case('Degradation')
          read(value,*) iDeg
          isPresent(2) = .true.
       case('objective')
          read(value,*) obj
          isPresent(3) = .true.
       case('StartPoint')
          isPresent(4) = .true.
          n = nTrig + nBoi + nChi
          call allocateVar(0,n)
          value_ = '('//trim(value)//')'
          n_ = hCount(value_)
          if(n_.ne.n) call abortExecution(13,n_,n,'StartPoint')
          read(value,*) (startPoint(i), i=1,n)
       case('FirstTimeStep')
          isPresent(5)  = .true.
          read(value,*) dt1
       case('UpTime')
          isPresent(6) = .true.
          n = nTrig + nBoi + nChi
          value_ = '('//trim(value)//')'
          n_ = hCount(value_)
          if(n_.ne.n) call abortExecution(13,n_,n,'UpTime')
          read(value,*) (upTime0(i) , i=1,n)
        case('DownTime')
          isPresent(7) = .true.
          n = nTrig + nBoi + nChi
          value_ = '('//trim(value)//')'
          n_ = hCount(value_)
          if(n_.ne.n) call abortExecution(13,n_,n,'DownTime')
          read(value,*) (downTime0(i) , i=1,n)
        case('Algorithm')
             isPresent(8) = .true.
             read(value,*) method
             if(method.ne.'Forward'.and.method.ne.'Backward') then
                call abortExecution(2,2,line,method)
             endif
        case('writePower')
             read(value,*), dummy
             if(dummy.ne.'.true.'.and.dummy.ne.'.false.') call abortExecution(2,3, line=line,word=value)
             read(value,*), writePower
        case('writeEnergy')
             read(value,*), dummy
             if(dummy.ne.'.true.'.and.dummy.ne.'.false.') call abortExecution(2,3, line=line,word=value)
             read(value,*), writeEnergy
        case('writeEfficiency')
             read(value,*), dummy
             if(dummy.ne.'.true.'.and.dummy.ne.'.false.') call abortExecution(2,3, line=line,word=value)
             read(value,*), writeEfficiency
        case('writeElectricRev')
             read(value,*), dummy
             if(dummy.ne.'.true.'.and.dummy.ne.'.false.') call abortExecution(2,3, line=line,word=value)
             read(value,*), writeElectricRev
        case('writeThermalRev')
             read(value,*), dummy
             if(dummy.ne.'.true.'.and.dummy.ne.'.false.') call abortExecution(2,3, line=line,word=value)
             read(value,*), writeThermalRev
        case('writeChillingRev')
             read(value,*), dummy
             if(dummy.ne.'.true.'.and.dummy.ne.'.false.') call abortExecution(2,3, line=line,word=value)
             read(value,*), writeChillingRev
        case('writeFuelCost')
             read(value,*), dummy
             if(dummy.ne.'.true.'.and.dummy.ne.'.false.') call abortExecution(2,3, line=line,word=value)
             read(value,*), writeFuelCost
        case('writeDemand')
             read(value,*), dummy
             if(dummy.ne.'.true.'.and.dummy.ne.'.false.') call abortExecution(2,3, line=line,word=value)
             read(value,*), writeDemand
        case('writeInput')
             read(value,*), dummy
             if(dummy.ne.'.true.'.and.dummy.ne.'.false.') call abortExecution(2,3, line=line,word=value)
             read(value,*), writeInput
        case('writeCosts')
             read(value,*), dummy
             if(dummy.ne.'.true.'.and.dummy.ne.'.false.') call abortExecution(2,3, line=line,word=value)
             read(value,*), writeCosts
        case('writeTrig')
             read(value,*), dummy
             if(dummy.ne.'.true.'.and.dummy.ne.'.false.') call abortExecution(2,3, line=line,word=value)
             read(value,*), writeTrig
        case('writeBoiler')
             read(value,*), dummy
             if(dummy.ne.'.true.'.and.dummy.ne.'.false.') call abortExecution(2,3, line=line,word=value)
             read(value,*), writeBoi
        case('writeChiller')
             read(value,*), dummy
             if(dummy.ne.'.true.'.and.dummy.ne.'.false.') call abortExecution(2,3, line=line,word=value)
             read(value,*), writeChi
        case(' ')
          if(verb) call warning(4,1,line=line)
        case  default
          call warning(1,1,line=line,word=keyword )
    end select
enddo

!---check if all the variablea were read---
nInp = size(isPresent)
do i = 1,nInp
    if(.not.isPresent(i)) call abortExecution(3,i)
enddo

close(genUnit)
100 format(A100)

end subroutine readGeneral
