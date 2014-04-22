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
!>\file readTrigen.f90
!>\brief Reads Trigeneration.inp file
!>\author 
!>     Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief Reads Trigeneration.inp file
!>\details 
!> This subroutine reads the file "Trigeneration.inp". The procedure looks for each specific entry
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


subroutine readTrigen

!---Declare Unit usage---
use shared
use inputVar
use fileTools
use cmdVar
use interfaces
use myArithmetic

implicit none

!---Declare Local Variables---
integer                 :: genUnit = 106
character(len=50)       :: inputFile = './Input/Cogen.inp'
logical                 :: filePresent
character(len=lword)    :: buffer,keyword, value,vector,elements,value_
integer                 :: firstLine, i, nInp, line, nRow, n1, n2, x, j, il, nl
logical,dimension(17)   :: isPresent = .false.
integer, dimension(100) :: dummy
integer                 :: error
integer                 :: n, nt, nee, net, nec, np
character(len=20), dimension(5)                :: param
real(kind = prec), allocatable, dimension(:,:) :: matrix

!---Check File Presence---
inquire(file = inputFile, exist = filePresent)
if(.not.filePresent) then
   call abortExecution(1,2)
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

!---Look for the the "Number" entry that is neede for most of the other entries---
buffer = 'Number'
call iFindEntry(buffer,1,genUnit,.true.,dummy(1),isPresent(1))
nTrig = dummy(1)
if (nTrig.le.0) return
if(.not.isPresent(1)) call abortExecution(3,5)
call allocateVar(1)

!--added because some entries are deactivated.
isPresent(3) = .true.
isPresent(6) = .true.
isPresent(7) = .true.
isPresent(11) = .true.
isPresent(14) = .true.
iPrio(1) = .false.

!---read the input list---
line = firstLine
do 
    call readKeyword(genUnit,.false., keyword,value,error,nl)
    line = line + nl
    if(error.eq.1) call abortExecution(0,2,nl)
    select case(keyword)
       case('end')
          exit
       case('Technology')
          read(value,*) (tecT(i), i=1,nTrig)
          isPresent(15) = .true.
       case('Power')
          read(value,*) (pMaxT(i), i=1,nTrig)
          isPresent(2) = .true.
!       case('Priority')
!          read(value,*) (TrigPriority(i), i=1,nTrig)
!          iPrio(1) = .true.
!       case('DegradationRate')
!          read(value,*) (degRateT(i), i=1,nTrig)
!          isPresent(3) = .true.
       case('FuelCost')
          read(value,*) (fuelCostT(i), i=1,nTrig)
          isPresent(4) = .true.
       case('FuelLHV')
          read(value,*) (fuelLHVT(i), i=1,nTrig)
          isPresent(5) = .true.
!       case('Investment')
!          read(value,*) (invT(i), i=1,nTrig)
!          isPresent(6) = .true.
!       case('Lifetime')
!          read(value,*) (lifeT(i), i=1,nTrig)
!          isPresent(7) = .true.
       case('OnOffCost')
          read(value,*) (fireCostT(i), i=1,nTrig)
          isPresent(8) = .true.
       case('OeMCost')
          read(value,*) (maintCostT(i), i=1,nTrig)
          isPresent(9) = .true.
       case('SetPoint')
          isPresent(10) = .true.
          value_ = value
          do i=1,nTrig
              nSpT(i) = hCount(value_)
              n2 = index(value_,')') + 1
              value_ =  value_(n2 + 1:)
          enddo
          call allocateVar(2)
          do i = 1, nTrig
             n1 = index(value,'(') + 1
             n2 = index(value,')') - 1
             vector = trim(value(n1:n2))
             read(vector,*) (spT(j,i), j=1,nSpT(i))
             value =  value(n2 + 2:)
          enddo
!       case('Size')
!           value_ = value
!           isPresent(11) = .true.
!           do i=1,nTrig
!               nSizeT(i) = hCount(value_)
!               n2 = index(value_,')') + 1
!               value_ =  value_(n2 + 1:)
!           enddo
!           call allocateVar(3)
!           do i = 1, nTrig
!              n1 = index(value,'(') + 1
!              n2 = index(value,')') - 1
!              vector = trim(value(n1:n2))
!              read(vector,*) (kSizeT(i,j), j=1,nSizeT(i))
!              value =  value(n2 + 2:)
!           enddo
       case('ElettrEfficiency')
              isPresent(12) = .true.
              backspace(genUnit)
              line = line - 1
              do i = 1,nTrig
                  nEtaElT(i) = vCount(genUnit,.false.)
              enddo
              j = sum(nEtaElT)
              call allocateVar(4)
              call rewUnit(genUnit,j)
              do i = 1, nTrig
                 etaElT(:,:,i) =  dmatrixRead(genUnit,nEtaElT(i),2)
              enddo
              line = line + j
       case('ThermalEfficiency')
              isPresent(13) = .true.
              backspace(genUnit)
              line = line - 1
              do i = 1,nTrig
                  nEtaThT(i) = vCount(genUnit,.false.)
              enddo
              j = sum(nEtaThT)
              call allocateVar(5)
              call rewUnit(genUnit,j)
              do i = 1, nTrig
                 etaThT(:,:,i) =  dmatrixRead(genUnit,nEtaThT(i),2)
              enddo
              line = line + j
!       case('ChillingEfficiency')
!              isPresent(14) = .true.
!              backspace(genUnit)
!              line = line - 1 
!              do i = 1,nTrig
!                  nEtaChT(i) = vCount(genUnit,.false.)
!              enddo
!              j = sum(nEtaChT)
!              call allocateVar(6)
!              call rewUnit(genUnit,j)
!              do i = 1, nTrig
!                 etaChT(:,:,i) =  dmatrixRead(genUnit,nEtaChT(i),2)
!              enddo
!             line = line + j
       case('MinUpTime')
              read(value,*) (minUpTimeT(i), i=1,nTrig)
              isPresent(16) = .true.
       case('MinDownTime')
              read(value,*) (minDownTimeT(i), i=1,nTrig)
              isPresent(17) = .true.
       case('Number')
              continue
       case(' ')
            if(verb) call warning(4,2,line=line)
       case('TempCorrection')
            read(value,*) (param(i), i=1,5)
            do i=1,5
               select case(param(i))
                  case('temp')
                     nt = i
                  case('etaEl')
                     nee = i
                  case('etaTh')
                     net = i
                  case('etaCh')
                     nec = i
                  case('pmax')
                     np = i
               end select
            enddo
            do i=1,nTrig
               ntcT(i) = vCount(genUnit,.false.)
            enddo
            call allocateVar(23,maxval(ntcT))
            allocate(matrix(maxval(ntcT),5))
            n = sum(ntcT)
            call rewUnit(genUnit,n)
            do i=1,nTrig
               matrix = rNaN(rVal)
               matrix = dmatrixRead(genUnit,ntcT(i),5)
               tempCorrT(:,1,i) = matrix(:,nt)
               tempCorrT(:,2,i) = matrix(:,nee)
               tempCorrT(:,3,i) = matrix(:,net)
               tempCorrT(:,4,i) = matrix(:,nec)
               tempCorrT(:,5,i) = matrix(:,np)
            enddo
            deallocate(matrix)
       case('PresCorrection')
            read(value,*) (param(i), i=1,5)
            do i=1,5
               select case(param(i))
                  case('pres')
                     nt = i
                  case('etaEl')
                     nee = i
                  case('etaTh')
                     net = i
                  case('etaCh')
                     nec = i
                  case('pmax')
                     np = i
               end select
            enddo
            do i=1,nTrig
               npcT(i) = vCount(genUnit,.false.)
            enddo
            call allocateVar(24,maxval(npcT))
            allocate(matrix(maxval(npcT),5))
            n = sum(npcT)
            call rewUnit(genUnit,n)
            do i=1,nTrig
               matrix = rNaN(rVal)
               matrix = dmatrixRead(genUnit,npcT(i),5)
               presCorrT(:,1,i) = matrix(:,nt)
               presCorrT(:,2,i) = matrix(:,nee)
               presCorrT(:,3,i) = matrix(:,net)
               presCorrT(:,4,i) = matrix(:,nec)
               presCorrT(:,5,i) = matrix(:,np)
            enddo
            deallocate(matrix)
       case('AltCorrection')
            read(value,*) (param(i), i=1,5)
            do i=1,5
               select case(param(i))
                  case('alt')
                     nt = i
                  case('etaEl')
                     nee = i
                  case('etaTh')
                     net = i
                  case('etaCh')
                     nec = i
                  case('pmax')
                     np = i
               end select
            enddo
            do i=1,nTrig
               nacT(i) = vCount(genUnit,.false.)
            enddo
            call allocateVar(25,maxval(nacT))
            allocate(matrix(maxval(nacT),5))
            n = sum(nacT)
            call rewUnit(genUnit,n)
            do i=1,nTrig
               matrix = rNaN(rVal)
               matrix = dmatrixRead(genUnit,nacT(i),5)
               altCorrT(:,1,i) = matrix(:,nt)
               altCorrT(:,2,i) = matrix(:,nee)
               altCorrT(:,3,i) = matrix(:,net)
               altCorrT(:,4,i) = matrix(:,nec)
               altCorrT(:,5,i) = matrix(:,np)
            enddo
            deallocate(matrix)
       case('PEF')
           kPEC(2) = .true.
           read(value,*) (pefT(i),i=1,nTrig)
       case('onOffPec')
           kPEC(4) = .true.
           read(value,*) (pecOnT(i),i=1,nTrig)
       case default
               if(.not.silent) call warning(1,2,line=line,word=keyword)
    end select
enddo
do i = 1, nTrig
   etaChT(:,:,i) =  zero
enddo
!---check if all the variablea were read---
nInp = size(isPresent)
do i = 1,nInp
    if(.not.isPresent(i)) call abortExecution(4,i)
enddo

close(genUnit)
100 format(A500)

end subroutine readTrigen
