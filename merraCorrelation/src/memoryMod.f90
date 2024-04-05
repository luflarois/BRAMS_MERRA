!=============================================================================================
module memoryMod
    !# Modulo memoria
    !#
    !# @note
    !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
    !#
    !# **Brief**: Modulo memoria
    !#
    !# **Documentation**: <http://brams.cptec.inpe.br/documentation/>
    !#
    !# **Author(s)**: Luiz Flavio Rodrigues **&#9993;**<luiz.rodrigues@inpe.br>
    !#
    !# **Date**: 03 September 2020 (Thursday)
    !# @endnote
    !#
    !# @changes
    !# &#9744; <br/>
    !# @endchanges
    !# @bug
    !#
    !#@endbug
    !#
    !#@todo
    !#  &#9744; <br/>
    !# @endtodo
    !#
    !# @warning
    !# Now is under CC-GPL License, please see
    !# &copy; <https://creativecommons.org/licenses/GPL/2.0/legalcode.pt>
    !# @endwarning
    !#
    
    !Use area
    use dump

    implicit none

    include "constants.f90"
    character(len=*),parameter :: sourceName='memoryMod.f90' !Name of source code
    character(len=*),parameter :: procedureName='**memoryMod**' !Name of this procedure
    !
    !Local Parameters

    !Local variables
    type st 
      character(len=25) :: name
      real :: lat 
      real :: lon 
      integer :: sCase
      integer :: px
      integer :: py
    end type st
    type(st), allocatable :: site(:)
    integer :: nSites
    integer :: nx
    integer :: ny
    real :: latIni
    real :: lonIni
    real :: dLat
    real :: dLon
    integer :: nDays
    integer :: nTimes
    real, allocatable :: var(:,:,:)
    real, allocatable :: correlation(:,:)
    logical :: inicializado
    real, allocatable :: aerodata(:,:,:)
    real, allocatable :: outData(:,:)

end module memoryMod 