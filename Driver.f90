!==============================================================================!
  program Driver
!------------------------------------------------------------------------------!
!   Program which demonstrates how to write a binary VTU file for two cells    !
!                                                                              !
!     10-------11------12                                                      !
!     /|       /|      /|                                                      !
!    / |      / |     / |                                                      !
!   7--------8-------9  |                                                      !
!   |  |     |  |    |  |                                                      !
!   |  4-----|--5----|--6                                                      !
!   | /      | /     | /                                                       !
!   |/       |/      |/                                                        !
!   1--------2-------3                                                         !
!                                                                              !
!------------------------------------------------------------------------------!
  implicit none
!------------------------------[Local parameters]------------------------------!
  integer, parameter           :: IP = 8         ! integer precision
  integer, parameter           :: RP = 8         ! real precision
  integer, parameter           :: SP = 4         ! single precision
  character(len= 1), parameter :: LF = char(10)  ! line feed
  character(len= 0), parameter :: IN_0 = ''      ! indentation levels
  character(len= 2), parameter :: IN_1 = '  '
  character(len= 4), parameter :: IN_2 = '    '
  character(len= 6), parameter :: IN_3 = '      '
  character(len= 8), parameter :: IN_4 = '        '
  character(len=10), parameter :: IN_5 = '          '
  integer(IP),       parameter :: VTK_HEXAHEDRON = 12
  integer(IP),       parameter :: N_NODES = 12
  integer(IP),       parameter :: N_CELLS =  2
!-----------------------------------[Locals]-----------------------------------!
  integer(SP)       :: data_size              ! should be SP no matter what
  integer(IP)       :: c, n, fu, cell_offset
  real(RP)          :: x(12) = 0.0, y(12) = 0.0, z(12) = 0.0
  integer(IP)       :: conn(8, N_CELLS)
  character(len=80) :: str1, str2
!==============================================================================!

  open(newunit = fu,               &
       file    = 'binary.vtu',     &
       status  = 'replace',        &
       form    = 'unformatted',    &
       access  = 'stream')

  !-------------------------------------------!
  !   Set grid connectivity and coordinates   !
  !-------------------------------------------!
  conn = reshape( (/ 1,  2,  5,  4,  7,  8, 11, 10,      &
                     2,  3,  6,  5,  8,  9, 12, 11  /), shape(conn) )
  x(2:11:3)=2.0;  x(3:12:3)=4.0;  y(4:6)=1.0;  y(10:12)=1.0;  z(7:12)=0.8;

  !------------!
  !   Header   !
  !------------!
  write(fu) IN_0 // '<?xml version="1.0"?>' // LF
  write(fu) IN_0 // '<VTKFile type="UnstructuredGrid" version="0.1" ' //  &
                    'byte_order="LittleEndian">'                      // LF
  write(fu) IN_1 // '<UnstructuredGrid>' // LF
  write(str1, '(i0.0)') N_NODES
  write(str2, '(i0.0)') N_CELLS
  write(fu) IN_2 // '<Piece NumberOfPoints="' // trim(str1) //  &
                         '" NumberOfCells ="' // trim(str2) // '">' // LF

  !---------------------!
  !   Point data info   !
  !---------------------!
  write(fu) IN_3 // '<Points>' // LF
  write(str1, '(i1)')   0                          ! data_offset
  write(str2, '(i0.0)') RP * 8                     ! real precision
  write(fu) IN_4 // '<DataArray type="Float' // trim(str2) // '"' //  &
                    ' NumberOfComponents="3"'                     //  &
                    ' format="appended"'                          //  &
                    ' offset="' // trim(str1) // '">' // LF
  write(fu) IN_4 // '</DataArray>' // LF
  write(fu) IN_3 // '</Points>'    // LF

  !--------------------!
  !   Cell data info   !
  !--------------------!
  write(fu) IN_3 // '<Cells>' // LF

  ! Connectivity
  write(str1, '(i0.0)') SP + N_NODES * 3 * RP         ! data_offset
  write(str2, '(i0.0)') IP * 8                        ! integer precision
  write(fu) IN_4 // '<DataArray type="Int' // trim(str2) // '"' //  &
                    ' Name="connectivity"'                      //  &
                    ' format="appended"'                        //  &
                    ' offset="' // trim(str1) // '">' // LF
  write(fu) IN_4 // '</DataArray>' // LF

  ! Offsets
  write(str1, '(i0.0)') SP + N_NODES * 3 * RP +   &
                        SP + N_CELLS * 8 * IP         ! data_offset
  write(str2, '(i0.0)') IP * 8                        ! integer precision
  write(fu) IN_4 // '<DataArray type="Int' // trim(str2) // '"' //  &
                    ' Name="offsets"'                           //  &
                    ' format="appended"'                        //  &
                    ' offset="' // trim(str1) // '">' // LF
  write(fu) IN_4 // '</DataArray>' // LF

  ! Types
  write(str1, '(i0.0)') SP + N_NODES * 3 * RP +  &
                        SP + N_CELLS * 8 * IP +  &
                        SP + N_CELLS * IP             ! data_offset
  write(str2, '(i0.0)') IP * 8                        ! integer precision
  write(fu) IN_4 // '<DataArray type="Int' // trim(str2) // '"' //  &
                    ' Name="types"'                             //  &
                    ' format="appended"'                        //  &
                    ' offset="' // trim(str1) // '">' // LF
  write(fu) IN_4 // '</DataArray>' // LF

  write(fu) IN_3 // '</Cells>' // LF
  write(fu) IN_2 // '</Piece>'             // LF
  write(fu) IN_1 // '</UnstructuredGrid>'  // LF

  !---------------------!
  !   Append all data   !
  !---------------------!
  write(fu) IN_0 // '<AppendedData encoding="raw">' // LF
  write(fu) '_'

  !----------------!
  !   Point data   !
  !----------------!
  data_size = N_NODES * 3 * RP         ! three coordinates for each node
  write(fu) data_size
  do n = 1, N_NODES
    write(fu) x(n), y(n), z(n)
  end do

  !---------------!
  !   Cell data   !
  !---------------!

  ! Connectivity
  data_size = N_CELLS * 8 * RP         ! eight nodes for cell
  write(fu) data_size
  do c = 1, N_CELLS
    write(fu) conn(1,c)-1, conn(2,c)-1, conn(3,c)-1, conn(4,c)-1
    write(fu) conn(5,c)-1, conn(6,c)-1, conn(7,c)-1, conn(8,c)-1
  end do

  ! Offsets
  data_size = N_CELLS * IP         ! one offset for each cell
  write(fu) data_size
  cell_offset = 0
  do c = 1, N_CELLS
    cell_offset = cell_offset + 8
    write(fu) cell_offset
  end do

  ! Types
  data_size = N_CELLS * IP     ! one type for each cell
  write(fu) data_size
  do c = 1, N_CELLS
    write(fu) VTK_HEXAHEDRON
  end do

  !------------!
  !            !
  !   Footer   !
  !            !
  !------------!
  write(fu) LF // IN_0 // '</AppendedData>' // LF
  write(fu)       IN_0 // '</VTKFile>'      // LF

  print *, '# File binary.vtu was created'

  close(fu)

  end
