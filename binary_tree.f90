module binary_tree

implicit none

type node
  integer               :: n       ! number of data
  real                  :: keyr    ! real key

!  integer, allocatable  :: vali(:) ! array to store integer data
!  real,    allocatable  :: valr(:) ! array to store real data

  type (node), pointer  :: left
  type (node), pointer  :: right
  type (node), pointer  :: parent

end type node

type (node), pointer    :: root 

contains
!
!******************************************************************************
!                       SUBROUTINE INSERT_NODE                                *
!******************************************************************************
!     Subroutine insert_node inserts a node in the binary tree.               *
!******************************************************************************
!
subroutine insert_node(t)

implicit none

!...dummy variables
type (node)             :: t

!... local variables
type (node), pointer    :: x,y
type (node), pointer    :: p

!
!------------------------------------------------------------------------------
!
!... if tree is empty
if (.not.associated(root)) then
  allocate(root); 
  root%n    = t%n
  root%keyr = t%keyr
!  if (root%n > 0 ) then
!    allocate(root%valr(1:root%n))
!    root%valr(:) = t%valr(:)
!  end if
  root%left  => null()
  root%right => null()
  return
end if

!... search the location
x => root
y    => null()

do while (associated(x))
  y => x
  if (t%keyr <= x%keyr) then
    x => x%left
  else
    x => x%right
  end if
end do

!... set the node
allocate(p);
p%n = t%n
p%keyr = t%keyr
!if (p%n > 0 ) then
!  allocate(p%valr(1:p%n))
!  p%valr(:) = t%valr(:)
!end if
p%parent => y
p%right  => null()
p%left   => null()
if (t%keyr <= y%keyr) then
  y%left => p
else
  y%right => p
end if

end subroutine insert_node
!
!******************************************************************************
!                       SUBROUTINE FIND_NODE                                  *
!******************************************************************************
!     Subroutine find_node finds the node that matches the key.               *
!******************************************************************************
!
recursive subroutine find_node(r,x)

implicit none

!... dummy variables
real                 :: r
type (node), pointer :: x

!
!------------------------------------------------------------------------------
!
if (.not.associated(x)) return

if (x%keyr == r) then
  return
else if (r < x%keyr) then
  x => x%left
  call find_node(r,x)
else
  x => x%right
  call find_node(r,x)
end if

return
end subroutine find_node

!
!******************************************************************************
!                       SUBROUTINE PRINT_TREE                                 *
!******************************************************************************
!     Subroutine print_tree prints out the data stored in the tree.           *
! Input is root to print full tree or some node of tree to print subtree.     *
!******************************************************************************
!
recursive subroutine print_tree(x)

implicit none

!... dummy variables
type (node), pointer    :: x

!
!------------------------------------------------------------------------------
!
if (.not.associated(x)) then
  return
else
  call print_node(x)
end if

if (associated(x%left))  call print_tree(x%left)
if (associated(x%right)) call print_tree(x%right)

return
end subroutine print_tree
!
!******************************************************************************
!                       SUBROUTINE PRINT_NODE                                 *
!******************************************************************************
!     Subroutine print_node prints out the data stored in the node.           *
!******************************************************************************
!
subroutine print_node(x)

implicit none

!... dummy variables
type (node), pointer    :: x

!... local variables
integer                 :: i

!
!------------------------------------------------------------------------------
!
write(*,2000) x%keyr
if (x%n == 0 ) return 
!if (allocated(x%vali)) then
!  do i=1,x%n
!    write(*,1000) x%vali(i)
!  end do
!else if (allocated(x%valr)) then
!  do i=1,x%n
!    write(*,2000) x%valr(i)
!  end do
!else
!  write(*,9000)
!  stop
!end if

1000 format(2x,I8)
2000 format(2x,f16.6)
9000 format(/,2x,'Neither real nor integer data found!')

end subroutine print_node
!
!******************************************************************************
!                       SUBROUTINE MIN_TREE                                   *
!******************************************************************************
!     Subroutine min_tree finds the minimum of the values of keyr.            *
!******************************************************************************
!
subroutine min_tree(x)

implicit none

!... dummy variables
type (node), pointer    :: x

!... local variables

!
!------------------------------------------------------------------------------
!

do while(associated(x%left))
  x => x%left
end do

end subroutine min_tree
!
!******************************************************************************
!                       SUBROUTINE MAX_TREE                                   *
!******************************************************************************
!     Subroutine max_tree finds the maximum of the values of keyr.            *
!******************************************************************************
!
subroutine max_tree(x)

implicit none

!... dummy variables
type (node), pointer    :: x

!... local variables

!
!------------------------------------------------------------------------------
!

do while(associated(x%right))
  x => x%right
end do

end subroutine max_tree
!
!******************************************************************************
!                       SUBROUTINE SUCCESSOR_TREE                             *
!******************************************************************************
!     Subroutine successor_tree finds the next bigger element of tree         *
!  Input is a node and not a value.                                           *
!******************************************************************************
!
subroutine successor_tree(x)

implicit none

!... dummy variables
type (node), pointer    :: x

!... local variables
type (node), pointer    :: y
!
!------------------------------------------------------------------------------
!

!... if right child exist then max of right element is successor
if (associated(x%right)) then
  x => x%right
  call min_tree(x)
  return
end if

!... else
y => x%parent
do while (associated(y) .and. associated(y%right,x))
  x => y
  y => x%parent
end do
x => y

return
end subroutine successor_tree
!
!******************************************************************************
!                       SUBROUTINE PREDECESSOR_TREE                           *
!******************************************************************************
!     Subroutine predecessor_tree finds the next smaller element of tree.     *
! Input is a node and not the value.                                          *
!******************************************************************************
!
subroutine predecessor_tree(x)

implicit none

!... dummy variables
type (node), pointer    :: x

!... local variables
type (node), pointer    :: y
!
!------------------------------------------------------------------------------
!

!... if left child exist then min of left element is predecessor
if (associated(x%left)) then
  x => x%left
  call max_tree(x)
  return
end if

!... else
y => x%parent
do while (associated(y) .and. associated(y%left,x))
  x => y
  y => x%parent
end do
x => y

return
end subroutine predecessor_tree 
!
!******************************************************************************
!                       SUBROUTINE DELETE_NODE                                *
!******************************************************************************
!     Subroutine delete_node deletes a node in the tree. If the node has no   *
! child then it is simply eliminated. If it has only one child, that child is *
! linked to parent directly. If node has both right and left child we find its*
! successor and link it place of given node.                                  *
! ** If a node has two children then its successor has no left child!!        *
!******************************************************************************
!
subroutine delete_node(z)

implicit none

!... dummy variables
type (node), pointer    :: z

!... local variables
type (node), pointer    :: y

!
!------------------------------------------------------------------------------
!
if (.not.associated(z)) then
  write(*,'(A)') "Node to be deleted doesn't exist!"
end if

if (.not.associated(z%left) .and. .not.associated(z%right)) then
  write(*,*) "1"
  y => z%parent
  if (associated(y%left ,z)) y%left  => null()
  if (associated(y%right,z)) y%right => null()
  deallocate(z)
else if (.not.associated(z%left)) then
  write(*,*) "2"
  y => z%right
  y%parent => z%parent
  if (associated(z%parent%right,z)) z%parent%right => y
  if (associated(z%parent%left ,z)) z%parent%left  => y
  deallocate(z)
else if (.not.associated(z%right)) then
  write(*,*) "3"
  y => z%left
  y%parent => z%parent
  if (associated(z%parent%right,z)) z%parent%right => y
  if (associated(z%parent%left ,z)) z%parent%left  => y
  deallocate(z)
else ! have both children
  write(*,*) "4"
  y => z
  call successor_tree(y) ! y will have no left child
write(*,*) "y % keyr =",y%keyr
  y%right%parent => y%parent
  if (associated(y%parent%right,y)) y%parent%right => y%right
  if (associated(y%parent%left ,y)) y%parent%left  => y%right
! copy the data
  z%keyr = y%keyr
!  if (y%n > 0 ) then
!    if (allocated(z%valr)) deallocate(z%valr)
!    z%n = y%n
!    allocate(z%valr(1:z%n))
!    z%valr(1:z%n) = y%valr(1:y%n)
!  end if
  deallocate(y)
end if

return
end subroutine delete_node
  

end module binary_tree

!***********************DUMMY TEST ROUTINES************************************
program main

use binary_tree

implicit none

type (node)            :: x
type (node), pointer   :: y
integer                :: i
integer                :: ar(1:12)

ar = (/15,5,16,3,12,20,10,13,18,23,6,7/)
do i=1,12
  x%n    = 0
  x%keyr = ar(i)
!  allocate(x%valr(x%n))
!  x%valr = ar(i)

  call insert_node(x)
!  deallocate(x%valr)

end do

!... add 7
!x%n=0; x%keyr = 7;
!call insert_node(x)
!... add 2
!x%keyr = 2
!call insert_node(x)

call print_tree(root)
y => root
call min_tree(y)
write(*,*) "Min tree value =",y%keyr
y => root
call max_tree(y)
write(*,*) "Max tree value =",y%keyr
y => root
call find_node(5.0,y)
write(*,*) "Search for 5 : y%keyr =",y%keyr
call successor_tree(y)
write(*,*) "successor for 5 =",y%keyr
y=>root
call find_node(5.0,y)
call delete_node(y)
call print_tree(root)



end program main
  
