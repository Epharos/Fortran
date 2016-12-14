program learning
implicit none
    interface
        subroutine add(a, v)
                integer, allocatable, dimension(:) :: a
                integer, intent(in) :: v
                integer :: i, l
        end subroutine add
    end interface

    integer, allocatable, dimension(:) :: ar
    integer :: v

    write(*, '(a, $)') 'Entrez une valeur v a ajouter : '
    read(*, *) v

    call add(ar, v)
    call add(ar, v)
    call add(ar, v)
    call add(ar, v)
    call add(ar, v)
    call add(ar, v)

    print *, size(ar)

end program learning

        subroutine add(a, v)
            integer, allocatable, dimension(:) :: temp, a
            integer, intent(in) :: v
            integer :: i, l

            if(allocated(a)) then

                l = size(a)

                allocate(temp(l))

                do i = 1, l
                    temp(i) = a(i) 
                end do

                deallocate(a)
                allocate(a(l + 1))

                do i = 1, l
                    a(i) = temp(i)
                end do

                a(l + 1) = v

                deallocate(temp)
            else
                allocate(a(1))
                a(1) = v
            end if

        end subroutine add
