module class_Person
    implicit none

    public :: Person, sayName, askForName

    type Person
        character(len = 20) :: name
    end type Person

contains
    subroutine askForName(this)
        type(Person) :: this

        write(*, *), "Entrez votre nom : "
        read(*, *) this%name
    end subroutine askForName

    subroutine sayName(this)
        type(Person), intent(in) :: this

        write(*, "(2a)") "Bonjour ", this%name
    end subroutine sayName
end module class_Person

program poo
    use class_Person
    implicit none

    type(Person) :: p = Person('')

    call askForName(p)
    call sayName(p)

end program poo
