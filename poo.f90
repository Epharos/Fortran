module Classes ! Début du module class_Point
    implicit none ! Pas d'attribution implicite

    public :: Point, printPoint, askCoords, setCoords, inf_or_equal, rangeTo, belongsTo, Collection, add, printer, intersect, &
                centerOfGravity, maxInRange, sort, sorted ! Déclaration des fonctions publiques du module

    type Point ! Créer du type (class) Point
        real :: x = 0.0, y = 0.0, z = 0.0 ! Les variables sont déclarées et assignées -> pas de constructeur implicite
    end type Point ! Fin du type

    type Collection
        type(Point), dimension(20) :: points
        integer :: lenght = 0
    end type Collection

contains

    subroutine printPoint(p) ! Fonction d'affichage
        class(Point), intent(in) :: p ! Déclaration des types des arguments ; "intent(in)" signifie que l'argument ne peut pas être modifié pendant l'execution

        print "(a, 3f5.1, a)", "(", p%x, p%y, p%z, ")" ! Affichage
    end subroutine printPoint ! Fin de la fonction

    subroutine askCoords(p) ! Fonction qui demande à l'utilisateur d'entrer les coordonnées du points
        class(Point) :: p ! Comme avant

        print *, "Entrez une position x : "
        read(*, *) p%x ! Fonction d'input
        print *, "Entrez une position y : "
        read(*, *) p%y
        print *, "Entrez une position z : "
        read(*, *) p%z
    end subroutine askCoords ! Fin de la fonction

    subroutine setCoords(p, x, y, z) ! Fonction qui permet de mettre directement des coordonnées au point
        real, intent(in) :: x, y, z
        class(Point) :: p

        p%x = x
        p%y = y
        p%z = z
    end subroutine setCoords ! Fin de la fonction

    function inf_or_equal(p, q) result(flag) ! Fonction qui détermine si un point est inférieur ou égal à un autre (TRUE) ou s'il est strictement supérieur (FALSE)
        class(Point), intent(in) :: p, q
        logical :: flag

        flag = .FALSE.

        if(p%x < q%x) then
            flag = .TRUE.
        else if(p%x == q%x) then
            if(p%y < q%y) then
                flag = .TRUE.
            else if(p%y == q%y) then
                if(p%z <= q%z) then
                    flag = .TRUE.
                end if
            end if
        end if
    end function inf_or_equal ! Fin de la fonction

    function rangeTo(p, q) result(distance) ! Fonction qui calcule la distance à un autre point
        class(Point), intent(in) :: p, q
        real :: distance, x, y, z

        x = (p%x - q%x) ** 2
        y = (p%y - q%y) ** 2
        z = (p%z - q%z) ** 2

        distance = sqrt(x + y + z)
    end function rangeTo ! Fin de la fonction

    function belongsTo(c, p) result(b) ! Fonction qui détermine si un point appartient à une collection
        class(Collection), intent(in) :: c
        class(Point), intent(in) :: p
        logical :: b
        integer :: i

        b = .false.

        do i = 1, c%lenght
            if(rangeTo(c%points(i), p) .lt. 0.00001) then
                b = .true.
            end if
       end do
   end function belongsTo ! Fin de la fonction

    subroutine add(c, p) ! Fonction qui ajoute un point à une collection
        class(Collection) :: c
        class(Point), intent(in) :: p

        if(c%lenght .lt. 20) then
            c%lenght = c%lenght + 1
            c%points(c%lenght) = p
        else
            print *, "*** Array Full ***"
        end if
    end subroutine add ! Fin de la fonction

    subroutine printer(c) ! Fonction qui affiche une collection
        class(Collection), intent(in) :: c
        integer :: i

        do i = 1, c%lenght
            call printPoint(c%points(i))
        end do
    end subroutine printer ! Fin de la fonction

    function intersect(c, d) result(cp) ! Fonction qui retourne une nouvelle collection comprennant tous les points communs
        class(Collection), intent(in) :: c, d
        type(Collection) :: cp
        integer :: i

        do i = 1, c%lenght
            if(belongsTo(d, c%points(i))) then
                call add(cp, c%points(i))
            end if
        end do
    end function intersect ! Fin de la fonction

    function centerOfGravity(c) result(cog) ! Fonction qui retourne le center de gravité de la collection
        type(Collection), intent(in) :: c
        type(Point) :: cog
        integer :: i
        real :: x = 0.0, y = 0.0, z = 0.0

        do i = 1, c%lenght
            x = x + c%points(i)%x
            y = y + c%points(i)%y
            z = z + c%points(i)%z
        end do

        x = x / c%lenght
        y = y / c%lenght
        z = z / c%lenght

        call setCoords(cog, x, y, z)
    end function centerOfGravity ! Fin de la fonction

    function maxInRange(c, n) result(i) ! Fonction qui retourne l'indice du plus grand élément d'une collection
        type(Collection), intent(in) :: c
        integer, intent(in) :: n
        integer :: i, a = 1
        i = 1

        do a = 1, n
            if(inf_or_equal(c%points(i), c%points(a))) then
                i = a
            end if
        end do
    end function maxInRange ! Fin de la fonction

    subroutine sort(c) ! Fonction qui tri une collection
        type(Collection) :: c
        integer :: i, maxIndex
        type(Point) :: temp

        do i = 1, c%lenght
            maxIndex = maxInRange(c, i)
            temp = c%points(i)
            !call printPoint(c%points(i))
            c%points(i) = c%points(maxIndex)
            c%points(maxIndex) = temp
        end do
    end subroutine sort ! Fin de la fonction

    function sorted(c) result(flag) ! Fonction qui vérifie si une collection est triée
        type(Collection), intent(in) :: c
        logical :: flag
        integer :: i
        flag = .TRUE.

        do i = 1, c%lenght - 1
            if(.not. inf_or_equal(c%points(i), c%points(i + 1))) then
                flag = .FALSE.
                EXIT
            end if
        end do
    end function sorted ! Fin de la fonction

end module Classes ! Fin du module

! ------------------------------------------------------------

program PointAndCollection
    use Classes
    implicit none

    real :: distance
    type(Point) :: p, q, g
    type(Collection) :: c
    p = Point()
    q = Point()
    c = Collection()

    call setCoords(p, 2., 5., 3.)
    call setCoords(q, 1., 3., 5.)
    call printPoint(p)
    call printPoint(q)

    if(inf_or_equal(p, q)) then
        print *, "p est inférieur ou égal à q !"
    else
        print *, "q est inférieur à p !"
    end if

    distance = rangeTo(p, q)

    print *, distance

    call add(c, p)
    call add(c, q)
    call printer(c)

    print *, "Sorting"

    call sort(c)
    call printer(c)
    g = centerOfGravity(c)
    call printPoint(g)

    print *, "Adding"

    call add(c, g)
    call printer(c)

    print *, "Sorting"

    call sort(c)
    call printer(c)
    g = centerOfGravity(c)
    call printPoint(g)


end program PointAndCollection
