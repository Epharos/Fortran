! ============================================================================
! Name        : plop.f90
! Author      : 
! Version     :
! Copyright   : Your copyright notice
! Description : Hello World in Fortran
! ============================================================================

! RAPPEL :
! Commencer le programme par :
! program [nom]
!   implicit none
! end program

! Faire un print :
! print *, "[texte]"

! Demander une valeur :
! read *, [variable]

! Faire un plusieurs instruction sur une ligne :
! print *, "." ; read *, n

! Faire une instruction sur plusieur lignes :
! print *, "Valeur de n", n, &
!          "Valeur de p", p

! Couper une string en plusieur lignes :
! print *, "Ceci est une long &
!          & texte (pas du tout)"

! Une variable ne peut pas ex�der 31 caract�res
! Aucune distinction entre majuscule et minuscule
! INTEGER : entier
! CHARACTER : caract�re
! LOGICAL : Bool�an (.TRUE. ou .FALSE.)
! REAL : float (7 chiffres significatifs)
! DOUBLE PRECISION : double (15 chiffres significatifs)
! COMPLEX : complexe

! PARAMETER : constance
! DIMENSION : taille d'un tableau
! SAVE : statique
! EXTERNAL : proc�dure externe
! INTRINSIC : proc�dure intrins�que

! D�claration :
! type[, attributs ::] [nom]
! INTEGER, SAVE :: [nom]
! INTEGER :: [nom]
! INTEGER [nom]
! SAVE [nom]

! Pour un string :
! CHARACTER(LEN = N) [nom]
! Par d�faut LEN = 1

! Par d�faut un chiffre est en base 10
! La base 2 se d�clare comme : B'101'
! La base 8 se d�clare comme : O'752'
! La base 16 se d�clare comme : Z'F3B'

! Les chiffres r�els (float) doivent obligatoire comport� le point d�cimal :
! 1.
! 3.14
! Les valeurs commencent par 0 peuvent l'omettre :
! .66
! Ils peuvent �galement comporter E
! 3.14 <-> 314E-2

! Les chiffres r�els (double) doivent s'�crire :
! 0D0 <-> 0d0
! 3.14d0 <-> 314d-2

! Les complexes s'�crivent :
! (r, i) [r et i sont des flottants]
! 2.5 + i s'�crira :
! (2.5 + 1.)

! Les substrings de string se font comme suit :
! [var](n:m) avec n �tant l'index du caract�re de d�but (+1) et m l'index de fin (+1)

! L'initialisation des variables se fait comme suit :
! DATA list/valeur/[, ...,  list/valeur/]
! Exemple :
! INTEGER A, B
! REAL PI
! LOGICAL run
! DATA A, PI/3, 3.14/
! DATA run, PI/.TRUE., 7/

! Mais il est �galement possible de faire :
! INTEGER A/3/, B/7/
! REAL PI/3.14/
! LOGICAL run/.TRUE./

! Ou bien :
! INTEGER :: A = 3
! REAL :: PI = 3.14

! Pour d�clarer une constante :
! LOGICAL, PARAMETER :: V = .TRUE., F = .FALSE.
! DOUBLE PRECISION :: PI, EXP
! PARAMETER (PI = 3.14d0, EXP = 2.17d0)

! Permettre � deux variables d'avoir la m�me case m�moire au sein de la m�me unit� du programme :
! EQUIVALENCE (var1, var2, ..., varn)

! Op�rateurs logiques (nombres) :
! .LT. ou <
! .LE. ou <=
! .EQ. ou ==
! .NE. ou /= (diff�rent)
! .GT. ou >
! .GE. ou >=

! Op�rateurs logiques (logique) :
! .NOT.
! .AND.
! .OR.
! .EQV. (�quivalence logique)
! .NEQV. (non �quivalence logique)

! Concat�nation :
! c1 // c2

! Attention :
! CHARACTER(LEN = 2) :: c = 'a'
! c = c // 'b' Donne une erreur
! c = TRIM(c) // 'b' Valide

! Conditions :
! [bloc :] IF([exp]) THEN
!            [exec]
!          ELSE IF(exp) THEN [bloc]
!            ...
!          ELSE [bloc]
!            ...
!          END IF [bloc]

! IF(exp) exec

! [bloc] SELECT CASE(select)
!   CASE(v1 [, v2, ..., vn]) [bloc]
!       instruction
!   CASE DEFAULT [bloc]
! END SELECT [bloc]

! Boucle Do For :
! [bloc] DO i=1, max, [pas]
!   inst
! END DO [bloc]

! Boucle Do While :
! [bloc] DO WHILE (exp)
!   inst
! END DO [bloc]

! Equivalent au break : EXIT
! Equivalent du continue : CYCLE

! Boucle Do infinie :
! DO
!   inst (penser au EXIT)
! END DO

! Utiliser un SUBROUTINE pour une fonction sans retour :
! SUBROUTINE NAME(ARGS)
!   INSTRUCTIONS
! END SUBROUTINE NAME
! Appel : call NAME(args)

! Utiliser une FUNCTION pour une fonction avec retour :
! FUNCTION NAME(ARGS)
!   inst
!   return x
! END FUNCTION NAME

! Dans les deux cas penser � red�finir les variables dans la fonction

program plop
    implicit none

    integer price/0/, user/0/, tour/0/
    integer, parameter :: inf = 5000, sup = 15000
    character(5) :: infs = '5000', sups = '15000', prices, tours
    integer * 4 time(3)

    call itime(time)
    call srand((time(1) + time(2)) * time(3) * 10000)

    price = int(rand() * (sup - inf) + inf)

    write(prices, '(I0)') price

    print "(a, i5, a, i4)", "Valeur de price ", price, " et ", inf

    do while (user .ne. price)
        write(*, *), "Entrez une valeur entre " // trim(infs) // " et " // trim(sups)
        read *, user

        if (user .lt. inf .or. user .gt. sup) then
            print *, "Relisez la r�gle !"
            cycle
        end if

        tour = tour + 1

        if (user .lt. price) then
            print *, "C'est plus"
        else if (user .gt. price) then
            print *, "C'est moins"
        end if

        print *, "--------"
    end do

    write(tours, '(I0)') tour
    write(*, *), "Bien jou� ! Le prix �tait donc de " // trim(prices) // ", que vous avez trouv� en " // trim(tours) // " tours"
    print *, "Au revoir"
end program
