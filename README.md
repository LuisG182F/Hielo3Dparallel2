!===========================================================================================================
!Programa que calcula crecimento de grano con anisotropia de la energia con distintas formulas y
!particulas de diferentes tamaños, crea una imagen con mascara y otra en de distintas caras.
!este programa puede hacer simulaciones en 2D  si uno coloca un ancho 5 para particulas de radio 0 , 	
!un ancho 7 para particulas de radio 1, y un ancho de 9 con particulas de radio 2 
!Simulaciones 2D o 3D con particuals de diferente radio 0,1 y 2 debe ponerse un ancho mayor de 9. 
!El programa tambien permite empezar con un policristal de granos de tamaño 1 pixel cubico o bien de un policristal con granos grandes. 
!SE debe dar la concentracion de particulas en función del volunens y el programa calcula las particuals que deben colocarse para generar 
!esa concentracion volumetrica 
!El programa debe compilarse con varios modulos ya que cada uno ejecuta muchas funciones

!
!Compile:
!ifort -o [nombre] presicion.f90 hielonn.f90 precipitado.f90 radio0.f90 radio1.f90 radio2.f90 cal_radio.f90 poli_3D_Q.f90
!gfortran -o fenix presicion.f90 hielonn.f90 precipitado.f90 radio0.f90 radio1.f90 radio2.f90 cal_radio.f90 poli_3D_Q.f90
!cd c:\projects\programas\policristal
!
!Run:
!./[nombre].exe

!----------------------------VERSION LUIS 1.0
!===========================================================================================================
# Hielo3Dparallel
# Hielo3Dparallel2
