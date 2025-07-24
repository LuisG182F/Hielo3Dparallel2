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

program grano_particulas_movil

use omp_lib

use presicion

use hielonn

use cal_radio

use precipitado1

use radio0

use radio1

use radio2

use Montecarlo

implicit none

! Definir N como una constante
integer, parameter :: N = 500

! Usar N para definir las dimensiones de las matrices
integer, parameter :: filas = N, columnas = 10, ancho = N, stoptime = 10000000
character(len=60), parameter :: radio_fijo = 'n'



!pregunta si quieremos un radio de particula fija- radio_fijo = 'n' debemos poner ancho=9, radio_fijo = 's' hay que ver que tipo de particula vamos a generar!!!) 

!ancho 5 para radio 0
!ancho 7 para radio 1
!ancho 9 para radio 2
integer,parameter                       ::   paso=500000,paso_area=10000,paso_luz=1000000
!efectos probailistico de las particulas_ fraccion de particulas moviles
real(pr),parameter                      ::   casino=1

real(pr),parameter                      ::   pi=3.14159
!factores relacionados con la funcion de energia de activacion y superficial
real(pr),parameter                      ::   beta=1.0,ang_l=20.0
!cuanto mas chico es beta menor es la influencia de la temperatura
integer                                 ::   delta,i,j,k,s, BG_part0,BG_part1,BG_part2,BG_part,iter, temp
integer                                 ::   time,swap,ii,jj,kk,w,num,time0
!factores relacionados con la funcion de energia superficial y con la posibilidad de rotacion de los granos
integer                                 ::   energy, Nhilos,thread_id,inicio,fin,tam_bloque
integer                                 ::   Q,numero_g,u, materia, cont_radio
integer                                 ::   ms,veci,bordei,centro,bordeCT
integer                                 ::   matrix,radius,mat_total1
real(pr)                                ::   radio_g,angulote, fraccion_v
real(pr)                                ::   difa,area_g,fraccion_v0,fraccion_v1,fraccion_v2
integer, allocatable      ::   c(:,:,:)

integer, allocatable                    ::   vect(:), orient1(:)

character(len=200)                      ::   archivo,archivo1,precip
character(len=60)                       ::   timew,volumen,volumen1,volumen2,volumen3,respuesta,conce,distri_b
character(len=10000)                    ::   directorio,directorio1
integer, allocatable                    ::   bloques(:,:)
                                                               
call rand0

!parametros para arreglar warnings
veci=0
bordect=0
matrix=0
centro=0
bordei=0
u=1
inicio=0
fin=0

allocate(vect(filas*columnas*ancho))
allocate(orient1(filas*columnas*ancho))
allocate(c(filas, columnas, ancho))

Q = ancho * filas * columnas    



Write( volumen1,'(I10)' )  filas
Write( volumen2,'(I10)' )  columnas
Write( volumen3,'(I10)' )  ancho



volumen = 'output/' // trim(adjustl(volumen1)) // 'x' // &
          trim(adjustl(volumen2)) // 'x' // trim(adjustl(volumen3)) // '_' // &
          trim(adjustl(respuesta)) // '.txt'

!respuesta
!n es cuando se parte de un cristal inicial generado grande
!s fabrico cristal de cero

!enegy
!1=logaritmo, 2=senusoidal ice, 3=uniforme, 4=lineal, 5=lineal ice


!precipitados 
!pi con precipitados
!puro sin precipitados

!fración de concentració de precipitados por 100%. 

!distri_b es como vamos a distribuir a las particulas
!distri_b = 'uniforme' (es uniforme)  o distri_b = 'unifor' (es random)


respuesta = 'n'

distri_b = 'uniform'

time0 = 1000000
energy = 3
precip ='pii'
          
fraccion_v0 = 0.3_pr                                      !fraccion de materia 0
fraccion_v1 = 1_pr                                        !fraccion de materia 1 
fraccion_v2 = 1.4_pr                                       !fraccion de materia 2
fraccion_v=fraccion_v1

directorio = 'output/'
directorio1 = 'inicial/'


!/home/pastor/Documentos/Pastor/2022/Simulaciones/poli_3_3D                            


if (radio_fijo=='s') radius =1 
                                      


!******************************************************
!
!   Lectura o Generacion de archivo inicial
!
!******************************************************


if (respuesta=='n') then



!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+
!+     Lectura de policristal inicial
!+            
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

     Write( timew,'(I10)' ) time0
     !archivo = trim(adjustl(directorio1)) //trim(adjustl(volumen))// '_' // trim(adjustl(timew))// ' ' //'.txt'
     
     !open(unit=11,file=archivo)
     archivo = 'output/' // trim(adjustl(volumen1)) // 'x' // trim(adjustl(volumen2)) // 'x' // &
          trim(adjustl(volumen3)) // '_' // trim(adjustl(respuesta)) // '.txt'
    open(unit=11,file=archivo)
     
     do k=1,ancho
        do i=1,filas
           do j=1,columnas
              s = j + ( i-1 ) * columnas + ( k-1 ) * ( filas*columnas )
              read(11,*) c(i,j,k),orient1(s)
                if (c(i,j,k)==0) then
                    !print*,'hay particulas'
                else
                end if
           end do
        end do
     end do
     close(11)
else
    

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!+
!+     Generación del policristal inicial 
!+              
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!!$omp parallel do
do k=1,Q
orient1(k)=0
end do
!!omp end parallel do

!!$omp parallel do collapse(3) private(s)
do i=1,filas
   do j=1,columnas
      do k=1,ancho

      !233 continue
      !print *, "RandNew() = ", RandNew()
      s=int(Q*RandNew())+1

      !if (s==0) goto 233

      c(i,j,k)=s
                                       
      end do
   end do
end do
!!$omp end parallel do
end if

 
   do k=1,Q
    orient1(k)=0
   end do
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+
!+     Generacion del listado de sitios
!+
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

do k=1,Q
   vect(k) = k
end do

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&
!!!!!!!!!!!!!!!!!defino los bloques de paralelización!!!!!!!!!!!!!!!!!!!!!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&

call omp_set_num_threads(12)               ! indicamos que deseamos usar hasta 4 hilos
Nhilos     = omp_get_max_threads()

tam_bloque = Q/(Nhilos*2)
allocate(bloques(2*Nhilos, tam_bloque))


!$omp parallel private(thread_id, inicio, fin, k)
   thread_id = omp_get_thread_num()

   ! Primer bloque para este hilo
   inicio = thread_id * tam_bloque + 1
   fin = (thread_id + 1) * tam_bloque
    do k = inicio, fin
            bloques(thread_id + 1, k - inicio + 1) = vect(k)
    end do

  
   ! Segundo bloque para este hilo (desfasado Nhilos posiciones)
   inicio = (thread_id + Nhilos) * tam_bloque + 1
   fin    = (thread_id + Nhilos + 1) * tam_bloque
   do k = inicio, fin
      bloques(thread_id + 1 + Nhilos, k - inicio + 1) = vect(k)
   end do
!$omp end parallel
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&


!*************************************************************
!
!        Funcion de energia superficial
!
!*************************************************************

archivo1 = trim(adjustl(directorio)) //'energy'// '.txt' 
open(unit=45,file=archivo1, status='replace')


do i=1,91
   angulote=1.0*i
   difa=Lif(angulote-1.0,energy,ang_l)
   write(45,*) i-1,difa
end do

close(45)




if ((precip=='pi') .and. (radio_fijo=='s'))  call Prep_r(c,filas,columnas,ancho,fraccion_v, distri_b,radius)
if ((precip=='pi') .and. (radio_fijo=='n'))  call Prep(c,filas,columnas,ancho,fraccion_v0,fraccion_v1,fraccion_v2)



Write( conce,'(I10)' ) 10
  

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
!       Contar cuantos cristales hay
!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


do k=1,ancho
   do i=1,filas
      do j=1,columnas
         s=c(i,j,k)
          if (s /=0  ) then
         orient1(s)=orient1(s)+1  
          else
          end if                                                                                       
      end do
   end do
end do


numero_g = 0

do s=1,Q
   if (orient1(s)>=1) numero_g = numero_g+1
end do


call graph(c,0,directorio,conce,filas,columnas,ancho,delta,distri_b)
!call mask(c,0,directorio,conce,filas,columnas,ancho,delta,distri_b)
!call tresD(c,0,directorio,conce,filas,columnas,ancho)
!call distri(orient,0,directorio,conce,filas,columnas,ancho)
!call misori(c,time,directorio,conce,filas,columnas,ancho)
!call archivo_xyz(c, filas, columnas, ancho, time, directorio)
!call archivo_lammpstrj(c, filas, columnas, ancho, time, directorio)
    
archivo = trim(adjustl(directorio)) //'area'// '_' // 'medio'//' '// trim(adjustl(conce)) // ' '// '.txt' 
open(unit=13, file=archivo)
flush(13)


 print*,'__________________________________________________________'

!write(*,*),'cristal inicial         ',respuesta
!write(*,*)
!write(*,*)
!write(*,*)
!write(*,*),'energía superficial     ', energy 
!write(*,*),'1=logaritmo'
!write(*,*),'2=senusoidal ice'
!write(*,*),'3=uniforme' 
!write(*,*),'4=lineal'
!write(*,*),'5=lineal ice)'
!write(*,*)
!write(*,*),'tiempo inicial         ' ,time0
!write(*,*)
!write(*,*),'numero de precipitados ' ,Q_p        
!write(*,*),'__________________________________________________________'

do time=time0,stoptime

print*,time


!$omp parallel private(thread_id,i,j,temp)
   temp=0
   thread_id = omp_get_thread_num()
   
    do i = tam_bloque, 2, -1
       j = int(RandNew() * i) + 1  ! Número aleatorio entre 1 e i
       ! Intercambiamos bloques(thread_id + 1,i) con bloques(thread_id + 1,j)
       temp = bloques(thread_id + 1,i)
       bloques(thread_id + 1,i) = bloques(thread_id + 1,j)
       bloques(thread_id + 1,j) = temp
    end do

     ! Segundo bloque del hilo
   do i = tam_bloque, 2, -1
      j = int(RandNew() * i) + 1  ! Número aleatorio entre 1 e i
      temp = bloques(thread_id + 1 + Nhilos, i)
      bloques(thread_id + 1 + Nhilos, i) = bloques(thread_id + 1 + Nhilos, j)
      bloques(thread_id + 1 + Nhilos, j) = temp
   end do
!$omp end parallel

    BG_part0=0
    BG_part1=0
    BG_part2=0
    
    !puntero_s=Q
    w=0
    cont_radio=0


!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!           aqui empieza el calculo de montecarlo
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
!*******Encontrar  ii,jj,kk
    
    !iter es el indice que recorrerá vectR, que es mas amigable para paralelizar 
!$omp parallel DEFAULT(NONE)     &
!$omp   private(iter, ii, jj, kk, swap, ms, num,thread_id, radius)     &
!$omp   reduction(+:BG_part0,BG_part1,BG_part2,cont_radio) &
!$omp   shared(tam_bloque,bloques,Q,energy,c)

   thread_id = omp_get_thread_num()
   
    do iter = 1, tam_bloque

      ! === PRIMERO: punto iter del bloque PAR asignado a este hilo ===
      swap = bloques(2 * thread_id + 2, iter)
      ms = 0
      if (mod(swap, filas * columnas) /= 0) ms = 1
      kk = int(swap / (filas * columnas)) + ms
      num = swap - (kk - 1) * (filas * columnas)
      jj = mod(num, columnas)
      if (jj == 0) jj = columnas
      ii = (num - jj) / columnas + 1
      if (swap == Q) then
         ii = filas
         kk = ancho
         jj = columnas
      end if
      call Montecarlo_parallel(ii, jj, kk, c, filas, columnas, ancho, radio_fijo, energy, BG_part0, BG_part1, BG_part2)

      !$omp barrier  ! <--- asegurás que todos terminaron el paso PAR

      ! === DESPUÉS: punto iter del bloque IMPAR asignado a este hilo ===
      swap = bloques(2 * thread_id + 1, iter)
      ms = 0
      if (mod(swap, filas * columnas) /= 0) ms = 1
      kk = int(swap / (filas * columnas)) + ms
      num = swap - (kk - 1) * (filas * columnas)
      jj = mod(num, columnas)
      if (jj == 0) jj = columnas
      ii = (num - jj) / columnas + 1
      if (swap == Q) then
         ii = filas
         kk = ancho
         jj = columnas
      end if
      call Montecarlo_parallel(ii, jj, kk, c, filas, columnas, ancho, radio_fijo, energy, BG_part0, BG_part1, BG_part2)

   end do
!$omp end parallel

!%%%%%%%%%%%%%%%verificacion cant de materia%%%%%%%%%%%%%%%%%
materia=0
do i=1,filas
   do j=1,columnas
      do k=1,ancho
      if (c(i,j,k)==0) materia=materia+1
      end do
   end do
 end do
print*, "la cantidad de materia es:", materia
!print*, "la cantidad de particulas radio 0 es:",cont_radio
!%%%%%%%%%%%%%%%verificacion cant de materia%%%%%%%%%%%%%%%%%



!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!           aqui termina el calculo de montecarlo
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                
!
!           Generacion de los archivos     
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!write(*,*) time
if ((mod(time,paso)==0) .and. (time/=0)) then
     call graph(c,time,directorio,conce,filas,columnas,ancho,delta,distri_b)
     !call mask(c,time,directorio,conce,filas,columnas,ancho,delta,distri_b)
     !call tresD(c,time,directorio,conce,filas,columnas,ancho) 
     !call distri(orient,time,directorio,conce,filas,columnas,ancho)
     !call misori(c,time,directorio,conce,filas,columnas,ancho)
     call archivo_xyz(c, filas, columnas, ancho, time, directorio)
     !call archivo_lammpstrj(c, filas, columnas, ancho, time, directorio)
     end if


if (mod(time,paso_area)==0)  then



!******************************************************************* 
!                 Calculo de radio, area media   
!*******************************************************************

 

!************************************************************
!
!            Contar cuantos cristales hay
!
!************************************************************

do s=1,Q
   orient1(s)=0
end do

do i=1,filas
   do j=1,columnas
      do k=1,ancho
         s = c(i,j,k)
         if (s/=0) then
         orient1(s) = orient1(s) + 1
         else
         end if
      end do
   end do
end do

numero_g = 0
do s=1,Q
   if (orient1(s)>2) numero_g=numero_g+1
end do


radio_g=Q/numero_g



if (ancho>10) then

radio_g=(3.0*radio_g/(4.0*3.14159))

radio_g=radio_g**(1.0/3.0)

area_g=4.0*3.14159*radio_g**(2.0) 

else

radio_g=radio_g/ancho

area_g=radio_g 

radio_g=(radio_g/3.14159)**(1.0/2.0)



end if 
   

   BG_part=BG_part0+BG_part1+BG_part2     !nro total de particulas en BG
   
   if (radio_fijo=='s') then
   mat_total1 = int(columnas*filas*ancho*fraccion_v/100._pr/(2*radius+1)**3)    !nro total de particulas
   else
   mat_total1 = int(columnas*filas*ancho*fraccion_v/100._pr)
   end if
   
   write(13,3) time, radio_g, area_g,BG_part0,BG_part1,BG_part2,BG_part,mat_total1
   flush(13)
   print*, time, radio_g, area_g,BG_part0,BG_part1,BG_part2,BG_part,mat_total1
    
   3    FORMAT(I10,',',F10.3,',',F10.3,',',I10,',',I10,',',I10,',',I10,',',I10) 
end if

   
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     Generacion de policristal parcial (corte de luz)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if ((mod(time,paso_luz)==0))  then
   Write( timew,'(I10)' )  time
   archivo = 'output/' // trim(adjustl(volumen1)) // 'x' // trim(adjustl(volumen2)) // 'x' // &
          trim(adjustl(volumen3)) // '_' // trim(adjustl(respuesta)) // '.txt'
   open(unit=20,file=archivo)

   do k=1,ancho
     do i=1,filas
       do j=1,columnas
          s = j + ( i - 1 )*columnas + ( k - 1 )*( filas*columnas )
          write(20,*) c(i,j,k),orient1(s)
       end do
     end do
   end do
   close(20)
end if
end do
close(19)
close(13)
deallocate(vect, orient1,bloques, c)
end program grano_particulas_movil
