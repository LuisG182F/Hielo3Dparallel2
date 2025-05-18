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



implicit none

! Definir N como una constante
integer, parameter :: N = 50

! Usar N para definir las dimensiones de las matrices
integer, parameter :: filas = N, columnas = N, ancho = N, stoptime = 100
character(len=60), parameter :: radio_fijo = 'n'



!pregunta si quieremos un radio de particula fija- radio_fijo = 'n' debemos poner ancho=9, radio_fijo = 's' hay que ver que tipo de particula vamos a generar!!!) 

!ancho 5 para radio 0
!ancho 7 para radio 1
!ancho 9 para radio 2
integer,parameter                       ::   paso=10,paso_area=10,paso_luz=500000
!efectos probailistico de las particulas_ fraccion de particulas moviles
real(pr),parameter                      ::   casino=1.0

real(pr),parameter                      ::   pi=3.14159
!factores relacionados con la funcion de energia de activacion y superficial
real(pr),parameter                      ::   beta=1.0,ang_l=20.0
!cuanto mas chico es beta menor es la influencia de la temperatura
integer                                 ::   delta,sip1,sip2,sip3,iii,jjj,kkk,i,j,k,s, BG_part0,BG_part1,BG_part2,BG_part,iter, temp
integer                                 ::   time,puntero_s,swap,ii,jj,kk,w,num,sitioi,sitiof,time0
integer                                 ::   jfl,jkl,jcl,jfr,jkr,jcr,volu,pastor,u1
!factores relacionados con la funcion de energia superficial y con la posibilidad de rotacion de los granos
integer                                 ::   energy
integer                                 ::   Q,numero_g,e,mama,u
integer                                 ::   mm,nn,ll,ms,veci,bordei,bordef,centro,bordeCT
integer                                 ::   matrix,tarzan,centro0,centro00,centro1,centro2,radius,mat_total1
real(pr)                                ::   radio_g,deltaGG,xx,ww,angulote,mini1,mini2, fraccion_v
real(pr)                                ::   Hi,difa,difa1,ruleta,area_g,fraccion_v0,fraccion_v1,fraccion_v2
integer,dimension(filas,columnas,ancho) ::   c

integer,dimension(10,10,10)             ::   c1,c2,c3
integer,dimension(filas*columnas*ancho) ::   vect,orient1, vectR

integer,dimension(26)                   ::   vecinos
real(pr),dimension(26)                  ::   Hff,deltaG
character(len=200)                      ::   archivo,archivo1,precip
character(len=60)                       ::   timew,volumen,volumen1,volumen2,volumen3,respuesta,conce,distri_b
character(len=10000)                    ::   directorio,directorio1


                                                               

call rand0

!parametros para arreglar warnings
veci=0
bordect=0
matrix=0
centro=0
bordei=0
u=1


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

respuesta = 's'

distri_b = 'uniform'

time0 = 0
energy = 3
precip ='pi'
          
fraccion_v0 = 0.003_pr                                        !fraccion de materia 0
fraccion_v1 = 0.01_pr                                        !fraccion de materia 1 
fraccion_v2 = 0.014_pr                                        !fraccion de materia 2
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
     archivo = trim(adjustl(directorio1)) //trim(adjustl(volumen))// '_' // trim(adjustl(timew))// ' ' //'.txt'
     
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

do k=1,Q
orient1(k)=0
end do


do k=1,ancho

do i=1,filas

do j=1,columnas

233 continue
s=int(Q*Rand())+1

if (s==0) goto 233

c(i,j,k)=s
                                       
end do

end do

end do

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




if ((precip=='pi') .and. (radio_fijo=='s'))  call Prep_r(c,filas,columnas,ancho,fraccion_v1, distri_b,radius)
if ((precip=='pi') .and. (radio_fijo=='n'))  call Prep(c,filas,columnas,ancho,fraccion_v0,fraccion_v2,fraccion_v2)



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
call mask(c,0,directorio,conce,filas,columnas,ancho,delta,distri_b)

call tresD(c,0,directorio,conce,filas,columnas,ancho)
!call distri(orient,0,directorio,conce,filas,columnas,ancho)
!call misori(c,time,directorio,conce,filas,columnas,ancho)


    
archivo = trim(adjustl(directorio)) //'area'// '_' // 'medio'//' '// trim(adjustl(conce)) // ' '// '.txt' 

open(unit=13,file=archivo)


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

!vectR es una array que tiene todos los elementos de vect pero ordenados aleatoriamente
do i = 1, Q
    vectR(i) = vect(i)  ! Copiamos vect en vectR
end do

do time=time0,stoptime

print*,time


!························································································
!
!   Generacion de particulas o burbujas
!
!........................................................................................


temp=0

! Ahora desordenamos vectR usando Fisher–Yates
do i = Q, 2, -1
    j = int(Rand() * i) + 1  ! Número aleatorio entre 1 e i
    ! Intercambiamos vectR(i) con vectR(j)
    temp = vectR(i)
    vectR(i) = vectR(j)
    vectR(j) = temp
end do
    
    BG_part0=0
    BG_part1=0
    BG_part2=0
    
    !puntero_s=Q
    w=0
    
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!           aqui empieza el calculo de montecarlo
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    
!*******Encontrar  ii,jj,kk

    !iter es el indice que recorrerá vectR, que es mas amigable para paralelizar 
    !!$omp parallel do private(swap, ms, kk, num, jj, ii, sitioi, jfl, jcl, jkl, jfr, jcr, jkr, vecinos)
    do iter=1,Q
    
      
       !w=int(Rand()*(puntero_s))+1
       swap=vectR(iter)
       ms=0    
       
       
       if(mod(swap,(filas*columnas))/=0) ms=1
            kk=int(swap/(filas*columnas))+ms
            num=swap-(kk-1)*(filas*columnas)
            jj=mod(num,columnas)
            if (jj==0) jj=columnas  
                 ii=(num-jj)/columnas+1
            if (swap==Q) then
                     ii=filas
                     kk=ancho
                     jj=columnas
            end if
            
            
            sitioi=c(ii,jj,kk)
            
            
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+
!+                 Calculo de vecinos!!!!
!+
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            
    jfl = ii-1 ;  if (jfl < 1)        jfl = filas
                 jcl = jj-1 ;  if (jcl < 1)        jcl = columnas
                 jkl = kk-1 ;  if (jkl < 1)        jkl = ancho
                 jfr = ii+1 ;  if (jfr > filas)    jfr = 1  
                 jcr = jj+1 ;  if (jcr > columnas) jcr = 1
                 jkr = kk+1 ;  if (jkr > ancho)    jkr = 1
                 
                 vecinos(1) = c(jfl, jcl,jkl)
                 vecinos(2) = c(jfl, jj,jkl) 
                 vecinos(3) = c(jfl, jcr,jkl)
                 vecinos(4) = c(ii, jcl,jkl)
                 vecinos(5) = c(ii, jcr,jkl)
                 vecinos(6) = c(jfr, jcl,jkl)
                 vecinos(7) = c(jfr, jj,jkl)
                 vecinos(8) = c(jfr, jcr,jkl)
                 
                 vecinos(9) = c(jfl, jcl,kk)
                 vecinos(10) = c(jfl, jj,kk)
                 vecinos(11) = c(jfl, jcr,kk)
                 vecinos(12) = c(ii, jcl,kk)
                 vecinos(13) = c(ii, jcr,kk)
                 vecinos(14) = c(jfr, jcl,kk)
                 vecinos(15) = c(jfr, jj,kk)
                 vecinos(16) = c(jfr, jcr,kk)
                 
                 vecinos(17) = c(jfl, jcl,jkr)
                 vecinos(18) = c(jfl, jj,jkr)
                 vecinos(19) = c(jfl, jcr,jkr)
                 vecinos(20) = c(ii, jcl,jkr)
                 vecinos(21) = c(ii, jcr,jkr)
                 vecinos(22) = c(jfr, jcl,jkr)
                 vecinos(23) = c(jfr, jj,jkr)
                 vecinos(24) = c(jfr, jcr,jkr)
                 
                 vecinos(25) = c(ii, jj,jkl)
                 vecinos(26) = c(ii, jj,jkr)        
            
        
          
                 
!-------------------------------------------------------------
!       El sitio de red es una particula con c(i,j,k)==0
!-------------------------------------------------------------


                 if (sitioi==0) then
                 

                                    
!******************************************************************************************
!                Centro Vet
!
!  Esta rutina calcula si la particula esta en el centro de la particula.
!  Arroja una variable vet=1 si lo esta y vet=0 si no esta en el centro de la particula.
!
!******************************************************************************************
                   
                   
!******************************************************************************************
!                Borde vec
!
!  Esta rutina calcula si la particula esta en el borde de grano (BG).
!  Arroja una variable vec=1 si esta en el BG y vec=0 si no esta en el BG.
!
!*****************************************************************************************

!******************************************************************************************
!                BordeCT vect
!
!  Esta rutina calcula si la particula esta en el borde de grano (BG).
!  Arroja una variable vec=1 si esta en el BG y vec=0 si no esta en el BG.
!
!*****************************************************************************************

!*****************************************************************************************
!                Veci ve
!
!  Esta rutina calcula si la particula esta al lado de otra particula. 
!  Arroja una variable ve=1 si lo esta y ve=0 si no lo esta.
!
!*****************************************************************************************

!*****************************************************************************************
!                MatrixM vecm
!
!  Esta rutina calcula si la particula esta dentro de la region de calculo. 
!  Arroja una variable vecc=0 si lo esta y vecc=1 si no lo esta.
!
!*****************************************************************************************
    
   if (radio_fijo=='n') then

   radius=8
   centro1=radiopp1(c,ii,jj,kk,filas,columnas,ancho)
   centro2=radiopp2(c,ii,jj,kk,filas,columnas,ancho)
   centro0=radiopp0(c,ii,jj,kk,filas,columnas,ancho)
   centro00=radiopp00(c,ii,jj,kk,filas,columnas,ancho)
   
   if ((centro1==0)) radius=2
   if ((centro0==0) .and. (centro2==1)) radius=1
   if ((centro00==1))  radius=0
   if (radius==8) goto 145
   else
   end if
   
 
  
   
   
                    Select case (radius)  
                    
                         case(0)
                         
                               bordei=Vec(c,ii,jj,kk,filas,columnas,ancho); 
                               matrix=Vecm(radius,ii,jj,kk,filas,columnas,ancho);
                               !centro=Vet(c,ii,jj,kk,filas,columnas,ancho)
                               centro=1
                                                                                        
                                                                           
                         case(1)
                               
                                
                                bordei=Vec1(c,ii,jj,kk,filas,columnas,ancho); 
                                matrix=Vecm1(radius,ii,jj,kk,filas,columnas,ancho);
                                centro=Vet1(c,ii,jj,kk,filas,columnas,ancho)
                                                     
                            
                         case(2)
                               
                                                             
                               bordei=Vec2(c,ii,jj,kk,filas,columnas,ancho); 
                                matrix=Vecm2(radius,ii,jj,kk,filas,columnas,ancho);
                                centro=Vet2(c,ii,jj,kk,filas,columnas,ancho)
                                
                                
                             
                    end select

                         if ((bordei==0) .or. (matrix==1) .or. (centro==0))  goto 145
                        
                        !particula no centrada o fuera de la region de calculo que no haga NADA            
                        
                        
                        Select case (radius)  
                    
                         case(0)
                                
                          BG_part0=BG_part0+1
                                                                             
                        case(1)
                          BG_part1=BG_part1+1
        
                        case(2)
                                
                          BG_part2=BG_part2+1     
                                
                              
                    end select
                        
                        ruleta=rand()
                        
                         if (ruleta>casino) goto 145
                         !print*, 'pase'
                         
                        tarzan=0
                        23 e=int(Rand()*26)+1
                         
                        tarzan=tarzan+1
                        
                        
                        sitiof=vecinos(e)
                                        
                         
                         Select case (e)
                         case(1)
                                iii=jfl; jjj=jcl;  kkk=jkl;  
                         case(2)
                                iii=jfl; jjj=jj;  kkk=jkl;    
                         case(3)
                                iii=jfl; jjj=jcr;  kkk=jkl;    
                         case(4)
                                iii=ii; jjj=jcl;  kkk=jkl;    
                         case(5)
                                iii=ii; jjj=jcr;  kkk=jkl;    
                         case(6)
                                iii=jfr; jjj=jcl;  kkk=jkl;    
                         case(7)
                                iii=jfr; jjj=jj;   kkk=jkl;    
                         case(8)
                                iii=jfr; jjj=jcr;  kkk=jkl;    
                         case(9)
                                iii=jfl; jjj=jcl; kkk=kk;     
                         case(10)
                                iii=jfl; jjj=jj;  kkk=kk;    
                         case(11)
                                iii=jfl; jjj=jcr; kkk=kk;     
                         case(12)
                                iii=ii;  jjj=jcl; kkk=kk;     
                         case(13)
                                iii=ii;  jjj=jcr; kkk=kk;     
                         case(14)
                                iii=jfr; jjj=jcl; kkk=kk;     
                         case(15)
                                iii=jfr; jjj=jj;  kkk=kk;     
                         case(16)
                                iii=jfr; jjj=jcr; kkk=kk;     
                         case(17)
                                iii=jfl; jjj=jcl; kkk=jkr;    
                         case(18)
                                iii=jfl; jjj=jj;  kkk=jkr;    
                         case(19)
                                iii=jfl; jjj=jcr; kkk=jkr;    
                         case(20)
                                iii=ii;  jjj=jcl; kkk=jkr;    
                         case(21)
                                iii=ii;  jjj=jcr; kkk=jkr;    
                         case(22)
                                iii=jfr; jjj=jcl; kkk=jkr;    
                         case(23)
                                iii=jfr; jjj=jj;  kkk=jkr;    
                         case(24)
                                iii=jfr; jjj=jcr; kkk=jkr;    
                         case(25)
                                iii=ii;  jjj=jj;  kkk=jkl;    
                         case(26)
                                iii=ii;  jjj=jj;  kkk=jkr;  
                         end select
                         

                                         
                     sip1=iii-ii
                     sip2=jjj-jj
                     sip3=kkk-kk
                     !tarzan=0
                     
             
              
!---------------------------------------------------------------------------------
!
!   Voy a mover la particula a la nueva posición
!
!---------------------------------------------------------------------------------


                                         do mm=-radius,radius,1
                                            do nn=-radius,radius,1
                                               do ll=-radius,radius,1
                                               
                                                 c2(3+mm,3+nn,3+ll)=c(iii+mm,jjj+nn,kkk+ll)
                                                 !Print*, iii+mm,jjj+nn,kkk+ll
                                               end do
                                            end do
                                         end do

                                        do mm=-radius,radius,1
                                            do nn=-radius,radius,1
                                               do ll=-radius,radius,1
                                                        c1(3+mm,3+nn,3+ll)=c(ii+mm,jj+nn,kk+ll)
                                                        c3(3+mm,3+nn,3+ll)=c(ii-sip1+mm,jj-sip2+nn,kk-sip3+ll)
                                                        !Print*, ii-sip1+mm,jj-sip2+nn,kk-sip3+ll
                                                end do
                                            end do
                                        end do
                                   
                                        do mm=-radius,radius,1
                                            do nn=-radius,radius,1
                                                do ll=-radius,radius,1
                                                    c(ii+mm,jj+nn,kk+ll)=c3(3+mm,3+nn,3+ll)
                                                    !Print*, ii+mm,jj+nn,kk+ll
                                                end do
                                            end do
                                        end do
                                        
                                        do mm=-radius,radius,1
                                            do nn=-radius,radius,1
                                                do ll=-radius,radius,1
                                                    c(iii+mm,jjj+nn,kkk+ll)=c1(3+mm,3+nn,3+ll)
                                                end do
                                            end do
                                        end do

                    Select case (radius)  
                    
                         case(0)
                                veci=Ve(c,iii,jjj,kkk,filas,columnas,ancho);
                                !veci=0
                                matrix=Vecm(radius,iii,jjj,kkk,filas,columnas,ancho);
                                bordeCT=Vect0(c,iii,jjj,kkk,filas,columnas,ancho,radius);
                                                                             
                         case(1)
                                
                                
                                veci=Ve1(c,iii,jjj,kkk,filas,columnas,ancho);
                                bordef=Vec1(c,iii,jjj,kkk,filas,columnas,ancho); 
                                matrix=Vecm1(radius,iii,jjj,kkk,filas,columnas,ancho);
                                bordeCT=Vect1(c,iii,jjj,kkk,filas,columnas,ancho);
                            
                         case(2)
                                
                                
                                veci=Ve2(c,iii,jjj,kkk,filas,columnas,ancho);
                                bordef=Vec2(c,iii,jjj,kkk,filas,columnas,ancho); 
                                matrix=Vecm2(radius,iii,jjj,kkk,filas,columnas,ancho);
                                bordeCT=Vect2(c,iii,jjj,kkk,filas,columnas,ancho);
                              
                    end select
                    
                                  !if ((veci==1) .or.  (matrix==1) .or. (bordef==0)) then
                                  !tiene particulas al lado, esta fuera de la zona de callculo y la particula se salio del BG
                                  if ((veci==1) .or.  (matrix==1) .or. (bordeCT==0)) then
                                  !tiene particulas al lado, esta fuera de la zona de callculo y la particula se descentro del BG
                                  !if (((veci==1) .or.  (matrix==1)) .or. ((bordei==1) .and. (bordeCT==0))) then
                                  !tiene particulas al lado, esta fuera de la zona de callculo o si estaba en BG y se salio 
                                  !if ((veci==1) .or.  (matrix==1)) then
                                  !tiene particulas al lado, esta fuera de la zona de callculo 
                                  
                                  
                                        do mm=-radius,radius,1
                                            do nn=-radius,radius,1
                                                do ll=-radius,radius,1
                                                    c(iii+mm,jjj+nn,kkk+ll)=c2(3+mm,3+nn,3+ll)
                                                end do
                                            end do
                                        end do
                                        
                                        do mm=-radius,radius,1
                                            do nn=-radius,radius,1
                                                do ll=-radius,radius,1
                                                    c(ii+mm,jj+nn,kk+ll)=c1(3+mm,3+nn,3+ll)
                                                end do
                                            end do
                                        end do
                                        
                                        if (tarzan<=200) then
                                        !tarzan=tarzan+1
                                        !print*,tarzan
                                            goto 23
                                            
                                        else
                                        
                                        goto 145
                                        end if
                            else
                                   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                !%%%%%%%REVISAR como enganchar esto con mi vectR!!!!!!%%%%%%%%%%%%    
                                   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%       
                            pastor= jjj + ( iii-1 ) * columnas + ( kkk-1 ) * ( filas*columnas )
                                        
                                        do u1=1,Q
                                        if (vect(u1)==pastor) w=u1
                                        end do
                                        swap=vect(w)                
                          
                            
                            
                                     end if
            
                
                 else
                 

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!   El sitio de red es un atomo de cristal
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     
   
  

   
   
  !do mm=1,26 
  !       if (vecinos(mm)==0) goto 145   
  !end do    

volu=par(c,ii,jj,kk,filas,columnas,ancho);
if (volu==26) goto 145   

  
            
            
                 Hi=0
                 !!$OMP PARALLEL DO PRIVATE(difa) REDUCTION(+:Hi)
                   do i=1,26
                     difa=abs((Vecinos(i)-sitioi)*90.0/Q)
                        if ((Vecinos(i)/=0) .and. (difa/=0)) then
                        Hi=Hi+Lif(difa,energy,ang_l)
                        else
                        end if
                 end do
                 !!$OMP END PARALLEL DO

                 
!*****************************Cristal puro anisotropico ***********************************************

   if (energy/=3) then
    
                
                mini1=200
                
                do e=1,26
                 
                 sitiof=vecinos(e)
                 mama=vecinos(e)
                 vecinos(e)=sitioi
                   
                    Hff(e)=0
                  
                  do i=1,26
                    difa=abs((vecinos(i)-sitiof)*90.0/Q)
                    Hff(e)=Hff(e)+Lif(difa,energy,ang_l)
                  end do
                   
                   deltaG(e)=Hff(e)-Hi
                   
                   if (deltaG(e)/=0) then
                   
                   mini2=deltaG(e)
                   if (mini2<mini1)   then 
                   mini1=mini2
                   u=e    
                   else
                   end if
                   
                   else
                   end if
                   
                   
                   vecinos(e)=mama
                 end do
                                         
                sitiof=vecinos(u)
                deltaGG=mini1
                                                
!&&&&&&&&&&&& Cristal puro uniforme &&&&&&&&&&&&&&&

    else
                                                e=int(Rand()*26)+1
                                                sitiof=vecinos(e)
                                                    if ((sitioi==sitiof) .or. (sitiof==0)) then 
                                                        goto 145
                                                    else
                                                    end if                                    
                                                Hff(e)=0
                                                    do i=1,26
                                                        difa=abs((vecinos(i)-sitiof)*90.0/Q)
                                                            if ((sitiof/=0) .and. (difa/=0) ) then 
                                                                Hff(e)=Hff(e)+Lif(difa,energy,ang_l)
                                                            else
                                                            end if
                                                        deltaGG=Hff(e)-Hi
                                                    end do
    end if

!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!   Calculo de la variacion del hamiltoneano
!_____________________________________________


difa=abs((sitioi-sitiof)*90.0/Q) 
difa1=Lif(difa,energy,ang_l)

    if (deltaGG<=0) then 
    
    c(ii,jj,kk)=sitiof
    else
    ww=0
    if (difa1/=0) ww=(exp(-deltaGG/beta/difa1))*difa1
    xx=Rand()
        if (xx<ww) then
        
        c(ii,jj,kk)=sitiof
        else 
        c(ii,jj,kk)=sitioi
        end if
    end if
  end if


145 continue
end do
!!$omp end parallel do

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
     call mask(c,time,directorio,conce,filas,columnas,ancho,delta,distri_b)
       call tresD(c,time,directorio,conce,filas,columnas,ancho) 
     !call distri(orient,time,directorio,conce,filas,columnas,ancho)
     !call misori(c,time,directorio,conce,filas,columnas,ancho)
     end if


if (mod(time,paso_area)==0)  then


!******************************************************************* 
!                 Calculo de la coordenada, area media   
!*******************************************************************

 

!************************************************************
!
!            Contar cuantos cristales hay
!
!************************************************************

do s=1,Q
   orient1(s)=0
end do

do k=1,ancho
   do i=1,filas
      do j=1,columnas
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

end program grano_particulas_movil
