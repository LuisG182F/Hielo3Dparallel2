module Montecarlo


use presicion

use hielonn

use cal_radio

use precipitado1

use radio0

use radio1

use radio2


contains


subroutine Montecarlo_parallel(ii,jj,kk,c,filas,columnas,ancho,radio_fijo,energy,BG_part0,BG_part1,BG_part2) 

implicit none

!pregunta si quieremos un radio de particula fija- radio_fijo = 'n' debemos poner ancho=9, radio_fijo = 's' hay que ver que tipo de particula vamos a generar!!!) 

!ancho 5 para radio 0
!ancho 7 para radio 1
!ancho 9 para radio 2
!efectos probailistico de las particulas_ fraccion de particulas moviles

! Usar N para definir las dimensiones de las matrices
integer :: filas, columnas,ancho
character(len=60)  :: radio_fijo 


real(pr),parameter                      ::   casino=1.0

real(pr),parameter                      ::   pi=3.14159
!factores relacionados con la funcion de energia de activacion y superficial
real(pr),parameter                      ::   beta=1.0,ang_l=20.0
!cuanto mas chico es beta menor es la influencia de la temperatura
integer                                 ::   sip1,sip2,sip3,iii,jjj,kkk,i, BG_part0,BG_part1,BG_part2
integer                                 ::   ii,jj,kk,sitioi,sitiof
integer                                 ::   jfl,jkl,jcl,jfr,jkr,jcr,volu
!factores relacionados con la funcion de energia superficial y con la posibilidad de rotacion de los granos
integer                                 ::   energy
integer                                 ::   Q,e,mama,u
integer                                 ::   mm,nn,ll,veci,bordei,bordef,centro,bordeCT
integer                                 ::   matrix,tarzan,centro0,centro00,centro1,centro2,radius
real(pr)                                ::   deltaGG,xx,ww,mini1,mini2
real(pr)                                ::   Hi,difa,difa1,ruleta
integer, allocatable                    ::   c(:,:,:)

integer,dimension(10,10,10)             ::   c1,c2,c3

integer,dimension(26)                   ::   vecinos
real(pr),dimension(26)                  ::   Hff,deltaG

!inicializacion de variables
veci=0
bordect=0
matrix=0
centro=0
bordei=0
u=0 !no se si esta bien inicializarlo asi
radius=0
Q=filas*columnas*ancho




!*******Encontrar  ii,jj,kk
            

               !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    !           aqui empieza el modulo, tomando como dato ii,jj,kk,c
               !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            
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
                        
                        ruleta=RandNew()
                        
                         if (ruleta>casino) goto 145
                         !print*, 'pase'
                         
                        tarzan=0
                        23 e=int(RandNew()*26)+1
                         
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
                                bordeCT=Vect0(c,iii,jjj,kkk,filas,columnas,ancho);
                                                                             
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
                                   goto 145
                                   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                !%%%%%%%REVISAR como enganchar esto con mi vectR!!!!!!%%%%%%%%%%%%    
                                   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%       
                            !pastor= jjj + ( iii-1 ) * columnas + ( kkk-1 ) * ( filas*columnas )
                                        
                                        !do u1=1,Q
                                        !if (vect(u1)==pastor) w=u1
                                        !end do
                                        !swap=vect(w)                
                          
                            
                            
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
                   do i=1,26
                     difa=abs((Vecinos(i)-sitioi)*90.0/Q)
                        if ((Vecinos(i)/=0) .and. (difa/=0)) then
                        Hi=Hi+Lif(difa,energy,ang_l)
                        else
                        end if
                 end do

                 
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
                                                e=int(RandNew()*26)+1
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
    xx=RandNew()
        if (xx<ww) then
        
        c(ii,jj,kk)=sitiof
        else 
        c(ii,jj,kk)=sitioi
        end if
    end if
  end if


145 continue

end subroutine Montecarlo_parallel

end module

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!           aqui termina el calculo de montecarlo
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%