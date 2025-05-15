module precipitado

use presicion

use radio0

use radio1

use radio2

use hielonn

 contains


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+
!+     Funcion generacion de los precipitados iniciales
!+
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine Prep(c,filas,columnas,ancho,fraccion_v) 

implicit none

integer                                 ::   filas,columnas,ancho
integer                                 ::   radius !radius es el radio de las impurezas 
real(pr)                                ::   mat_total,fraccion_v
integer                                 ::   i,j,k,ir,jr,kr
integer                                 ::   ii,jj,kk,delta,borde1,radio_ran
integer                                 ::   pepo,mm,nn,ll,w,xx,matrix1,veci1
integer,dimension(filas,columnas,ancho) ::   c
character(len=60)                       ::   distri_b,radio_fijo





mat_total = columnas*filas*ancho*fraccion_v/100    !nro total de particulas




        !particulas de diferente tamaño
        

        pepo= int(mat_total)
        
        


                 w=0
                 do while ((pepo-w)>=0)  
                    
                    
                    radius=int(Rand()*3)
                    
                    print*, pepo-w,radius
                               
                    123 xx=Rand()*filas
                    ii=int(xx)
                    
                    
                    xx=Rand()*columnas
                    jj=int(xx)
                                
                    
                  
                    xx=Rand()*ancho
                    kk=int(xx)
                    
                    
          
         Select case (radius)  
                            
                                 case(0)
                                        
                                
                                        matrix1=Vecm(radius,ii,jj,kk,filas,columnas,ancho); 
                                        
                           
                                 case(1)
                                        
                                        matrix1=Vecm1(radius,ii,jj,kk,filas,columnas,ancho);
                                        ! 
                                        
                                    
                                 case(2)
                                        
                                        matrix1=Vecm2(radius,ii,jj,kk,filas,columnas,ancho);
                                        
                                      
        end select



        if     ((matrix1==0)  ) then


        Select case (radius)  
                            
                                 case(0)
                                        
                                
                                         
                                        veci1=Ve(c,ii,jj,kk,filas,columnas,ancho);
                                        !26
                           
                                 case(1)
                                        
                                        
                                        veci1=Ve1(c,ii,jj,kk,filas,columnas,ancho);
                                         !98
                                 case(2)
                                        
                                       
                                        veci1=Ve2(c,ii,jj,kk,filas,columnas,ancho);
                                         !218
        end select


         
         if (veci1==0) then 
         

        do mm=-radius,radius,1
        do nn=-radius,radius,1
        do ll=-radius,radius,1



        c(ii+mm,jj+nn,kk+ll)=0



        end do
        end do
        end do

        w=w+(2*radius+1)**3           
        else  
        goto 123
        end if

        else
        goto 123
        end if

        end do





end subroutine Prep


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+
!+     Funcion generacion de los precipitados iniciales
!+
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


subroutine Prep_r(c,filas,columnas,ancho,fraccion_v, distri_b,radius) 

implicit none

integer                                 ::   filas,columnas,ancho
integer                                 ::   radius !radius es el radio de las impurezas 
real(pr)                                ::   mat_total,fraccion_v
integer                                 ::   i,j,k,ir,jr,kr
integer                                 ::   ii,jj,kk,delta,borde1,radio_ran
integer                                 ::   pepo,mm,nn,ll,w,xx,matrix1,veci1
integer,dimension(filas,columnas,ancho) ::   c
character(len=60)                       ::   distri_b,radio_fijo





mat_total = columnas*filas*ancho*fraccion_v/100    !nro total de particulas

if (distri_b=='uniforme') then 

delta = int((mat_total)**(1.0/3.0))

        do i=1,delta
        do j=1,delta
        do k=1,delta


            ir=real(filas)/real(delta)*real(i)
            ii=int(ir)
            jr=real(columnas)/real(delta)*real(j)
            jj=int(jr)
            kr=real(ancho)/real(delta)*real(k)
            kk=int(kr)


            Select case (radius)  
                                
                                     case(0)
                                            
                                    
                                            matrix1=Vecm(radius,ii,jj,kk,filas,columnas,ancho); 
                               
                                     case(1)
                                            
                                            matrix1=Vecm1(radius,ii,jj,kk,filas,columnas,ancho);
                                            
                                        
                                     case(2)
                                            
                                            matrix1=Vecm2(radius,ii,jj,kk,filas,columnas,ancho);
                                          
            end select


            if     ((matrix1==0)  ) then
                                                  

                do mm=-radius,radius,1
                do nn=-radius,radius,1
                do ll=-radius,radius,1

                c(ii+mm,jj+nn,kk+ll)=0


                end do
                end do
                end do

            else
            end if

        end do
        end do
        end do


else


        pepo=int(mat_total/(2*radius+1)**3)
        
           
                 w=0
        do while ((pepo-w)>=0)  
                    print*, pepo-w
                    
                    
                               
                    122 xx=Rand()*filas
                    ii=int(xx)
                    
                    
                    xx=Rand()*columnas
                    jj=int(xx)
                                
                    
                  
                    xx=Rand()*ancho
                    kk=int(xx)
                    
                    
          
                               Select case (radius)  
                            
                                 case(0)
                                        
                                
                                        matrix1=Vecm(radius,ii,jj,kk,filas,columnas,ancho); 
                                        
                           
                                 case(1)
                                        
                                        matrix1=Vecm1(radius,ii,jj,kk,filas,columnas,ancho);
                                        ! 
                                        
                                    
                                 case(2)
                                        
                                        matrix1=Vecm2(radius,ii,jj,kk,filas,columnas,ancho);
                                        
                                      
                                 end select



                if     ((matrix1==0)  ) then


                            Select case (radius)  
                                                
                                                     case(0)
                                                            
                                                    
                                                             
                                                            veci1=Ve(c,ii,jj,kk,filas,columnas,ancho);
                                                            !26
                                               
                                                     case(1)
                                                            
                                                            
                                                            veci1=Ve1(c,ii,jj,kk,filas,columnas,ancho);
                                                             !98
                                                     case(2)
                                                            
                                                           
                                                            veci1=Ve2(c,ii,jj,kk,filas,columnas,ancho);
                                                             !218
                            end select

                         if (veci1==0) then 
                         

                                do mm=-radius,radius,1
                                do nn=-radius,radius,1
                                do ll=-radius,radius,1

                                c(ii+mm,jj+nn,kk+ll)=0

                                end do
                                end do
                                end do

                        w=w+1            
                        else  
                        goto 122
                        end if

                else
                goto 122
                end if

        end do



end if

end subroutine Prep_r


!*********************************************************************
!                Function sitio (FORTRAN)
!
!  Esta rutina calcula si el sitio esta dentro de un grano 
!  Arroja una variable par1=1 si esta  y par1=0 si no esta .
!
!*********************************************************************

Integer function par(c,ii,jj,kk,filas,columnas,ancho) 
IMPLICIT NONE  
integer                                        :: filas,columnas,ancho,peruca
integer                                        :: ii,jj,kk,jfl,jfr,jcl,jcr,jkl,jkr,mfl,mfr,mcl,mcr,mkl,mkr,s 
integer,dimension(filas,columnas,ancho)        :: c 
integer, dimension(98)                         :: vvecinos 

par=0
                 jfl = ii-1 ;  if (jfl < 1)        jfl = filas
                 jcl = jj-1 ;  if (jcl < 1)        jcl = columnas
                 jkl = kk-1 ;  if (jkl < 1)        jkl = ancho
                 jfr = ii+1 ;  if (jfr > filas)    jfr = 1  
                 jcr = jj+1 ;  if (jcr > columnas) jcr = 1
                 jkr = kk+1 ;  if (jkr > ancho)    jkr = 1
                 
                 Vvecinos(1) = c(jfl, jcl,jkl)
                 Vvecinos(2) = c(jfl, jj,jkl) 
                 Vvecinos(3) = c(jfl, jcr,jkl)
                 Vvecinos(4) = c(ii, jcl,jkl)
                 Vvecinos(5) = c(ii, jcr,jkl)
                 Vvecinos(6) = c(jfr, jcl,jkl)
                 Vvecinos(7) = c(jfr, jj,jkl)
                 Vvecinos(8) = c(jfr, jcr,jkl)
                 
                 Vvecinos(9) = c(jfl, jcl,kk)
                 Vvecinos(10) = c(jfl, jj,kk)
                 Vvecinos(11) = c(jfl, jcr,kk)
                 Vvecinos(12) = c(ii, jcl,kk)
                 Vvecinos(13) = c(ii, jcr,kk)
                 Vvecinos(14) = c(jfr, jcl,kk)
                 Vvecinos(15) = c(jfr, jj,kk)
                 Vvecinos(16) = c(jfr, jcr,kk)
                 
                 Vvecinos(17) = c(jfl, jcl,jkr)
                 Vvecinos(18) = c(jfl, jj,jkr)
                 Vvecinos(19) = c(jfl, jcr,jkr)
                 Vvecinos(20) = c(ii, jcl,jkr)
                 Vvecinos(21) = c(ii, jcr,jkr)
                 Vvecinos(22) = c(jfr, jcl,jkr)
                 Vvecinos(23) = c(jfr, jj,jkr)
                 Vvecinos(24) = c(jfr, jcr,jkr)
                 
                 Vvecinos(25) = c(ii, jj,jkl)
                 Vvecinos(26) = c(ii, jj,jkr)
       



do s=1,26
if (vvecinos(s)/=0) peruca=vvecinos(s)
end do 

do s=1,26
if (vvecinos(s)==c(ii,jj,kk)) par=par+1
end do 


end function

end module precipitado


