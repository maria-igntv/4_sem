    Program test
    use dflib
    implicit none

    !	**** variable description ****
    integer(4) I, J, K, im_fi, im_v, kx, ky, k_x, k_v, N, l, i_status

    real*8 Fi, v, Fi0, v0, dt, a, gam, om2, Fi_, v_, a_, x, y, aw
    real*8 F0, om3, t, delta_om, omegam, prec, ampl, fm, df

    real*8 funct1, funct2, funct3       ! functions


    t = 0.; dt = 0.01; gam = 0.01; om2 = 1.; Fi0 = 0.2; v0 = 0.

    Fi = Fi0
    v = v0   
    
    prec = 0.5
    x = 0.

    kx = 180   ! x scale
    ky = 10  ! y scale

    k_x = 15   !phase x scale
    k_v = 15   !phase v scale


    f0 = 1  !force

    om3 = 0.8 !force freq
    Omegam = 1.9
    delta_om = OmegaM/2000


    Fi_ = Fi0
    v_ = v0


    OPEN (unit = 1, file = './Resonance.txt', status = 'unknown')

    DO n = 1, 2000

        om3 = 0.
        om3 = om3 + n*delta_om

        t = 0.
        Fi = Fi0
        v = v0
        ampl = 0.


        DO I = 1, 50000 ! Pedictor - Corrector

            a = -(2.)*Gam*v - om2*Fi + funct3(t, om3, f0)

            fi_ = fi + v*dt
            v_ = v + a*dt
            a_ = -(2.)*gam*v_-om2*fi_ + funct3(t, om3, f0)

            fi = fi + v*dt + a*dt**2/2.
            aw = -(2.)*gam*v_-om2*fi + funct3(t, om3, f0)

            v = v + (a + aw)*dt/2.
            t = t + dt

            !phase space coords
            im_fi = fi*k_x
            im_v = v*k_v

            !amplitude plot coords

            IF (-prec < v .AND. v < prec .AND. Fi .GT. ampl.AND. t > 3./gam) THEN
                ampl = Fi
                y = ABS(Fi * ky)

            END IF
           
           If (n == 2000/2.) then 
              If (t < (4/gam)) then
                  i_status = SETCOLOR(3)
                  i_status = SETPIXEL(im_fi + 900, im_v + 350)    
              else 
                  i_status = SETCOLOR(5)
                  i_status = SETPIXEL(im_fi + 900, im_v + 350)    
              endif
           Endif

        END DO
        
        
        
        write(1,250) delta_om*n, ampl
250     FORMAT(5x,10F10.2)
        
!         write(2,450) ampl 
! 450     FORMAT(5x,10F10.2)

        !resonance plot coord
        x = x + delta_om*kx

        !Resonance plot
        J = SETCOLOR(INT2(6))
        K = SETPIXEL(x + 100, -y+600 ) 

    END DO
    
    CLOSE(1)
    CLOSE(2)

      Write(*, *) "|       Fi      |", "|      V       |"    
      write(*,100)  (fi , v)
        100 Format (2x, 2F13.5) 
      write(*,*)
      write(*,*) 'Press ESC to exit the execution window'
      read(*,*)
    end program test


    ! Functions
    
    real*8 function Funct1(t, om, A)
    implicit none
    real*8 t, om, A, phi
    Funct1 = A*cos(om*t)
    !Write(*, *) "Function A*cos(om*t)"    

    return
    end

    real*8 function funct2(t, om, A)      ! cos, if f(x) > 0 and 0 else
    implicit none

    real*8 t, om, A

    If (cos(om*t) < 0.0) then
        funct2 = 0.0
    else
        funct2 = A*cos(om*t)
    endif
    !Write(*, *) "Function cos(om*t), if cos > 0 and 0 else"  
    return
    end


    real*8 function funct3(t, om, A)      ! fourier series
    implicit none

    real*8 t, om, A, pi
    
    pi = 3.14
    funct3 = A*( (1./pi) + 0.5*cos(om*t) + 2/(3.*pi)*cos(2*om*t) - 2/(15.*pi)*(cos(4*om*t)) )
    !Write(*, *) "Function Fourier series"  

    return
    end