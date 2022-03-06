program oscillation
    use dflib
    implicit none
    
    !variable definition
    integer i, j, k, i_status, im_fi, im_v, kx, ky
    real(8) fi, v, fi0, v0, dt, om2, gam, a, fi_new, v_new, a_new, x, a_w, E0, E
    !end of variable definition
    
    fi0=1.; v0=0.; gam=0.; om2=1.
    dt = 0.01
    fi = fi0; v = v0
    x = 0.; kx = 5; ky = 1000

    E0 = -(v0**2/2. - om2**2*cos(fi0))
    
    DO I = 1, 5000   !   Euler method
        a = -(2.)*gam*v - om2*sin(fi)
        fi = fi + v*dt 
        v = v + a*dt
            
        im_fi = fi*100
        im_v = v*100
        
        x = x + kx*dt
        E = -(v*v/2. - om2*cos(fi) + E0)*ky
        
        i_status = SETCOLOR(INT(2))
        i_status = SETPIXEL (im_fi + 200, im_v + 200)
        
        i_status = SETCOLOR(INT2(2))
        i_status = SETPIXEL( x+50, E+600 )
        i_status = SETCOLOR(INT2(7))
        i_status = SETPIXEL( x+50, 601 )
        i_status = SETPIXEL( 50, 601 - 2*x/3 )
            
        
    ENDDO
   
    fi0=1.; v0=0.; gam=0.; om2=1.
    dt = 0.01
    fi = fi0; v = v0; x = 0. 
    
    E0 = -(v0**2/2. - om2**2*cos(fi0))
    
    DO I = 1, 5000   !  Euler Cromer Method
        a = -(2.)*gam*v - om2*sin(fi)
        v = v + a*dt   
        fi = fi + v*dt 
        x = x + kx*dt
        E = -(v*v/2. - om2*cos(fi) + E0)*ky*50
        
        im_fi = fi*100
        im_v = v*100
        
        i_status = SETCOLOR(INT(3))
        i_status = SETPIXEL (im_fi + 550, im_v + 200)
        i_status = SETCOLOR(INT2(3))
        i_status = SETPIXEL( x+450, -E+601 ) 
        i_status = SETCOLOR(INT2(7))
        i_status = SETPIXEL( x+450, 601 )
        i_status = SETPIXEL( 450, 601 - 2*x/3 )
        
    ENDDO
    
    
    fi0=1.; v0=0.; gam=0.; om2=1.
    dt = 0.01
    fi = fi0; v = v0; x = 0. 
    kx = 5
    
    E0 = -(v0**2/2. - om2**2*cos(fi0))
    
    fi_new = fi0
    v_new = v0
    
    DO I = 1, 5000 !  Predictor-Corrector Method
        a = -(2.)*gam*v - om2*sin(fi)
        
        fi_new = fi + v*dt
        v_new = v + a*dt    
        a_new = -(2.)*gam*v - om2*sin(fi)
        
        fi = fi + v*dt + a*dt*dt/2.
        a_w = -(2.)*gam*v_new - Om2*sin(fi)
        v = v + (a + a_w)*dt/2.
        
        x = x + kx*dt
        E = -(v*v/2. - om2*cos(fi) + E0)*ky*1000
        
        im_fi = fi*100
        im_v = v*100
        
        i_status = SETCOLOR(INT(5))
        i_status = SETPIXEL (im_fi + 1050, im_v + 200)
        
        IF (dt/3*3 == dt) then
            i_status = SETCOLOR(INT2(5))
            i_status = SETPIXEL(x+750, -E+600)
        
            i_status = SETCOLOR(INT2(7))
            i_status = SETPIXEL(x+750, 601)
            i_status = SETPIXEL(750, 601 - x/10)
        ENDIF
        
    ENDDO
    

    write (*, *) 'Press any key to exit the progmam'
    write (*, *) ' '
    write (*, *) 'Euler Method          Euler-Cromer Method          Predictor-Corrector Method'
    read (*, *) 
    
    
    end program oscillation