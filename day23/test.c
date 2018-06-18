#include <stdio.h>
#include <inttypes.h>
#include <stdbool.h>

int main(int argc, char** argv) {

    int a,b,c,d,e,f,g,h;
    b = c = d = e = f = g = h = 0;

    a = 0;

    int cnt = 0;

    b = 65;
    c = b;
    if (a != 0)
        goto realmode;
    goto mainloop;

realmode:
    b *= 100;
    b -= -100000;
    c = b;
    c -= -17000;

mainloop:
    f = 1;
    d = 2;

label1:
    e = 2;

label2:
    g = d;
    g *= e;
    cnt++;
    g -= b;

    if ( g != 0 )
        goto label3;

    f = 0;

label3:
    e -= -1;
    g = e;
    g -= b;

    if ( g != 0 )
        goto label2;

    d -= -1;
    g = d;
    g -= b;

    if ( g != 0 )
        goto label1;

    if ( f != 0 )
        goto label5;

    h -= -1;

label5:
    g = b;
    g -= c;

    if ( g != 0 )
        goto label6;

    goto end;

label6:
    b -= -17;
    goto mainloop;

end:
    printf("[a:%d][b:%d][c:%d][d:%d][e:%d][f:%d][g:%d][h:%d]<count:%d>\n", a,b,c,d,e,f,g,h, cnt);

    return 0;
}
