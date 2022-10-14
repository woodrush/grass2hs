#include <stdio.h>
#include <stdlib.h>

int stack_depth = 4;
char curchar;

int app_W_length = 0;
int app_w_length = 0;

void unexpectedError (const char* s) {
    fprintf(stderr, "Error: Unexpected %s\n", s);
    exit(1);
}

void unexpectedCharError (char c) {
    fprintf(stderr, "Error: Unexpected %c\n", c);
    exit(1);
}

void readChar () {
    for (;;) {
        curchar = getchar();
        if (curchar == 'w' || curchar == 'W' || curchar == 'v' || curchar == EOF) {
            return;
        }
    }
}

int getLengthOf (char w) {
    int length = 0;
    for (;;) {
        if (curchar == w) {
            length++;
        } else {
            return length;
        }
        readChar();
    }
    return length;
}

void readApp () {
    app_W_length = getLengthOf('W');
    if (curchar != 'w') {
        unexpectedCharError(curchar);
    }
    app_w_length = getLengthOf('w');
}

void emitAbs (int abs_length) {
    for (int i = 0; i < abs_length; i++) {
        printf("return $ F $ \\x%d -> ", stack_depth);
        stack_depth++;
    }
}

void emitApp () {
    const int W_ind = stack_depth - app_W_length + 1;
    const int w_ind = stack_depth - app_w_length + 1;
    printf("let x%d = g f%d f%d in ", stack_depth, W_ind, w_ind);
}

void emitAppClause () {
    while (curchar != EOF && curchar != 'v') {
        readApp();
        emitApp();
        stack_depth++;
    }
    stack_depth--;
    printf("f%d\n", stack_depth);
}

void readFuncDef () {
    int initstack = stack_depth;
    printf("f%d = ", stack_depth);

    int abs_length = getLengthOf('w');
    if (curchar == EOF || curchar == 'v') {
        emitAbs(abs_length);
    }
    emitAppClause();
    stack_depth = initstack;
}

int main (void) {
    for (;;) {
        readChar();
        if (curchar == 'w') {
            readFuncDef();
        } else if (curchar == 'W') {
            int initstack = stack_depth;
            printf("f%d = ", stack_depth);
            emitAppClause();
            stack_depth = initstack;
        } else if (curchar == EOF) {
            break;
        }
        stack_depth++;
    }
    stack_depth--;
    printf("main = g f%d f%d\n", stack_depth, stack_depth);
}
