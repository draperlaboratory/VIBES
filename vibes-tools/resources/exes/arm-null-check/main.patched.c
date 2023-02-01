#include <stddef.h>

int patch_fun(int *x){
    if(x == NULL){
        return -1;
    }
    return *x;
}


int main(){
    int x = 5;
    return patch_fun(&x);
}
