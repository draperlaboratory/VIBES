int patch_fun(int *x){
    return *x;
}


int main(){
    int x = 5;
    return patch_fun(&x);
}
