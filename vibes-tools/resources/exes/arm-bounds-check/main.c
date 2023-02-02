int patch_fun(int a[],int i){
    return a[i];
}


int main(){
    int x[] = {5,4,3};
    return patch_fun(x, 3);
}