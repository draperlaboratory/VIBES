int patch_fun(int a[],int i){
    if(i < 3 && i >= 0){
        return a[i];
    }
    return -1;
}


int main(){
    int x[] = {5,4,3};
    return patch_fun(x, 3);
}
