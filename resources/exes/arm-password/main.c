int patch_fun(char *passwd){
    if(strcmp(passwd, "MyGoodPassword7") == 0){
        return 0;
    }
    else{
        return -1;
    }
}


int main(){
    char* password = "MyGoodPassword";
    return patch_fun(password);
}
