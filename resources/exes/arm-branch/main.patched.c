
int foo(unsigned int x){
  if(x > 0){
    return 0;
  }
  else{
    return 1;
  }
}

int main() {
  return foo(-1);
}
