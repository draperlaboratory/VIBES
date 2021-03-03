
int foo(unsigned int x){
  if(x > 0){
    return 2;
  }
  else{
    return 1;
  }
}

int main() {
  return foo(-1);
}
