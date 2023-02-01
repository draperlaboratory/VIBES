
int f1() {
  return 1;
}

int f2() {
  return 2;
}

int main() {
  int x = f1() + f2();
  if (x == 7) {
    return 1;
  } else {
    return 0;
  }
}
