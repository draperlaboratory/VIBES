struct S {
  int x;
  char y;
  short z;
};

void foo(struct S *s) { s->y = 3; }

int main()
{
  struct S s;
  foo(&s);
  return s.y;
}
