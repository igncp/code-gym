// this function is not exposed
int _calculate() {
  return -1;
}

int calculate() {
  return _calculate() * 2;
}
