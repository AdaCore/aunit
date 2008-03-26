typedef int jmp_buf[5];

static jmp_buf JMPBUF[5];
static int insetjmp = 0;

int mysetjmp (void (*next (void *)), void *arg1)
{
  int ret;
  if (!__builtin_setjmp (JMPBUF[insetjmp])) {
    insetjmp++;
    next(arg1);
    insetjmp--;
    return 0;
  }
  return 1;
}

void mylongjmp (void)
{
  insetjmp--;
  if (insetjmp >= 0)
    __builtin_longjmp (JMPBUF[insetjmp], 1);
  //  else
  //    abort();
}
