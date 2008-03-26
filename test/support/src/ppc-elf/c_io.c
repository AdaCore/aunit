void bug_putstring(
  const char *buf,
  int len
)
{
   int i;
   char c;

   for (i=0; i<len; i++) {
    
     c = buf[i];
     asm volatile( "li 10,0x020          /* Code for .OUTCHR */\n\
                    mr 3, %0             /* character */\n\
                    sc"                  /* Call EPPCBUG */
 	:: "b" (c) : "3", "10" );
   }
}

void putchar(
  const char c
)
{
   asm volatile( "li 10,0x020          /* Code for .OUTCHR */\n\
                  mr 3, %0             /* character */\n\
                  sc"                  /* Call EPPCBUG */
		 :: "b" (c) : "3", "10" );
}

void bug_putnl(void)
{
   asm volatile( "li 10,0x026          /* Code for .PCRLF */\n\
                  sc"                  /* Call EPPCBUG */
		 ::: "10" );
}
