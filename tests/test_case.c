#include <stdio.h>
#include <sys/types.h>
extern int test_case(int);

int test_function(int i)
{
 int a=0;
switch(i)
{
	case 0:
	{
		a=5;
	}
	break;
	case 1 :
	{
		a=1;

	}
	break;
	case 2 :
	{
		a=2;

	}
	break;
	default:
		a=7;
	
}
return a;
}

int main()
{
  
  int i, j, k;
  int errors=0;
  int success=0;

  for (i=0;i<7;i++)
  { 
	 if (test_case(i)!=test_function(i))
	{
	  errors++;
	//printf("%d %d %d",i,test_case(i),test_function(i));
	}
	else
	  success++;
}

  printf("success,%d\nerrors,%d\ntotal,%d\n",success,errors,success+errors);

  return 0;
}
