  #include <stdio.h>
  #include <stdlib.h>
  #include <string.h>
  char* concat(char *str1, char *str2)
  {
         char *result = malloc(strlen(str1)+strlen(str2));
         strcpy(result, str1);
         strcat(result, str2);
         return result;
  }
  void rev(char *str)
  {
     char *start = str;
     char *end = start + strlen(str) - 1;
     char temp;
     while (end > start)
     {
         temp = *start;
         *start = *end;
         *end = temp;
         ++start;
         --end;
     }
  }  int main() {
int a;
int b;
int n;
int t;

while (1){
scanf("%d", &n );

a = 0;
b = 1;
while (n){
t = a;
a = b;
b = b+t;
n = n-1;
}

printf("%d",&a);

}


}
