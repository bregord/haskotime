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
int x;
float y;
char* z;
char* w;
int u;

if (x){
y = 1.0;
}else{
x = 10;
}

x = 10;
y = 1.0;
printf("%s",&z);

while (x){
y = y*2.0;
x = x-1;
}


}
