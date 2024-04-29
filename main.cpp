#include <stdio.h>

extern "C" int my_printf(const char* buffer, ...);

int main()
{

    int a = my_printf("1)'%o'\n2)'%d %s %x %u%%%c%b'\n3)'%d %s %x %d%%%c%b %p'\n", -20, 120, "love", 3802, 100, 33, 127,
                                                                         -1, "love", 3802, 100, 33, 127, "love");
    int b = 0;

    // my_printf("abballllllllllllllll%n-best\n", &b);
    printf("%p\n", "love");

    return 0;
}
