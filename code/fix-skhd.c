
#include <Carbon/Carbon.h>


int main(int argc, const char *argv[])
{
    if (IsSecureEventInputEnabled())
	printf("Secure keyboard entry is enabled.\n");

    DisableSecureEventInput();

    if (IsSecureEventInputEnabled())
	printf("Secure keyboard entry is still enabled.\n");


    return 0;
}
