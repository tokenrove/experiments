/* Silly idea: use randomness from ASLR or similar as seed for an
 * internal PRNG.  Don't do this.
 */

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#if defined(__linux__) && defined(__x86_64__)
#include <sys/auxv.h>

uint32_t seed;

extern int main();

__attribute__((constructor))
static void init_seed(void)
{
    /* per http://elixir.free-electrons.com/linux/v4.13.9/source/arch/x86/mm/mmap.c#L50,
       http://elixir.free-electrons.com/linux/v4.13.9/source/arch/x86/mm/mmap.c#L143 */
    uintptr_t vdso = (getauxval(AT_SYSINFO_EHDR))>>12,
              code = ((uintptr_t)&main)>>12;
    seed = (uint64_t)( (vdso&((1UL<<22)-1)) ^ // actually 22 bits?  not clear
                       ((code&((1UL<<22)-1))<<22));
    if (!seed) abort();
}
#elif defined(__OpenBSD__)  /* kudos phoebe; even better than ASLR */
static uint32_t seed __attribute__((section(".openbsd.randomdata")));
#else
#error "Not implemented for your platform yet"
#endif

int main(void)
{
    printf("%u\n", seed);
    return 0;
}
