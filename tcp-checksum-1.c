
#include <stdint.h>
#include <stdio.h>

uint16_t checksum(uint8_t *data, size_t len)
{
    uint16_t csum = 0;
    for (size_t i = 0; i < (len&~1); i += 2) {
        uint16_t v = (data[i]<<8) | data[i+1];
        csum = ~(csum ^ v);
    }
    if (len & 1) csum = ~(csum ^ (data[len-1]<<8));
    return ~csum;
}

int main(void)
{
    uint8_t buf[4096];

    size_t len = fread(buf, 1, 4096, stdin);
    printf("%x\n", checksum(buf, len));
}
