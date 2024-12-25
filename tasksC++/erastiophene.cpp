#include <cstring>

typedef int integer;

integer* eratosthenes_sieve(integer n) {
    static const integer MAX_SIZE = 10000;
    static bool is_prime[MAX_SIZE];
    static integer primes[MAX_SIZE];
    integer SIZE_OF_BOOL = 1;

    if (n >= MAX_SIZE) {
        return nullptr;
    }

    memset(is_prime, true, (n + 1) * SIZE_OF_BOOL);
    is_prime[0] = is_prime[1] = false;

    for (integer i = 2; i * i <= n; i=i+1) {
        if (is_prime[i]) {
            for (integer j = i * i; j <= n; j =j+ i) {
                is_prime[j] = false;
            }
        }
    }

    integer prime_count = 0;
    for (integer i = 2; i <= n; i=i+1) {
        if (is_prime[i]) {
            primes[prime_count=prime_count+1] = i;
        }
    }

    primes[prime_count] = -1;
    return primes;
}
