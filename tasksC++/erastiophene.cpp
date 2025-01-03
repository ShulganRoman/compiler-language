typedef int integer;

integer* eratosthenes_sieve(integer n) {
    const integer MAX_SIZE = 10000;
    bool is_prime[MAX_SIZE];
    static integer primes[MAX_SIZE];

    for (integer i = 0; i <= n; i = i + 1) {
        is_prime[i] = true;
    }

    is_prime[0] = is_prime[1] = false;

    for (integer i = 2; i * i <= n; i = i + 1) {
        if (is_prime[i]) {
            for (integer j = i * i; j <= n; j = j + i) {
                is_prime[j] = false;
            }
        }
    }

    integer prime_count = 0;
    for (integer i = 2; i <= n; i = i + 1) {
        if (is_prime[i]) {
            primes[prime_count] = i;
            prime_count = prime_count + 1;
        }
    }

    primes[prime_count] = -1;
    return primes;
}
