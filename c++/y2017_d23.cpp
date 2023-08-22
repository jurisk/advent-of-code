#include <cassert>
#include <vector>
#include <iostream>

enum class TaskPart {
    PART_1 = 1,
    PART_2 = 2
};

int calculate(TaskPart part) {
    assert(part == TaskPart::PART_1 || part == TaskPart::PART_2);

    int times_mul_invoked = 0;

    int b, c, d, e, f, h = 0;

    // 1: set b 65
    b = 65;

    // 2: set c b
    c = b;

    // 3: jnz a 2
    if (part == TaskPart::PART_2) {
        // 4: jnz 1 5
        // 5: mul b 100
        b *= 100;
        times_mul_invoked++;

        // 6: sub b -100000
        b += 100000;

        // 7: set c b
        // 8: sub c -17000
        c = b + 17000;
    }

l9: 
    // 9: set f 1
    // 10: set d 2
    f = 1;
    d = 2;

    do {
        // 11: set e 2
        e = 2;

        do {
            // 12: set g d
            // 13: mul g e
            // 14: sub g b
            times_mul_invoked++;

            // 15: jnz g 2
            if (d * e == b) {
                // 16: set f 0
                f = 0;
            }
            
            // 17: sub e -1
            e++;

            // 18: set g e
            // 19: sub g b
            // 20: jnz g -8
        } while (e != b);

        // 21: sub d -1
        d++;

        // 22: set g d
        // 23: sub g b
        // 24: jnz g -13
    } while (d != b);

    // Looking at this debug output, we can see that `f` indicates whether `b` is prime
    // std::cout << "b = " << b << ", f = " << f << '\n';

    // 25: jnz f 2
    if (f == 0) {
        // 26: sub h -1
        h++;
    }

    // 27: set g b
    // 28: sub g c
    // 29: jnz g 2
    if (b != c) {
        // 30: jnz 1 3
        // 31: sub b -17
        // 32: jnz 1 -23
        b += 17;
        goto l9;
    }

    if (part == TaskPart::PART_1) {
        return times_mul_invoked;
    } else if (part == TaskPart::PART_2) {
        return h;
    } else {
        return -1;
    }
}

std::vector<bool> sieveOfEratosthenes(int n) {
    std::vector<bool> prime(n + 1, true);
    prime[0] = prime[1] = false;

    for (int p = 2; p * p <= n; p++) {
        if (prime[p]) {
            for (int i = p * p; i <= n; i += p) {
                prime[i] = false;
            }
        }
    }
  
    return prime;
}

// This does what `calculate()` does, only more efficiently
int calculate_using_sieve(TaskPart part) {
    assert(part == TaskPart::PART_2);

    int b = (65 * 100) + 100000;
    int c = b + 17000;

    std::vector<bool> primes = sieveOfEratosthenes(c);

    int result = 0;

    for (int i = b; i <= c; i += 17) {
        if (!primes[i]) {
            result++;
        }
    }

    return result;
}

int main() {
    int part_1 = calculate(TaskPart::PART_1);
    std::cout << "Part 1: " << part_1 << '\n';
    assert(part_1 == 3969);

    int part_2 = calculate_using_sieve(TaskPart::PART_2);
    std::cout << "Part 2: " << part_2 << '\n';
    assert(part_2 == 917);

    return 0;
}