fn gcd(a: i64, b: i64) -> i64 {
    // Referred https://www.geeksforgeeks.org/program-to-find-gcd-or-hcf-of-two-numbers/
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Rational {
    n: i64,
    d: i64,
}

impl Rational {
    pub fn new(n: i64, d: i64) -> Rational {
        Rational {
            n,
            d
        }
    }
    // TODO: the reduce method
    pub fn reduce(&mut self) {
        let gcd = gcd(self.n, self.d);
        self.n /= gcd;
        self.d /= gcd;
    }
}

impl From<i64> for Rational {
    // TODO
    fn from(val: i64) -> Rational {
        Rational::new(val,1)
    }
}
