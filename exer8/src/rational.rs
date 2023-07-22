use std::fmt;

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
    pub fn reduce(&mut self) {
        let gcd = gcd(self.n, self.d);
        self.n /= gcd;
        self.d /= gcd;
    }
}

impl fmt::Display for Rational {
    // This trait requires `fmt` with this exact signature.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Write strictly the first element into the supplied output
        // stream: `f`. Returns `fmt::Result` which indicates whether the
        // operation succeeded or failed. Note that `write!` uses syntax which
        // is very similar to `println!`.
        write!(f, "{}/{}", self.n, self.d)
    }
}

impl From<i64> for Rational {
    fn from(val: i64) -> Rational {
        Rational::new(val,1)
    }
}

impl From<Rational> for f64 {
    fn from(val: Rational) -> f64 {
        val.n as f64 / val.d as f64
    }
}