#[cfg(test)]
use crate::rational::Rational;

#[test]
fn rational_test() {
    let mut r = Rational::new(6, 8);
    assert_eq!(format!("{:?}", r), "Rational { n: 6, d: 8 }");
    r.reduce();
    assert_eq!(format!("{:?}", r), "Rational { n: 3, d: 4 }");
    let four1 = Rational::from(4);
    let four2 = Rational::new(4, 1);
    assert_eq!(four1, four2);

    //Test display
    println!("rational r 3/4: {}", r);
    let r2 = Rational::new(-6,8);
    println!("rational r2 -6/8: {}", r2);

    //Test type conversion to f64
    let f1 = f64::from(r.clone());
    let f2:f64 = r.into();
    let f3 = f64::from(r2);
    let f:f64 = 0.75;
    println!("Converted with r 3/4 with f64::from: {}", f1);
    assert_eq!(f1, f);
    println!("Converted with r 3/4 with r.into(): {}", f2);
    assert_eq!(f2, f);
    println!("Converted with r2 -6/8 with f64::from: {}", f3);
    assert_eq!(f3, -f);
}
