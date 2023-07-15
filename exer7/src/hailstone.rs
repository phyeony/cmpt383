pub fn hailstone(n: u64) -> u64 {
    if n % 2 == 0 {
        n / 2
    } else {
        3 * n + 1
    }
}

pub fn hailstone_sequence_append(n: u64) -> Vec<u64> {
    let mut current = n;
    let mut vec = Vec::new();
    while current != 1 {
        vec.push(current);
        current = hailstone(current);
    }
    vec.push(1);
    vec
}

pub fn hailstone_sequence_prealloc(n: u64) -> Vec<u64> {
    let mut current = n;
    let mut count = 1; // starts with n.
    while current != 1 {
        count += 1;
        current = hailstone(current);
    }
    let mut vec = Vec::with_capacity(count);

    current = n;
    while current != 1 {
        vec.push(current);
        current = hailstone(current);
    }
    vec.push(1);
    vec
}


// for n = 686901248
// hailstone_sequence_append_686901248
// Additional Statistics:
// Lower bound	Estimate	Upper bound
// Slope	945.78 ns	949.01 ns	952.66 ns
// R²	0.9672725	0.9688605	0.9668337
// Mean	955.49 ns	958.53 ns	961.56 ns
// Std. Dev.	13.962 ns	15.517 ns	16.836 ns
// Median	952.85 ns	962.92 ns	967.85 ns
// MAD	12.526 ns	16.941 ns	22.115 ns

// hailstone_sequence_prealloc_686901248
// Lower bound	Estimate	Upper bound
// Slope	641.55 ns	643.17 ns	644.89 ns
// R²	0.9806430	0.9815099	0.9805243
// Mean	648.28 ns	650.97 ns	653.94 ns
// Std. Dev.	10.409 ns	14.530 ns	18.477 ns
// Median	646.30 ns	648.41 ns	649.96 ns
// MAD	7.0931 ns	10.701 ns	14.275 ns