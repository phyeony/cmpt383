use criterion::{criterion_group, criterion_main, Criterion};
use exer8::sort;
use exer8::sort_tests::random_vec;

const N: usize = 10000;

pub fn sort_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("Sorting");
    group.bench_function("quicksort", |b| {
        let v = random_vec(N);
        b.iter(|| {
            let mut vv = v.clone();
            sort::quicksort(&mut vv);
        })
    });
    group.bench_function(".sort", |b| {
        let v = random_vec(N);
        b.iter(|| {
            let mut vv = v.clone();
            vv.sort();
        })
    });
    group.bench_function(".sort_unstable", |b| {
        let v = random_vec(N);
        b.iter(|| {
            let mut vv = v.clone();
            vv.sort_unstable();
        })
    });
    group.bench_function("just_clone", |b| {
        // for interest, how much of these times is just the .clone()?
        let v = random_vec(N);
        b.iter(|| {
            let mut _vv = v.clone();
        })
    });
    group.finish();
    c.bench_function("insertion_sort", |b| {
        // insertion_sort should be slower, but how much?
        let v = random_vec(N);
        b.iter(|| {
            let mut vv = v.clone();
            sort::insertion_sort(&mut vv, 0, v.len() - 1);
        })
    });
}

criterion_group!(benches, sort_benchmark);
criterion_main!(benches);
