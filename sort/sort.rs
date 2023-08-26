use std::time::{Instant};
use rand::Rng;

mod algorithms;
use algorithms::SortingAlgorithm;
use algorithms::rough_sort::{CountingSort};

fn generate_random_input(size: usize) -> Vec<i32> {
    let mut rng = rand::thread_rng();
    (0..size).map(|_| rng.gen_range(1..size as i32)).collect()
}

fn is_sorted(array: Vec<i32>) -> bool {
    for i in 1..array.len() {
        if array[i - 1] > array[i] {
            return false;
        }
    }
    true
}

fn main() {
    let input_sizes = vec![256, 1024, 4096, 16384];

    let algorithms: Vec<Box<dyn SortingAlgorithm>> = vec![
        Box::new(CountingSort),
    ];

    
    // Loop through each input size
    for size in &input_sizes {
        let input_data = generate_random_input(*size);
        println!("Testing with input size: {}", size);
        
        for algo in &algorithms {
            let mut data_copy = input_data.clone();
            let start_time = Instant::now();
            let sorted = algo.sort(&mut data_copy);
            let elapsed_time = start_time.elapsed();
            assert!(is_sorted(sorted));
            println!("Counting Sort took: {:?}", elapsed_time.as_millis());
        }
        
        println!("----------------------------------");
    }
}

