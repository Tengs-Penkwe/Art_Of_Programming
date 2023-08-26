// use std::time::{Instant};
use rand::Rng;

// Import your sort algorithm modules here
// For example: 
// mod bubble_sort;
// mod merge_sort;
// mod quick_sort;

fn generate_random_input(size: usize) -> Vec<i32> {
    let mut rng = rand::thread_rng();
    (0..size).map(|_| rng.gen_range(1..size as i32)).collect()
}

fn main() {
    // Specify the sizes of input arrays you want to test
    let input_sizes = vec![10, 100, 1000, 10000];
    
    // Loop through each input size
    for size in &input_sizes {
        let input_data = generate_random_input(*size);
        
        println!("Testing with input size: {:?}", size);
        
        // Call and time each sorting algorithm here
        // For example:
        // let mut data_copy = input_data.clone();
        // let start_time = Instant::now();
        // bubble_sort::sort(&mut data_copy);
        // let elapsed_time = start_time.elapsed();
        // println!("Bubble Sort took: {:?}", elapsed_time);
        
        // Repeat the above steps for other sorting algorithms
        
        println!("----------------------------------");
    }
}

fn counting_sort(){

}
