use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    // Print the entire argument list
    println!("The arguments passed are: {:?}", args);

    for (index, arg) in args.iter().enumerate() {
        println!("Argument {} is {}", index, arg);
    }
}

fn mutl_acc0(r: i32, n: i32, a: i32) -> i32 {
    if (n==1)
}
