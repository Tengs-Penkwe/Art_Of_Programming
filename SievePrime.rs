use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    // Print the entire argument list
    println!("The arguments passed are: {:?}", args);

    for (index, arg) in args.iter().enumerate() {

        if index == 0 {
            continue;
        }

        match arg.parse::<i32>() {
            Ok(value) => {
                println!("Argument {} is an integer: {}", index, value), 
                  
            }
            Err(_) => println!("Argument {} is not an integer: {}", index, arg),
        }
    }
}


