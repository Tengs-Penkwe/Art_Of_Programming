use crate::algorithms::SortingAlgorithm; 

/// This count the order of each element
pub struct CountingSort;
impl SortingAlgorithm for CountingSort {
    fn sort(&self, array: &mut Vec<i32>) -> Vec<i32> {
        let length = array.len();

        let mut order: Vec<i32> = vec![0; length];

        for i in 0..length {
            for j in 0..i {
                if array[i] < array[j] {
                    order[j] += 1;
                } else {
                    order[i] += 1;
                }
            }
        }

        let mut sorted: Vec<i32> =  vec![0; length];
        for i in 0..length {
            let place = order[i];
            sorted[place as usize] = array[i]; 
        }

        sorted
    }
}

