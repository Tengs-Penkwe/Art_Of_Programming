use crate::algorithms::SortingAlgorithm; 

pub struct StraightInsertion;

impl SortingAlgorithm for StraightInsertion {
    fn sort(&self, array: &mut Vec<i32>) -> Vec<i32> {
        let n = array.len();
        
        let mut sorted: Vec<i32> = Vec::with_capacity(n);

        for i in 0..n {
            match sorted.binary_search(&array[i]) {
                Ok(index) => sorted.insert(index, array[i]),
                Err(index) => sorted.insert(index, array[i]),
            }
        }

        sorted
    }
}

/// This count the order of each element
pub struct ShellSort;
impl SortingAlgorithm for ShellSort {
    fn sort(&self, array: &mut Vec<i32>) -> Vec<i32> {
        let n: f32 = array.len() as f32;

        let pass = n.log2().floor();

        for step in (1..=pass as usize).rev() {
            for start in 0..step {
                let mut values_to_sort: Vec<i32> = array.iter().cloned().skip(start).step_by(step).collect();
                values_to_sort.sort();

                for (i, &value) in values_to_sort.iter().enumerate() {
                    array[start + step * i] = value;
                }
            }
        }

        array.clone()
    }
}
