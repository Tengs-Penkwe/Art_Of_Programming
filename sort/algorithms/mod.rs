pub mod rough_sort;

pub trait SortingAlgorithm {
    fn sort(&self, array: &mut Vec<i32>) -> Vec<i32>;
}
