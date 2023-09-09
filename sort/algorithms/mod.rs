pub mod rough_sort;
pub mod insertion_sort;

pub trait SortingAlgorithm {
    fn sort(&self, array: &mut Vec<i32>) -> Vec<i32>;
    // fn sort<T: PartialOrd + Clone>(&self, array: &mut Vec<T>) -> Vec<T>;
}
