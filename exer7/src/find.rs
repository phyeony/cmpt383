pub fn find_elt<T: Eq>(values: &Vec<T>, elt: T) -> Option<usize> {
    for (index, element) in values.iter().enumerate() {
        if *element == elt {
            return Some(index);
        }
    }
    None
}
