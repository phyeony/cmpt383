// Dynamic binding costed about 16 µs more.
// Result: 
// Zero Area Checks/any_shape_zero_area
//                         time:   [29.826 µs 30.025 µs 30.236 µs]
// Zero Area Checks/any_circle_zero_area
//                         time:   [13.515 µs 13.595 µs 13.675 µs]
// Zero Area Checks/any_rectangle_zero_area
//                         time:   [16.815 µs 16.931 µs 17.035 µs]

pub trait Shape {
    fn area(&self) -> f64;
    fn description(&self) -> &str; // used to inspect types during testing
}

#[derive(Debug, Clone)]
pub struct Circle {
    radius: f64,
}
impl Circle {
    pub fn new(radius: f64) -> Circle {
        Circle { radius }
    }
    pub fn random() -> Circle {
        Circle {
            radius: rand::random::<f64>() + 1.0,
        }
    }
}
impl Shape for Circle {
    fn area(&self) -> f64 {
        std::f64::consts::PI * self.radius.powi(2)
    }
    fn description(&self) -> &str {
        "circle"
    }
}

#[derive(Debug, Clone)]
pub struct Rectangle {
    width: f64,
    height: f64,
}
impl Rectangle {
    pub fn new(width: f64, height: f64) -> Rectangle {
        Rectangle { width, height }
    }
    pub fn random() -> Rectangle {
        Rectangle {
            width: rand::random::<f64>() + 1.0,
            height: rand::random::<f64>() + 1.0,
        }
    }
}
impl Shape for Rectangle {
    fn area(&self) -> f64 {
        self.width * self.height
    }
    fn description(&self) -> &str {
        "rectangle"
    }
}

pub fn any_circle_zero_area(shapes: &Vec<Box<Circle>>) -> bool {
    shapes.iter().any(|x| x.area() == 0.0)
}
pub fn any_rectangle_zero_area(shapes: &Vec<Box<Rectangle>>) -> bool {
    shapes.iter().any(|x| x.area() == 0.0)
}
pub fn any_shape_zero_area(shapes: &Vec<Box<dyn Shape>>) -> bool {
    shapes.iter().any(|x| x.area() == 0.0)
}

// generate 2*n Circles
pub fn make_circle_vec(n: usize) -> Vec<Box<Circle>> {
    let mut vec = Vec::with_capacity(2*n);
    for _ in 0..2*n {
        vec.push(Box::new(Circle::random()));
    }
    vec
}
// generate 2*n Rectangles
pub fn make_rectangle_vec(n: usize) -> Vec<Box<Rectangle>> {
    let mut vec = Vec::with_capacity(2*n);
    for _ in 0..2*n {
        vec.push(Box::new(Rectangle::random()));
    }
    vec
}
// generate n Circles and n Rectangles
pub fn make_mixed_vec(n: usize) -> Vec<Box<dyn Shape>> {
    let mut vec: Vec<Box<dyn Shape>> = Vec::with_capacity(2*n);
    for _ in 0..n {
        vec.push(Box::new(Circle::random()));
        vec.push(Box::new(Rectangle::random()));
    }
    vec
}