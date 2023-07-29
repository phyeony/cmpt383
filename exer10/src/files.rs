use std::fmt;
#[derive(Debug, Clone)]
pub struct SummationError {
    msg: String,
}

impl std::error::Error for SummationError {}
impl fmt::Display for SummationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}
impl From<std::io::Error> for SummationError {
    fn from(e: std::io::Error) -> SummationError {
        SummationError {
            msg: format!("io::Error: {}", e),
        }
    }
}
impl From<std::num::ParseIntError> for SummationError {
    fn from(e: std::num::ParseIntError) -> SummationError {
        SummationError {
            msg: format!("ParseIntError: {}", e),
        }
    }
}

pub fn sum_file_1(file_path: &std::path::Path) -> Result<i64, SummationError> {
    let content = std::fs::read_to_string(file_path);
    match content {
        Ok(content) => {
            let lines: Vec<&str> = content.trim().split('\n').collect();
            let integers: Result<Vec<i64>, std::num::ParseIntError> = lines.iter().map(|x| x.parse::<i64>()).collect();
            match integers {
                Ok(integers) => Ok(integers.iter().sum()),
                Err(error) => Err(SummationError::from(error))
            }
        },
        Err(error) => {
            Err(SummationError::from(error))
        },
    }
}

pub fn sum_file_2(file_path: &std::path::Path) -> Result<i64, SummationError> {
    let content = std::fs::read_to_string(file_path)?;
    let lines: Vec<&str> = content.trim().split('\n').collect();
    let integers: Vec<i64> = lines.iter().map(|x| x.parse::<i64>()).collect::<Result<Vec<i64>, _>>()?;
    Ok(integers.iter().sum())
}
