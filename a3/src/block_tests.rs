#[cfg(test)]
mod block_tests {
    use crate::block::Block;
    use std::fmt::Write;

    #[test]
    fn example() {
        assert_eq!(1 + 1, 2);
    }

    #[test]
    fn test_hash_string_for_proof() {
        let mut b0 = Block::initial(16);
        b0.set_proof(56231);
        let mut b1 = Block::next(&b0, String::from("message"));
        b1.set_proof(2159);

        let b0_result = b0.hash_string();
        assert_eq!(
            b0_result,
            "0000000000000000000000000000000000000000000000000000000000000000:0:16::56231"
        );

        let b1_result = b1.hash_string();
        assert_eq!(
            b1_result,
            "6c71ff02a08a22309b7dbbcee45d291d4ce955caa32031c50d941e3e9dbd0000:1:16:message:2159"
        );
    }

    #[test]
    fn test_hash() {
        let mut b0 = Block::initial(16);
        b0.set_proof(56231);
        let mut b1 = Block::next(&b0, String::from("message"));
        b1.set_proof(2159);


        let mut b0_result = String::new();
        // fmt::Write for String always returns Ok() and never Err.
        write!(&mut b0_result, "{:02x}", b0.hash()).unwrap();

        assert_eq!(
            b0_result,
            "6c71ff02a08a22309b7dbbcee45d291d4ce955caa32031c50d941e3e9dbd0000"
        );

        let mut b1_result = String::new();
        // fmt::Write for String always returns Ok() and never Err.
        write!(&mut b1_result, "{:02x}", b1.hash()).unwrap();

        assert_eq!(
            b1_result,
            "9b4417b36afa6d31c728eed7abc14dd84468fdb055d8f3cbe308b0179df40000"
        );
    }

    #[test]
    fn test_is_valid_for_proof() {
        let mut b0 = Block::initial(19);
        b0.set_proof(87745);
        let mut b1 = Block::next(&b0, String::from("hash example 1234"));
        b1.set_proof(1407891);

        assert_eq!(true, b0.is_valid());
        assert_eq!(true, b1.is_valid());

        b1.set_proof(346082);
        assert_eq!(false, b1.is_valid());
    }

    #[test]
    fn test_mining() {
        let mut b0 = Block::initial(7);
        b0.mine(1);
        println!("{}", b0.hash_string());
        println!("{:02x}", b0.hash());
        assert_eq!(true, b0.is_valid());

        let mut b1 = Block::next(&b0, String::from("this is an interesting message"));
        b1.mine(1);
        println!("{}", b1.hash_string());
        println!("{:02x}", b1.hash());
        assert_eq!(true, b1.is_valid());

        let mut b2 = Block::next(&b1, String::from("this is not interesting"));
        b2.mine(1);
        println!("{}", b2.hash_string());
        println!("{:02x}", b2.hash());
        assert_eq!(true, b2.is_valid());
    }

    #[test]
    fn test_mining_2() {
        let mut b0 = Block::initial(20);
        b0.mine(1);
        println!("{}", b0.hash_string());
        println!("{:02x}", b0.hash());
        assert_eq!(true, b0.is_valid());

        let mut b1 = Block::next(&b0, String::from("this is an interesting message"));
        b1.mine(1);
        println!("{}", b1.hash_string());
        println!("{:02x}", b1.hash());
        assert_eq!(true, b1.is_valid());

        let mut b2 = Block::next(&b1, String::from("this is not interesting"));
        b2.mine(1);
        println!("{}", b2.hash_string());
        println!("{:02x}", b2.hash());
        assert_eq!(true, b2.is_valid());
    }
}
