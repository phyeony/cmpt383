use crate::queue::{Task, WorkQueue};
use digest::consts::U32;
use sha2::digest::generic_array::GenericArray;
use sha2::{Digest, Sha256};
use std::fmt::Write;
use std::sync;

type Hash = GenericArray<u8, U32>;

#[derive(Debug, Clone)]
pub struct Block {
    prev_hash: Hash,
    generation: u64,
    difficulty: u8,
    data: String,
    proof: Option<u64>,
}

impl Block {
    pub fn initial(difficulty: u8) -> Block {
        // create and return a new initial block
        Block {
            prev_hash: Hash::default(), //https://doc.rust-lang.org/nightly/core/primitive.u8.html
            generation: 0,
            difficulty,
            data: "".to_string(),
            proof: None,
        }
    }

    pub fn next(previous: &Block, data: String) -> Block {
        // create and return a block that could follow `previous` in the chain
        Block {
            prev_hash: previous.hash(),
            generation: previous.generation + 1,
            difficulty: previous.difficulty,
            data,
            proof: None,
        }
    }

    pub fn hash_string_for_proof(&self, proof: u64) -> String {
        // return the hash string this block would have if we set the proof to `proof`.
        let mut prev_hash_string = String::new();
        // fmt::Write for String always returns Ok() and never Err.
        write!(&mut prev_hash_string, "{:02x}", self.prev_hash).unwrap();
        format!(
            "{}:{}:{}:{}:{:?}",
            prev_hash_string, self.generation, self.difficulty, self.data, proof
        )
    }

    pub fn hash_string(&self) -> String {
        // self.proof.unwrap() panics if block not mined
        let p = self.proof.unwrap();
        self.hash_string_for_proof(p)
    }

    pub fn hash_for_proof(&self, proof: u64) -> Hash {
        // return the block's hash as it would be if we set the proof to `proof`.
        let mut hasher = Sha256::new();
        let hash_string = self.hash_string_for_proof(proof);
        hasher.update(hash_string.as_bytes());
        hasher.finalize()
    }

    pub fn hash(&self) -> Hash {
        // self.proof.unwrap() panics if block not mined
        let p = self.proof.unwrap();
        self.hash_for_proof(p)
    }

    pub fn set_proof(self: &mut Block, proof: u64) {
        self.proof = Some(proof);
    }

    pub fn is_valid_for_proof(&self, proof: u64) -> bool {
        // would this block be valid if we set the proof to `proof`?
        let n_bytes = self.difficulty / 8;
        let n_bits = self.difficulty % 8;
        let hash = self.hash_for_proof(proof);
        for i in 0..n_bytes {
            if hash[hash.len() - 1 - i as usize] != 0u8 {
                return false;
            }
        }
        // if hash[hash.len() - 1 - n_bytes as usize] & !((1 << n_bits) as u8) != 0 {
        //     return false;
        // }  
        if hash[hash.len() - 1 - n_bytes as usize] & ((1 << n_bits) - 1) != 0 {
            return false;
        }      
        return true;
    }

    pub fn is_valid(&self) -> bool {
        if self.proof.is_none() {
            return false;
        }
        self.is_valid_for_proof(self.proof.unwrap())
    }

    // Mine in a very simple way: check sequentially until a valid hash is found.
    // This doesn't *need* to be used in any way, but could be used to do some mining
    // before your .mine is complete. Results should be the same as .mine (but slower).
    pub fn mine_serial(self: &mut Block) {
        let mut p = 0u64;
        while !self.is_valid_for_proof(p) {
            p += 1;
        }
        self.proof = Some(p);
    }

    pub fn mine_range(self: &Block, workers: usize, start: u64, end: u64, chunks: u64) -> u64 {
        // With `workers` threads, check proof values in the given range, breaking up
        // into `chunks` tasks in a work queue. Return the first valid proof found.
        // HINTS:
        // - Create and use a queue::WorkQueue.
        // - Use sync::Arc to wrap a clone of self for sharing.

        if chunks == 0 {
            // if chunks = 0, just return 0.
            return 0;
        }
        let mut q = WorkQueue::<MiningTask>::new(workers);

        if chunks > end - start {
            // if the chunk is bigger than the range, we need just one task.
            let task = MiningTask {
                block: self.clone().into(),
                start,
                end,
            };

            q.enqueue(task).unwrap();

            let result = q.recv();
            q.shutdown();
            return result;
        } else {
            let proofs_per_chunk = ((end - start + 1) + chunks - 1) / chunks;
            for i in 0..chunks {
                let task = MiningTask {
                    block: self.clone().into(),
                    start: start + i * proofs_per_chunk,
                    end: start + (i + 1) * proofs_per_chunk - 1,
                };

                q.enqueue(task).unwrap();
            }

            for _ in 0..chunks {
                let result: u64 = q.recv();
                // result is the valid proof!
                q.shutdown();
                return result;
            }
            return 0;
        }
    }

    pub fn mine_for_proof(self: &Block, workers: usize) -> u64 {
        let range_start: u64 = 0;
        let range_end: u64 = 8 * (1 << self.difficulty); // 8 * 2^(bits that must be zero)
        let chunks: u64 = 2345;
        self.mine_range(workers, range_start, range_end, chunks)
    }

    pub fn mine(self: &mut Block, workers: usize) {
        self.proof = Some(self.mine_for_proof(workers));
    }
}

struct MiningTask {
    block: sync::Arc<Block>,
    start: u64,
    end: u64,
}

impl MiningTask {
    // implement MiningTask::new(???) -> MiningTask
    pub fn new(block: sync::Arc<Block>, start: u64, end: u64) -> MiningTask {
        MiningTask { block, start, end }
    }
}

impl Task for MiningTask {
    type Output = u64;

    fn run(&self) -> Option<u64> {
        for proof in self.start..=self.end {
            if self.block.is_valid_for_proof(proof) {
                return Some(proof);
            }
        }
        None
    }
}
