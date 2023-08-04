// Referred:
// https://stackoverflow.com/questions/59428096/understanding-the-send-trait
// https://doc.rust-lang.org/nomicon/send-and-sync.html
// https://users.rust-lang.org/t/when-does-a-thread-exit/21880/2
// https://www.youtube.com/watch?v=wpIiXhQvd-M&list=PLAJ-sYO1aGdxQ_skPPtJ7PlSAjTXM-atv&index=26

use spmc;
use std::sync::{mpsc, Arc};
use std::thread;

pub trait Task {
    type Output: Send;
    fn run(&self) -> Option<Self::Output>;
}

pub struct WorkQueue<TaskType: 'static + Task + Send> {
    send_tasks: Option<spmc::Sender<TaskType>>, // Option because it will be set to None to close the queue
    recv_tasks: Arc<spmc::Receiver<TaskType>>,
    //send_output: mpsc::Sender<TaskType::Output>, // not need in the struct: each worker will have its own clone.
    recv_output: mpsc::Receiver<TaskType::Output>,
    workers: Vec<thread::JoinHandle<()>>,
}

impl<TaskType: 'static + Task + Send> WorkQueue<TaskType> {
    pub fn new(n_workers: usize) -> WorkQueue<TaskType> {
        // Create channels
        let (input_channel_tx, input_channel_rx) = spmc::channel();
        let (output_channel_tx, output_channel_rx) = mpsc::channel();

        let input_channel_rx_arc = Arc::new(input_channel_rx);

        // Start worker threoads
        let mut handles = Vec::new();
        for _ in 0..n_workers {
            let output_channel_tx = output_channel_tx.clone();
            let input_channel_rx_arc_i = input_channel_rx_arc.clone();
            // https://stackoverflow.com/questions/75126641/value-moved-into-closure-in-previous-iteration-of-loop
            handles.push(thread::spawn(move || {
                Self::run(input_channel_rx_arc_i, output_channel_tx);
            }));
        }

        // Record join handles
        WorkQueue {
            send_tasks: Some(input_channel_tx),
            recv_tasks: input_channel_rx_arc,
            recv_output: output_channel_rx,
            workers: handles,
        }
    }

    fn run(recv_tasks: Arc<spmc::Receiver<TaskType>>, send_output: mpsc::Sender<TaskType::Output>) {
        // https://stackoverflow.com/questions/26199926/how-to-terminate-or-suspend-a-rust-thread-from-another-thread
        loop {
            // println!("Hello loop");
            // Receive message from queue
            match recv_tasks.recv() // blocks current thread until msg avail
            {
                Ok(val) => {
                    // println!("Received task from queue");

                    let task_result = val.run();
                    match task_result {
                        Some(res) => {
                            if let Err(_) = send_output.send(res) {
                                println!("Error in sending");
                                // break; // Channel is closed; break the loop.
                            } else {
                                // println!("Sent result");
                            }
                        },
                        None => {
                            // println!("Task result is none\n");
                        }
                    }
                },
                Err(e) => {
                    // recv_tasks are disconnected
                    break;
                    // match e {
                    //     mpsc::TryRecvError::Disconnected => {
                    //         println!("Thread exit... Disconnected error\n ");
                    //         break;
                    //     },
                    //     _ => {
                    //         println!("Empty Queue.. Wait for tasks to be enqueued {:?} \n ", e);
                    //     }
                    // }
                }
            }
            // task_result will be Err() if the spmc::Sender has been destroyed and no more messages can be received here
        }
    }

    pub fn enqueue(&mut self, t: TaskType) -> Result<(), spmc::SendError<TaskType>> {
        // send this task to a worker
        self.send_tasks
            .as_mut()
            .expect("Task is none. The sender channel is closed")
            .send(t)
    }

    // Helper methods that let you receive results in various ways
    pub fn iter(&mut self) -> mpsc::Iter<TaskType::Output> {
        self.recv_output.iter()
    }
    pub fn recv(&mut self) -> TaskType::Output {
        self.recv_output
            .recv()
            .expect("I have been shutdown incorrectly")
    }
    pub fn try_recv(&mut self) -> Result<TaskType::Output, mpsc::TryRecvError> {
        self.recv_output.try_recv()
    }
    pub fn recv_timeout(
        &self,
        timeout: std::time::Duration,
    ) -> Result<TaskType::Output, mpsc::RecvTimeoutError> {
        self.recv_output.recv_timeout(timeout)
    }

    pub fn shutdown(&mut self) {
        // Destroy the spmc::Sender so everybody knows no more tasks are incoming;
        // drain any pending tasks in the queue; wait for each worker thread to finish.
        // HINT: Vec.drain(..)
        // https://stackoverflow.com/questions/70404178/what-is-the-best-way-to-drain-a-mspc-channel-in-rust
        // drop(self.send_tasks);
        // Send a termination signal to each worker thread so that it exits the loop gracefully.

        // Destroy the spmc::Sender so everybody knows no more tasks are incoming;
        // This should make the Thread exit as task_result will be Err and cause to break the loop
        self.send_tasks.take();

        println!("Emptying queue");
        while let Ok(_) = self.recv_tasks.try_recv() {
            
        }

        // Wait for each worker thread to finish.
        self.workers.drain(..).for_each(|handler| {
            let _ = handler.join();
        })
    }
}

impl<TaskType: 'static + Task + Send> Drop for WorkQueue<TaskType> {
    fn drop(&mut self) {
        // "Finalisation in destructors" pattern: https://rust-unofficial.github.io/patterns/idioms/dtor-finally.html
        match self.send_tasks {
            None => {} // already shut down
            Some(_) => self.shutdown(),
        }
    }
}
