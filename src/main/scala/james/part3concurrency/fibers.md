## Concurrency vs Parallelism

Paralellism - multiple computations running at the same time

Concurrency - multiple computations overlap throughout their life times

Not the same thing!!!

Parallel programs may not necessarily be concurrent
- e.g. tasks are independent

Concurrent programs may not necessarily be parallel
- e.g. multi-tasking on the same CPU

## Fiber

**Fiber** = description of an effect being executed on some other thread

```
def createFiber: Fiber[IO, Throwable, String]
                 effect type, failure, result]
```

Creating a fiber is an *effectful operation* => will be wrapped in IO
Managing a fiber is an *effectful operation* => will be wrapped in IO

```
def runOnSomeOtherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
  fib <- io.start
  result <- fib.join
} yield result
```

## How Fibers work

- Cats Effect has a thread pool that manages the execution of effects

Thread: active = can run code
Fiber: passive = description of an effect

benefit is we can have far more fibers than threads 

**CE schedules fibers for execution**

## Motivation for Fibers

Why we need Fibers

- no more need for threads and locks
- delegate thread management
- avoid asynchronous code with callbacks
- maintain pure functional programming
- keep low level primitives (blocking, waiting, joining, cancelling etc)

Fiber scheduling concepts and impl details

- blocking effects in a fiber leading to *descheduling*
- semantic blocking (fiber looks blocked but no JVM thread actually blocked)
- cooperative scheduling
- the same fiber can run on _multiple__ JVM threads
- work-stealing thread pool