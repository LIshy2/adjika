# Adjika

```
type Either[L, R] = Left (value: L) | Right (value: R)

type List[A] = Cons(head: A, tail: List[A]) | Nil()

fun map(list, f) = 
    match list {
        is Cons(head, tail) => Cons(f(head), map(tail, f))
        is Nil() => Nil()
    }

actor PingPong {
    Waiting();
    Playing(counter: int64);
}

type Info = Info(other: MailBox[PingPong])

type Ping = Ping(from: MailBox[PingPong])

handler Info with Waiting = {
    mutate Playing(0);
    send Ping(me) to message.other;
}

handler Ping with Waiting = {
    mutate Playing(0);
    send Ping(me) to message.from;
}

handler Ping with Playing = {
    mutate Playing(counter + 1);
    send Ping(me) to message.from;
}

actor Main {
    Starting();
}

handler Starting with Starting = {
    val player1 = spawn Waiting();
    val player2 = spawn Waiting();
    send Info(player2) to player1;
}
```