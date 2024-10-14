#include "third_party/concurrentqueue.hpp"
#include "third_party/BS_thread_pool.hpp"
#include <atomic>
#include <iostream>



template <typename T>
struct HandlerPtr {
    void (*_handler)(void*, T, void*);

    void operator()(void* state, T message, void* mailbox) {
        this->_handler(state, message, mailbox);
    }
};

template <typename T>
class Message {
    HandlerPtr<T> _handler;
    T _data;

    public:
    Message() {
    }

    Message(void* handler, T data) {
        this->_handler._handler = (void (*)(void *, T, void *)) handler;
        this->_data = data;
    }

    T data() {
        return this->_data;
    }
    
    HandlerPtr<T> handler() {
        return this->_handler;
    }
};

template <typename T>
class MessageQueue {
    moodycamel::ConcurrentQueue<Message<T>> queue;

    public:
    MessageQueue() {
        queue = moodycamel::ConcurrentQueue<Message<T>>();
    }

    void push(Message<T> message) {
        queue.enqueue(message);
    }

    bool pop(Message<T> &item) {
        return queue.try_dequeue(item);;
    }
};

enum ActorStatus {
    Waiting, Scheduled, Working, Dead
};

class Actor {
    void* _state;

    public:
    std::atomic<ActorStatus> status;

    explicit Actor(void* state) {
        this->_state = state;
        this->status = ActorStatus::Waiting;
    }
    void* state() {
        return _state;
    }
};

struct GlobalState { 
    BS::thread_pool pool((unsigned int) 1);
    std::atomic<int> live;

    void live_loop() {
        while (live.load() > 0) {
        }
    }
} global;



class MailBox {
    Actor* actor;
    MessageQueue<int64_t> int64_queue;
    MessageQueue<void*> ptr_queue;


    public:
    explicit MailBox(void* state) {
        global.live.fetch_add(1);
        this->actor = new Actor(state);
        this->int64_queue = MessageQueue<int64_t>();
        this->ptr_queue = MessageQueue<void*>();
    }

    void message_int(int64_t message, void* handler) {
        this->int64_queue.push(Message(handler, message));
        this->schedule();
    }

    void message_any(void* message, void* handler) {
        this->ptr_queue.push(Message(handler, message));
        this->schedule();
    }

    void execute() {        
        auto scheduled_status = ActorStatus::Scheduled;
        auto working_status = ActorStatus::Working;
        if (this->actor->status.compare_exchange_strong(scheduled_status, ActorStatus::Working)) {
            Message<int64_t> int_msg;
            while (this->int64_queue.pop(int_msg)) {
                if (!this->is_dead()) {
                    int_msg.handler()(this->actor->state(), int_msg.data(), this);
                }
            }
            Message<void*> ptr_msg;
            while (this->ptr_queue.pop(ptr_msg)) {
                if (!this->is_dead()) {
                    auto msg = this->ptr_queue.pop(ptr_msg);
                    ptr_msg.handler()(this->actor->state(), ptr_msg.data(), this);
                }
            }
            if (!this->is_dead()) {
                this->actor->status.compare_exchange_strong(working_status, ActorStatus::Waiting);
            }
        }
    }

    void schedule() {
        auto waiting_status = ActorStatus::Waiting;
        auto working_status = ActorStatus::Working;
        if (this->actor->status.compare_exchange_strong(working_status, ActorStatus::Scheduled)) {
            global.pool.detach_task([this] { this->execute(); });
        }
        if (this->actor->status.compare_exchange_strong(waiting_status, ActorStatus::Scheduled)) {
            global.pool.detach_task([this] { this->execute(); });
        }
    }

    void death() {
        delete this->actor;
        this->actor->status = Dead;
        global.live.fetch_sub(1);
    }

    bool is_dead() {
        return this->actor->status == Dead;
    }
};

extern "C" void* init_main_state(); 

extern "C" void init_main_interactor(void* state, void* message, void* mailbox);

void init_main() {
    void* state = init_main_state();
    MailBox* main_mailbox = new MailBox(state);
    main_mailbox->message_any(NULL, (void*) init_main_interactor);
}

int main(int argc, char** argv) {
    init_main();
    global.live_loop();
}