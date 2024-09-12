#include "third_party/concurrentqueue.hpp"
#include "third_party/BS_thread_pool.hpp"
#include <atomic>
#include <iostream>



template <typename T>
struct handler_ptr {
    void (*_handler)(void*, T, void*);

    void operator()(void* state, T message, void* mailbox) {
        this->_handler(state, message, mailbox);
    }
};

template <typename T>
class Message {
    handler_ptr<T> _handler;
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
    
    handler_ptr<T> handler() {
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
    Waiting, Scheduled, Working
};

class Actor {
    void* _state;

    public:
    std::atomic<ActorStatus> status;

    Actor(void* state) {
        this->_state = state;
        this->status = ActorStatus::Waiting;
    }
    void* state() {
        return _state;
    }
};


BS::thread_pool pool;

class MailBox {
    Actor* actor;
    MessageQueue<int> int_queue;
    MessageQueue<void*> ptr_queue;


    public:
    MailBox(void* state) {
        std::cout << "new mailbox init " << this << std::endl;
        this->actor = new Actor(state);
        this->int_queue = MessageQueue<int>();
        this->ptr_queue = MessageQueue<void*>();
    }

    void message_int(int message, void* handler) {
        std::cout << "new_message to " << this << std::endl;
        std::cout << "thread id " << std::this_thread::get_id()  << std::endl;
        this->int_queue.push(Message(handler, message));
        this->schedule();
    }

    void message_any(void* message, void* handler) {
        std::cout << "new_message to " << this << std::endl;
        std::cout << "thread id " << std::this_thread::get_id()  << std::endl;
        this->ptr_queue.push(Message(handler, message));
        std::cout << "schedule" << std::endl;
        this->schedule();
    }

    void execute() {
        
        std::cout << "execute method" << std::endl; 
        auto scheduled_status = ActorStatus::Scheduled;
        auto working_status = ActorStatus::Working;
        if (this->actor->status.compare_exchange_strong(scheduled_status, ActorStatus::Working)) {
            std::cout << "dequing" << std::endl; 
            Message<int> int_msg;
            while (this->int_queue.pop(int_msg)) {
                std::cout << "int_handler" << std::endl; 
                int_msg.handler()(this->actor->state(), int_msg.data(), this);
            }
            Message<void*> ptr_msg;
            while (this->ptr_queue.pop(ptr_msg)) {
                auto msg = this->ptr_queue.pop(ptr_msg);
                std::cout << "ptr_handler" << std::endl; 
                ptr_msg.handler()(this->actor->state(), ptr_msg.data(), this);
            }
            std::cout << "finished" << std::endl; 
            this->actor->status.compare_exchange_strong(working_status, ActorStatus::Waiting);
        }
    }

    void schedule() {
        auto waiting_status = ActorStatus::Waiting;
        auto working_status = ActorStatus::Working;
        if (this->actor->status.compare_exchange_strong(working_status, ActorStatus::Scheduled)) {
            std::cout << "new task" << std::endl;
            pool.detach_task([this] { std::cout << "execution" << std::endl; this->execute(); });
        }
        if (this->actor->status.compare_exchange_strong(waiting_status, ActorStatus::Scheduled)) {
            std::cout << "new task" << std::endl;
            pool.detach_task([this] { std::cout << "execution" << std::endl; this->execute(); });
        }
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
    while (true) {

    }
}