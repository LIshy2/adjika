#include "actors.hpp"
#include "memory.hpp"


extern "C" void send_int(int64_t message, void*handler, MailBox* mailbox) {
    mailbox->message_int(message, handler);
}

extern "C" void send_ptr(void* message, void*handler, MailBox* mailbox) {
    mailbox->message_any(message, handler);
}

extern "C" MailBox* spawn_actor(void *state) {
    return new MailBox(state);   
}

extern "C" void* allocate(void *layout) {
    Allocation alloc((Layout*) layout); 
    return alloc;
}

extern "C" void* dup(void *mem) {
    Allocation alloc = mem;
    return alloc.dup();   
}

extern "C" void dedup(void *mem) {
    Allocation alloc = mem;
    alloc.dedup();
}