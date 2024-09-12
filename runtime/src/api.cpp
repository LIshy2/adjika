#include "actors.hpp"


extern "C" void send_int(int message, void*handler, MailBox* mailbox) {
    mailbox->message_int(message, handler);
}

extern "C" void send_ptr(void* message, void*handler, MailBox* mailbox) {
    mailbox->message_any(message, handler);
}

extern "C" MailBox* spawn_actor(void *state) {
    return new MailBox(state);   
}