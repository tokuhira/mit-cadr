//
//  main.c
//  EtherHelper
//
//  Created by Greg Gilley on 12/16/12.
//  Copyright (c) 2012 Greg Gilley. All rights reserved.
//

#include <stdio.h>
#include <errno.h>
#include <syslog.h>
#include <xpc/xpc.h>
#include <sys/socket.h>
#include <net/if.h>
#include <net/bpf.h>
#include <fcntl.h>
#include <sys/ioctl.h>

int tap_fd = -1;
const char* interface = "en0";
struct ifreq bound_if;
int buf_len = 1;

static void __XPC_Peer_Event_Handler(xpc_connection_t connection, xpc_object_t event) {
    syslog(LOG_NOTICE, "Received event in helper.");
    
	xpc_type_t type = xpc_get_type(event);
    
	if (type == XPC_TYPE_ERROR) {
        syslog(LOG_NOTICE, "XPC_Peer_Event_Handler error\n");

		if (event == XPC_ERROR_CONNECTION_INVALID) {
			// The client process on the other end of the connection has either
			// crashed or cancelled the connection. After receiving this error,
			// the connection is in an invalid state, and you do not need to
			// call xpc_connection_cancel(). Just tear down any associated state
			// here.
            
		} else if (event == XPC_ERROR_TERMINATION_IMMINENT) {
			// Handle per-connection termination cleanup.
		}
        
	}
    else if (type == XPC_TYPE_DICTIONARY)
    {

        const void *data;
        size_t len;
        const char *op;

        xpc_connection_t remote = xpc_dictionary_get_remote_connection(event);
        
        op = xpc_dictionary_get_string(event, "op");
        
        if (op && strcmp(op, "write") == 0)
        {
            syslog(LOG_NOTICE, "write\n");
            data = xpc_dictionary_get_data(event, "packet", &len);
            if (data)
            {
                if (write(tap_fd, data, len) != len)
                {
                    syslog(LOG_NOTICE, "error on write\n");
                }
            }
            else
                syslog(LOG_NOTICE, "no packet\n");

            xpc_object_t reply = xpc_dictionary_create_reply(event);
            xpc_dictionary_set_string(reply, "reply", "write!");
            xpc_connection_send_message(remote, reply);
            xpc_release(reply);
        }
        else if (op && strcmp(op, "read") == 0)
        {
            size_t len;
            uint32_t packet[375];

            
            len = read(tap_fd, packet, sizeof(packet));
            
            syslog(LOG_NOTICE, "read: %zd\n", len);

            xpc_object_t reply = xpc_dictionary_create_reply(event);
            xpc_dictionary_set_string(reply, "reply", "read!");
            if (len > 0)
                xpc_dictionary_set_data(event, "packet", packet, len);
            xpc_connection_send_message(remote, reply);
            xpc_release(reply);
        }
        else
        {
            syslog(LOG_NOTICE, "unknown op='%s'\n", op ? op : "(NULL)");
        }
    }
    else
    {
        syslog(LOG_NOTICE, "unknown type = %p\n", type);
    }
}

static void __XPC_Connection_Handler(xpc_connection_t connection)  {
    syslog(LOG_NOTICE, "Configuring message event handler for helper.");
    
	xpc_connection_set_event_handler(connection, ^(xpc_object_t event) {
		__XPC_Peer_Event_Handler(connection, event);
	});
	
	xpc_connection_resume(connection);
}

int main(int argc, const char *argv[]) {
    syslog(LOG_NOTICE, "Before creating mach service");
    xpc_connection_t service = xpc_connection_create_mach_service("com.cadr.EtherHelper",
                                                                  dispatch_get_main_queue(),
                                                                  XPC_CONNECTION_MACH_SERVICE_LISTENER);
    
    if (!service) {
        syslog(LOG_NOTICE, "Failed to create service.");
        exit(EXIT_FAILURE);
    }
    
    if (tap_fd == -1)
    {
        syslog(LOG_NOTICE, "before open bpf\n");

        if ((tap_fd = open("/dev/bpf3", O_RDWR)) < 0) {
            syslog(LOG_NOTICE, "Couldn't open /dev/bfp3: %s\n", strerror(errno));
            exit(EXIT_FAILURE);
        }
        
        strcpy(bound_if.ifr_name, interface);
        if (ioctl(tap_fd, BIOCSETIF, &bound_if) > 0)
        {
            syslog(LOG_NOTICE, "Can't bind to /dev/bfp3\n");
            exit(EXIT_FAILURE);
        }
        
        // activate immediate mode (therefore, buf_len is initially set to "1")
        if (ioctl(tap_fd, BIOCIMMEDIATE, &buf_len) == -1) {
            syslog(LOG_NOTICE, "Can't activate immediate mode\n");
            exit(EXIT_FAILURE);
        }
    }
    
    // request buffer length
    if (ioctl(tap_fd, BIOCGBLEN, &buf_len) == -1) {
        syslog(LOG_NOTICE, "Can't get buffer length\n");
        //  exit(EXIT_FAILURE);
    }
    syslog(LOG_NOTICE, "buffer length=%d\n", buf_len);

    syslog(LOG_NOTICE, "Configuring connection event handler for helper");
    xpc_connection_set_event_handler(service, ^(xpc_object_t connection) {
        __XPC_Connection_Handler(connection);
    });
    
    xpc_connection_resume(service);
    
    dispatch_main();
    
    xpc_release(service);
    
    return EXIT_SUCCESS;
}

