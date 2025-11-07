#include <iostream>
#include <string>
#include <thread>
#include <atomic>
#include <csignal>
#include <cstring>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>

// XRC Service - Minimal HTTP Implementation
// This is a placeholder service demonstrating the infrastructure

std::atomic<bool> running{true};

void signal_handler(int signal) {
    std::cout << "Received signal " << signal << ", shutting down gracefully..." << std::endl;
    running = false;
}

std::string generate_response(const std::string& path) {
    std::string body;

    if (path == "/" || path == "/health") {
        body = R"({
    "service": "xrc-service",
    "version": "1.0.0",
    "status": "healthy",
    "endpoints": [
        "/health",
        "/status",
        "/metrics"
    ]
})";
    } else if (path == "/status") {
        body = R"({
    "service": "xrc-service",
    "uptime_seconds": 0,
    "requests_served": 0,
    "status": "operational"
})";
    } else if (path == "/metrics") {
        body = R"({
    "http_requests_total": 0,
    "http_request_duration_ms": 0,
    "memory_usage_mb": 0
})";
    } else {
        body = R"({"error": "Not Found", "path": ")" + path + R"("})";
    }

    std::string response = "HTTP/1.1 200 OK\r\n";
    response += "Content-Type: application/json\r\n";
    response += "Content-Length: " + std::to_string(body.length()) + "\r\n";
    response += "Connection: close\r\n";
    response += "\r\n";
    response += body;

    return response;
}

void handle_client(int client_socket) {
    char buffer[4096] = {0};
    ssize_t bytes_read = read(client_socket, buffer, sizeof(buffer) - 1);

    if (bytes_read > 0) {
        std::string request(buffer);

        // Parse request path
        size_t path_start = request.find(' ') + 1;
        size_t path_end = request.find(' ', path_start);
        std::string path = request.substr(path_start, path_end - path_start);

        std::cout << "Request: " << path << std::endl;

        std::string response = generate_response(path);
        write(client_socket, response.c_str(), response.length());
    }

    close(client_socket);
}

int main(int argc, char** argv) {
    // Set up signal handlers
    std::signal(SIGINT, signal_handler);
    std::signal(SIGTERM, signal_handler);

    // Parse command line arguments
    int port = 8080;
    int workers = 4;

    for (int i = 1; i < argc; ++i) {
        std::string arg = argv[i];
        if (arg.find("--port=") == 0) {
            port = std::stoi(arg.substr(7));
        } else if (arg.find("--workers=") == 0) {
            workers = std::stoi(arg.substr(10));
        }
    }

    std::cout << "=====================================" << std::endl;
    std::cout << "XRC Service v1.0.0" << std::endl;
    std::cout << "=====================================" << std::endl;
    std::cout << "Port: " << port << std::endl;
    std::cout << "Workers: " << workers << std::endl;
    std::cout << "Status: RUNNING" << std::endl;
    std::cout << "=====================================" << std::endl;

    // Create socket
    int server_socket = socket(AF_INET, SOCK_STREAM, 0);
    if (server_socket < 0) {
        std::cerr << "Failed to create socket" << std::endl;
        return 1;
    }

    // Set socket options
    int opt = 1;
    if (setsockopt(server_socket, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt)) < 0) {
        std::cerr << "Failed to set socket options" << std::endl;
        close(server_socket);
        return 1;
    }

    // Bind socket
    struct sockaddr_in address;
    address.sin_family = AF_INET;
    address.sin_addr.s_addr = INADDR_ANY;
    address.sin_port = htons(port);

    if (bind(server_socket, (struct sockaddr*)&address, sizeof(address)) < 0) {
        std::cerr << "Failed to bind socket to port " << port << std::endl;
        close(server_socket);
        return 1;
    }

    // Listen
    if (listen(server_socket, 10) < 0) {
        std::cerr << "Failed to listen on socket" << std::endl;
        close(server_socket);
        return 1;
    }

    std::cout << "HTTP server listening on port " << port << std::endl;

    // Accept connections
    int request_count = 0;
    while (running) {
        fd_set read_fds;
        FD_ZERO(&read_fds);
        FD_SET(server_socket, &read_fds);

        struct timeval timeout;
        timeout.tv_sec = 1;
        timeout.tv_usec = 0;

        int activity = select(server_socket + 1, &read_fds, nullptr, nullptr, &timeout);

        if (activity > 0 && FD_ISSET(server_socket, &read_fds)) {
            int client_socket = accept(server_socket, nullptr, nullptr);
            if (client_socket >= 0) {
                request_count++;
                std::cout << "[" << request_count << "] Handling request..." << std::endl;

                // Handle in separate thread for simplicity
                std::thread client_thread(handle_client, client_socket);
                client_thread.detach();
            }
        }
    }

    std::cout << "XRC Service shutting down..." << std::endl;
    close(server_socket);
    return 0;
}
