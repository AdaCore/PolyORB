#include <iostream>
#include <thread>
#include <chrono>
#include <csignal>
#include <atomic>

// Widget Core Service - Minimal Implementation
// This is a placeholder service demonstrating the infrastructure

std::atomic<bool> running{true};

void signal_handler(int signal) {
    std::cout << "Received signal " << signal << ", shutting down gracefully..." << std::endl;
    running = false;
}

int main(int argc, char** argv) {
    // Set up signal handlers
    std::signal(SIGINT, signal_handler);
    std::signal(SIGTERM, signal_handler);

    // Parse command line arguments (basic)
    int port = 50051;
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
    std::cout << "Widget Core Service v1.0.0" << std::endl;
    std::cout << "=====================================" << std::endl;
    std::cout << "Port: " << port << std::endl;
    std::cout << "Workers: " << workers << std::endl;
    std::cout << "Status: RUNNING" << std::endl;
    std::cout << "=====================================" << std::endl;

    // Simulate service running
    int heartbeat_count = 0;
    while (running) {
        std::this_thread::sleep_for(std::chrono::seconds(5));
        heartbeat_count++;
        std::cout << "[" << heartbeat_count << "] Service heartbeat - healthy" << std::endl;
    }

    std::cout << "Widget Core Service shutting down..." << std::endl;
    return 0;
}
