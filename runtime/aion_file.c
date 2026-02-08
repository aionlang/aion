#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <stdbool.h>

bool aion_path_exists(const char* path) {
    struct stat st;
    return (stat(path, &st) == 0);
}

bool aion_is_file(const char* path) {
    struct stat st;
    if (stat(path, &st) != 0) {
        return false;
    }
    return S_ISREG(st.st_mode);
}   

bool aion_is_dir(const char* path) {
    struct stat st;
    if (stat(path, &st) != 0) {
        return false;
    }
    return S_ISDIR(st.st_mode);
}

bool aion_remove(const char* path) {
    return (remove(path) == 0);
}

bool aion_mkdir(const char* path) {
#ifdef _WIN32
    return (mkdir(path) == 0);
#else
    return (mkdir(path, 0755) == 0);
#endif
}