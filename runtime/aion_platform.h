/*
 * Small differences between operating systems that several C files of the standard library need to hide.
 * Included by those files; everything here is static, so a file pays only for what it uses.
 */

#ifndef AION_PLATFORM_H
#define AION_PLATFORM_H

#ifdef _WIN32
#include <windows.h>
#else
#include <dlfcn.h>
#endif

/*
 * Load the first library of `names` (NULL ends the list) that can be loaded, and look symbols up in it.  The
 * libraries the standard library binds are found when first used, so a program that never uses them needs
 * nothing installed.  Names are for the system's loader: "libssl.so.3" on Linux, "libssl-3-x64.dll" on Windows.
 */
static inline void *aion_dlopen(const char *const *names) {
    for (; *names; names++) {
#ifdef _WIN32
        void *lib = (void *)LoadLibraryA(*names);
#else
        void *lib = dlopen(*names, RTLD_NOW | RTLD_GLOBAL);
#endif
        if (lib) return lib;
    }
    return NULL;
}

static inline void *aion_dlsym(void *lib, const char *symbol) {
#ifdef _WIN32
    return (void *)GetProcAddress((HMODULE)lib, symbol);
#else
    return dlsym(lib, symbol);
#endif
}

#endif
