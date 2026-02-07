/*
 * __chkstk for x86_64 Windows (MinGW compatibility)
 *
 * LLVM targeting windows-msvc emits calls to __chkstk whenever a
 * function's stack frame exceeds one page (4 KB).  The MSVC CRT
 * provides this symbol, but MinGW gcc does not.
 *
 * Input:  RAX = number of bytes the prologue wants to allocate
 * Effect: probes each 4 KB page between the current RSP and
 *         (RSP - RAX) so that the Windows guard page mechanism
 *         can grow the committed stack region.
 * Output: RAX is preserved.
 */

#if defined(_WIN64) || (defined(_WIN32) && defined(__x86_64__))

__attribute__((naked))
void __chkstk(void) {
    __asm__ (
        "push   %rcx            \n"
        "push   %rax            \n"
        "cmp    $0x1000, %rax   \n"
        "lea    0x18(%rsp), %rcx\n"   /* rcx = original RSP */
        "jb     2f              \n"
        "1:                     \n"
        "sub    $0x1000, %rcx   \n"
        "testb  $0, (%rcx)      \n"   /* probe this page */
        "sub    $0x1000, %rax   \n"
        "cmp    $0x1000, %rax   \n"
        "ja     1b              \n"
        "2:                     \n"
        "sub    %rax, %rcx      \n"
        "testb  $0, (%rcx)      \n"   /* probe last partial page */
        "pop    %rax            \n"
        "pop    %rcx            \n"
        "ret                    \n"
    );
}

#endif
