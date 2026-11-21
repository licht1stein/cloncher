#ifndef CLONCHER_LAUNCHER_LAYER_H
#define CLONCHER_LAUNCHER_LAYER_H

#include <SDL3/SDL.h>

SDL_Window *cloncher_create_window(const char *, int, int, SDL_WindowFlags);
void cloncher_destroy_window(SDL_Window *);
bool cloncher_set_window_size(SDL_Window *, int, int);
void cloncher_present_timer(const void *, int, int, int);

/* Applied only while compiling CUI's SDL backend. */
#define SDL_CreateWindow cloncher_create_window
#define SDL_DestroyWindow cloncher_destroy_window
#define SDL_SetWindowSize cloncher_set_window_size
#endif
