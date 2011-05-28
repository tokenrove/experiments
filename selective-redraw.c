
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <GL/gl.h>
#include <SDL/SDL.h>

static SDL_Surface *display = NULL;
static int current_buffer = 0;
enum { NBUFFERS = 2, MAX_DIRT = 32 };
static SDL_Rect dirt[NBUFFERS][MAX_DIRT];
static int dirt_count[NBUFFERS];

typedef struct { float red, green, blue, alpha; } color_t;


void swap_buffers(void)
{
	SDL_GL_SwapBuffers();
	current_buffer = (current_buffer+1)%NBUFFERS;
}

int create_window(int w, int h, int fullscreen_p)
{
	unsigned int flags = SDL_OPENGL|(fullscreen_p*SDL_FULLSCREEN);

	SDL_Init(SDL_INIT_EVERYTHING);
	SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
	SDL_GL_SetAttribute(SDL_GL_SWAP_CONTROL, 1);
	if(!SDL_VideoModeOK(w, h, 32, flags))
		return 0;
	display = SDL_SetVideoMode(w, h, 32, flags);
	if(display == NULL) return 0;
	atexit(SDL_Quit);
	return 1;
}

void setup_flat_opengl(void)
{
	assert(display != NULL);
	glViewport(-1, -1, (1+display->w), (1+display->h));
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glOrtho(0, display->w, 0, display->h, -1, 1);
	glTranslatef(0.375, 0.375, 0); /* Redbook Appendix G */
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	glClearColor(0.2, 0.2, 0.2, 1.0);
}

void reset_pixel_store(void)
{
	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
	glPixelStorei(GL_PACK_ALIGNMENT, 1);
	glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
	glPixelStorei(GL_UNPACK_SKIP_PIXELS, 0);
}


void setup_gl_for_sprites(void)
{
	glDepthFunc(GL_LEQUAL);
	glAlphaFunc(GL_GREATER, 0.1);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glShadeModel(GL_FLAT);
	reset_pixel_store();
}

void draw_rectangle(color_t color, SDL_Rect r)
{
	glBegin(GL_QUADS);
	glColor4fv((float*)&color);
	glVertex2i(r.x,r.y);
	glVertex2i(r.x+r.w,r.y);
	glVertex2i(r.x+r.w,r.y+r.h);
	glVertex2i(r.x,r.y+r.h);
	glEnd();
}


void dirt_init()
{
	memset(dirt, 0, sizeof(dirt));
	memset(dirt_count, 0, sizeof(dirt_count));
}

void dirt_redraw(SDL_Rect r)
{
	draw_rectangle((color_t){0.,0.,0.,1.}, r);
}

void dirt_smirch(SDL_Rect r)
{
	for(int i = 0; i < NBUFFERS; i++) {
		dirt_count[i]++;
		/* If the amount of dirt exceeds our buffer size,
		 * replace it with a full redraw. */
		if(dirt_count[i] >= MAX_DIRT) {
			dirt_count[i] = 1;
			r = (SDL_Rect){0,0,display->w,display->h};
		}
		dirt[i][dirt_count[i]-1] = r;
	}
}

void dirt_sweep()
{
	assert(dirt_count[current_buffer] >= 0);
	for(int i = 0; i < dirt_count[current_buffer]; i++)
		dirt_redraw(dirt[current_buffer][i]);
	dirt_count[current_buffer] = 0;
}

void draw_cursor(int x, int y)
{
	draw_rectangle((color_t){0.8,0.1,0.3,1.0}, (SDL_Rect){x, y, 16, 16});
}



void event_loop(void)
{
	SDL_Event event;
	int x,y;

	x = y = 0;
	while(1) {
		dirt_sweep();
		draw_cursor(x, y);
		swap_buffers();

		SDL_WaitEvent(NULL);
		while(SDL_PollEvent(&event)) {
			switch(event.type) {
			case SDL_MOUSEBUTTONDOWN:
				return;
			case SDL_MOUSEMOTION:
				if((x == event.motion.x) &&
				   (y == display->h-event.motion.y))
					break;
				dirt_smirch((SDL_Rect){x, y, 16, 16});
				x = event.motion.x;
				y = display->h-event.motion.y;
				break;
			}
		}
	}
}

int main(void)
{
	if(!create_window(640, 480, 1))
		exit(1);
	setup_flat_opengl();
	setup_gl_for_sprites();
	SDL_ShowCursor(0);

	glClear(GL_COLOR_BUFFER_BIT);
	swap_buffers();
	glClear(GL_COLOR_BUFFER_BIT);
	dirt_init();

	event_loop();
	return 0;
}

