#include <time.h>
#include <unistd.h>
#include <sys/time.h>
#include <fcntl.h>
#include <limits.h>

#include <SDL2/SDL.h>

#define AMPLITUDE 28000
#define FREQUENCY 44100
#define PI 3.141592653589793

#define display_size 256
#define display_height 32
#define display_length 8

#define frequ 1200
#define timer_freq 60

#define mem_size 4096
#define program_start 0x200

#define num_keys 16

#define general_registers_size 16

#define stack_size 16

#define pixel_size 20

SDL_Window * window;

unsigned char mem[mem_size] = { 0 };
unsigned char display[display_size] = { 0 };
unsigned char keyboard_state[num_keys] = { 0 };

uint8_t general_registers[general_registers_size] = { 0 };
uint16_t I;
uint8_t timer;
uint8_t sound;
uint16_t pc;

uint16_t stack[stack_size] = { 0 };
uint8_t sp;

uint16_t program_end;

struct instruction {
	unsigned char code: 4;
	unsigned char v: 4;
	unsigned char w: 4;
	unsigned char tail: 4;
};

unsigned char sprites[80] =   { 0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
								0x20, 0x60, 0x20, 0x20, 0x70, // 1
								0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
								0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
								0x90, 0x90, 0xF0, 0x10, 0x10, // 4
								0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
								0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
								0xF0, 0x10, 0x20, 0x40, 0x40, // 7
								0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
								0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
								0xF0, 0x90, 0xF0, 0x90, 0x90, // A
								0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
								0xF0, 0x80, 0x80, 0x80, 0xF0, // C
								0xE0, 0x90, 0x90, 0x90, 0xE0, // D
								0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
								0xF0, 0x80, 0xF0, 0x80, 0x80  // F
								};

// Audio sample generation
void generateSamples(long * v, Sint16 *stream, int length) {
	// Using fourier square wave (somehow fitting for old computers)
	for (int i = 0; i < length; i++) {
		stream[i] = 0.5* AMPLITUDE * (4/PI) * (sin(*v * 2 * PI / FREQUENCY) + 0.33333*sin(*v * 6 * PI / FREQUENCY) + 0.2*sin(*v * 10 * PI / FREQUENCY));
		*v = (*v+392)%LONG_MAX;
	}
}

// Giving samples for the audio stream
void audio_callback(void *userdata, Uint8 *_stream, int _length)
{
    Sint16 *stream = (Sint16*) _stream;
    int length = _length / 2;
    long * v = (long*)userdata;

    generateSamples(v, stream, length);
}

// Set SDL surface pixel
void set_pixel(SDL_Surface * surface, int x, int y, uint32_t state) {
	y %= 32;
	x %= 64;
	int lx = display_length*8*pixel_size;
	uint32_t * screen = (uint32_t*)surface->pixels; 
	#pragma omp parallel for
	for (int i = 0; i < pixel_size; i++) {
		for (int j = 0; j < pixel_size; j++) {
			screen[(pixel_size*y+i)*lx + pixel_size*x+j] = state;
		}
	};
}

// Update all pixels
void show() {
	SDL_Surface * surface = SDL_GetWindowSurface(window);
	SDL_LockSurface(surface);
	#pragma omp parallel for
	for (int i = 0; i < display_height; i++) {
		for (int j = 0; j < display_length; j++) {
			unsigned char byte = display[i*display_length+j];
			for (int k = 0; k < 8; k++) {
				set_pixel(surface, j*8+k, i, byte&0x80 ? 0xFF00FF00 : 0);
				byte = byte << 1;

			}
		}
	}
	SDL_UnlockSurface(surface);
	SDL_UpdateWindowSurface(window);
	//SDL_Delay();

}

// Translate form SDL keycodes to chip-8 keycodes
uint8_t update_keyboard(int key, int state) {
	if (30 <= key && key <= 33) { 
		keyboard_state[key-30] = state;
		return (uint8_t) key-30;
	}
	unsigned char mch_key = -1;
	switch (key)
	{
		case 20:
			mch_key = 0x4;
			break;

		case 26:
			mch_key = 0x5;
			break;

		case 8:
			mch_key = 0x6;
			break;

		case 21:
			mch_key = 0x7;
			break;

		case 4:
			mch_key = 0x8;
			break;

		case 22:
			mch_key = 0x9;
			break;

		case 7:
			mch_key = 0xA;
			break;

		case 9:
			mch_key = 0xB;
			break;

		case 29:
			mch_key = 0xC;
			break;

		case 27:
			mch_key = 0xD;
			break;

		case 6:
			mch_key = 0xE;
			break;

		case 25:
			mch_key = 0xF;
			break;
	}
	if (mch_key >= 0) {
		keyboard_state[mch_key] = state;
		return (uint8_t) mch_key;
	}
	return 0;
}

// Block until the state of the keyboard changes
unsigned char get_key() {


	SDL_Event e;
	int c = 0;
	do {
		while (SDL_PollEvent(&e)) {
			if (e.type == SDL_QUIT) exit(EXIT_FAILURE);
			if (e.type == SDL_KEYDOWN || e.type == SDL_KEYUP)
				c = update_keyboard(e.key.keysym.scancode, (e.type == SDL_KEYDOWN));
		}
	} while( !c );

	return c;

}

// Instruction implementation:

void CLS() {
	memset(display, 0, display_size);
}

void RET() {
	pc = stack[sp--];
}

void JP(uint16_t addr) {
	pc = addr-2;
}

void CALL(uint16_t addr) {
	stack[++sp] = pc;
	pc = addr-2;
}

void SE_N(unsigned char v, unsigned char n) {
	pc += (general_registers[v] == n)*2;
}

void SNE_N(unsigned char v, unsigned char n) {
	pc += (general_registers[v] != n)*2;
}

void SE(unsigned char v, unsigned char w) {
	pc += (general_registers[v] == general_registers[w])*2;
}

void LDB(unsigned char v, unsigned char n) {
	general_registers[v] = n;
}

void ADDB(unsigned char v, unsigned char n) {
	general_registers[v] += n;
}

void LD(unsigned char v, unsigned char w) {
	general_registers[v] = general_registers[w];
}

void OR(unsigned char v, unsigned char w) {
	general_registers[v] = general_registers[v] | general_registers[w];
}

void AND(unsigned char v, unsigned char w) {
	general_registers[v] = general_registers[v] & general_registers[w];
}

void XOR(unsigned char v, unsigned char w) {
	general_registers[v] = general_registers[v] ^ general_registers[w];
}

void ADD(unsigned char v, unsigned char w) {
	unsigned int sum = general_registers[v] + general_registers[w];
	general_registers[15] = (sum > 255);
	general_registers[v] = sum & 0xFF;
}

void SUB(unsigned char v, unsigned char w) {
	general_registers[15] = (general_registers[v] > general_registers[w]);
	general_registers[v] -= general_registers[w];
}

void SHR(unsigned char v) {
	// Most significant bit: 0x80 = 10000000
	general_registers[15] = general_registers[v]&0x01 ? 1 : 0;
	general_registers[v] /= 2;
}

void SUBN(unsigned char v, unsigned char w) {
	general_registers[15] = (general_registers[w] > general_registers[v]);
	general_registers[v] = general_registers[w] - general_registers[v];
}

void SHL(unsigned char v) {
	general_registers[15] = general_registers[v]&0x80 ? 1 : 0;
	general_registers[v] *= 2;
}

void SNE(unsigned char v, unsigned char w) {
	pc += (general_registers[v] != general_registers[w])*2;
}

void LD_I(uint16_t addr) {
	I = addr;
}

void JP_VO(uint16_t addr) {
	pc = general_registers[0] + addr-2;
}

void RND(unsigned char v, unsigned char k) {
	general_registers[v] = ((unsigned char)rand()%256)&k;
}

void DRW(unsigned char v, unsigned char w, unsigned char n) {
	unsigned char x = general_registers[v]%(display_length*8);
	unsigned char y = general_registers[w]%display_height;
	general_registers[15] = 0;
	uint8_t used_bits = x%8;
	for (int i = 0; i < n; i++) {
		if (((mem[I+i] >> used_bits) & display[((y+i)*8+x/8)%256]) || ((mem[I+i] << 8-used_bits) & display[((y+i)*8+x/8+1)%256]) )
            general_registers[15] = 1;
		display[((y+i)*8+x/8)%256] = mem[I+i] >> used_bits ^ display[((y+i)*8+x/8)%256];
		display[((y+i)*8+(x/8)%display_length+1)%256] = mem[I+i] << 8-used_bits ^ display[((y+i)*8+(x/8)%display_length+1)%256];
	}
	show();
}

void SKP(unsigned char v) {
	pc += (keyboard_state[general_registers[v]])*2;
}

void SKNP(unsigned char v) {
	pc += (!keyboard_state[general_registers[v]])*2;
}

void LDT(unsigned char v) {
	general_registers[v] = timer;
}

void LDK(unsigned char v) {
	general_registers[v] = get_key();
}

void ILDT(unsigned char v) {
	timer = general_registers[v];
}

void LDS(unsigned char v) {
	if (!sound && general_registers[v])
		SDL_PauseAudio(0);
	sound = general_registers[v];
}

void ADDI(unsigned char v) {
	I += general_registers[v];
}

void LDF(unsigned char v) {
	I = 64 + 5*general_registers[v];
}

void LDM(unsigned char v) {
	mem[I] = general_registers[v]/100;
	mem[I+1] = (general_registers[v]-mem[I]*100)/10;
	mem[I+2] = general_registers[v]%10;
}

void LDIX(unsigned char v) {
	for (int i = 0; i <= v; i++)
		mem[I+i] = general_registers[i];
}

void LDXI(unsigned char v) {
	for (int i = 0; i <= v; ++i)
		general_registers[i] = mem[I+i];
}

// Parse instruction
struct instruction to_instruction(unsigned char * a) {
	struct instruction inst;
	inst.code = a[0] >> 4;
	inst.v = a[0] & 0x0F;
	inst.w = a[1] >> 4;
	inst.tail = a[1] & 0x0F;

	return inst;
}

// Parse instruction with three bytes as data
uint16_t get_n(struct instruction inst) {
	return inst.tail | inst.w << 4 | inst.v << 8;
}

// Parse instruction with two bytes as data
unsigned char get_k(struct instruction inst) {
	return inst.tail | inst.w << 4;
}

// Execute the apropiate routine for an instruction
void execute(struct instruction inst) {
	switch (inst.code)
	{
		case 0x0:
			if (inst.tail == 0x0) CLS();
			if (inst.tail == 0xE) RET();
			break;

		case 0x1:
			JP(get_n(inst));
			break;

		case 0x2:
			CALL(get_n(inst));
			break;

		case 0x3:
			SE_N(inst.v, get_k(inst));
			break;

		case 0x4:
			SNE_N(inst.v, get_k(inst));
			break;

		case 0x5:
			SE(inst.v, inst.w);
			break;

		case 0x6:
			LDB(inst.v, get_k(inst));
			break;

		case 0x7:
			ADDB(inst.v, get_k(inst));
			break;

		case 0x8:
			switch (inst.tail)
			{
				case 0x0:
					LD(inst.v, inst.w);
					break;

				case 0x1:
					OR(inst.v, inst.w);
				break;

				case 0x2:
					AND(inst.v, inst.w);
				break;

				case 0x3:
					XOR(inst.v, inst.w);
				break;

				case 0x4:
					ADD(inst.v, inst.w);
				break;

				case 0x5:
					SUB(inst.v, inst.w);
				break;

				case 0x6:
					SHR(inst.v);
				break;

				case 0x7:
					SUBN(inst.v, inst.w);
				break;

				case 0xE:
					SHL(inst.v);
				break;
			}
			break;

		case 0x9:
			SNE(inst.v, inst.w);
			break;

		case 0xA:
			LD_I(get_n(inst));
			break;

		case 0xB:
			JP_VO(get_n(inst));
			break;

		case 0xC:
			RND(inst.v, get_k(inst));
			break;

		case 0xD:
			DRW(inst.v, inst.w, inst.tail);
			break;

		case 0xE:
			if (inst.tail == 0xE)
				SKP(inst.v);
			if (inst.tail == 0x1)
				SKNP(inst.v);
			break;

		case 0xF:
			switch (inst.tail)
			{
				case 0x7:
					LDT(inst.v);
					break;

				case 0xA:
					LDK(inst.v);
					break;

				case 0x5:
					if (inst.w == 0x1)
						ILDT(inst.v);
					else if (inst.w == 0x5)
						LDIX(inst.v);
					else
						LDXI(inst.v);
				break;
				
				case 0x8:
					LDS(inst.v);
					break;
				
				case 0xE:
					ADDI(inst.v);
				break;
				
				case 0x9:
					LDF(inst.v);
				break;
				
				case 0x3:
					LDM(inst.v);
					break;

			}
			break;
	}
}

// Loads a chip-8 program from a filename
void load_program(const char * file_name) {
	program_end = 0;
	pc = program_start;
	sp = 0;
	int file;
	if ((file = open(file_name, O_RDONLY)) < 0) {
		exit(EXIT_FAILURE);
	}
	size_t size = lseek(file, 0, SEEK_END);
	lseek(file, 0, SEEK_SET);
	unsigned char * buff = malloc(size);
	read(file, buff, size);
	program_end = (size+0x200)-1;
	memcpy(mem+program_start, buff, size);
}

// main loop
void emulate(const char * file_name) {
	struct timeval start, end, ts;
	unsigned long temp_counter;
	load_program(file_name);
	SDL_Event e;
	int quit = 0;

	while( !quit ) {
		gettimeofday(&start, NULL);

		while( SDL_PollEvent( &e ) > 0 ) {
			if( e.type == SDL_QUIT ) quit = 1;

			if ( e.type == SDL_KEYDOWN ) update_keyboard(e.key.keysym.scancode, 1);
			if ( e.type == SDL_KEYUP ) update_keyboard(e.key.keysym.scancode, 0);
		}

		execute(to_instruction(mem+pc));
		pc += 2;
		quit = quit || (pc < 0x200 || pc > program_end);
		gettimeofday(&end, NULL);
		unsigned long delta_ns = (end.tv_sec - start.tv_sec) * 1000000 + end.tv_usec - start.tv_usec;
		if (delta_ns < (unsigned long)1000000/frequ) {
			temp_counter += 1000000/frequ;
			unsigned long remaining = (unsigned long)1000000/frequ - delta_ns;
			usleep(remaining);
		} else 
			temp_counter += delta_ns;
		while (temp_counter > 1000000/timer_freq) {
			temp_counter -= (1000000/timer_freq);
			if (timer) timer--;
			if (sound) {
				sound--;
				if ( !sound )
					SDL_PauseAudio(1);
			}
		}
	}
}

int main (int argc, unsigned char * argv[]) {
	time_t t;
	srand((unsigned) time(&t));

	if (argc < 2) {
		printf("The rom to be emulated has to be given as a parameter\n");
		exit(EXIT_FAILURE);
	}
	for (int i = 0; i < 80; i++) mem[64+i] = sprites[i];
	
	// Initialize sdl video
	if( SDL_Init( SDL_INIT_VIDEO ) < 0){
         fprintf( stderr, "Could not initialise SDL: %s\n", SDL_GetError() );
         exit( -1 );
     }
    window =  SDL_CreateWindow(
    	"bingle", 0, 0, display_length*8*pixel_size, display_height*pixel_size, 0);

    // Initialize sdl audio
    SDL_Init(SDL_INIT_AUDIO);
	SDL_AudioSpec desiredSpec;

	long wavetime = 0;

    desiredSpec.freq = FREQUENCY;
    desiredSpec.format = AUDIO_S16SYS;
    desiredSpec.channels = 1;
    desiredSpec.samples = 2048;
    desiredSpec.callback = audio_callback;
    desiredSpec.userdata = &wavetime;

    SDL_AudioSpec obtainedSpec;

    // you might want to look for errors here
    SDL_OpenAudio(&desiredSpec, &obtainedSpec);

    emulate(argv[1]);
    exit(EXIT_SUCCESS);
}

