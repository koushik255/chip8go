package main

import (
	// Remove the "chip8" import
	"fmt"
	"math/rand"
	"time"
)

type Chip8 struct {
	// VF is also used as a flag register for
	V [16]byte

	Memory [4096]byte

	I  uint16
	PC uint16

	Display [64 * 32]bool

	DelayTimer byte
	SoundTimer byte

	// CHIP-8 allows for up to 16 nested subroutine calls
	Stack [16]uint16
	SP    byte // Stack pointer - points to the top of the stack

	// Keypad: Represents the 16-key hexadecimal keypad (0-F)
	// true = key pressed, false = key not pressed
	Keys [16]bool

	// Random number generator for the CXNN instruction
	rng *rand.Rand
}

// NewChip8 creates and initializes a new CHIP-8 emulator
func NewChip8() *Chip8 {
	chip8 := &Chip8{
		PC: 0x200,
		SP: 0,
		// Initialize the random number generator with a time-based seed
		// This ensures different random sequences each time the emulator runs
		rng: rand.New(rand.NewSource(time.Now().UnixNano())),
	}

	// Load the built-in font set into memory
	// These font sprites allow programs to display hexadecimal digits
	chip8.loadFontset()

	return chip8
}

// ████ (0xF0)
// █  █ (0x90)
// █  █ (0x90)
// █  █ (0x90)
// ████ (0xF0)
var fontset = [80]byte{
	0xF0, 0x90, 0x90, 0x90, 0xF0, // 0: ████, █  █, █  █, █  █, ████
	0x20, 0x60, 0x20, 0x20, 0x70, // 1:   █ ,  ██ ,   █ ,   █ ,  ███
	0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2: ████,    █, ████, █   , ████
	0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3: ████,    █, ████,    █, ████
	0x90, 0x90, 0xF0, 0x10, 0x10, // 4: █  █, █  █, ████,    █,    █
	0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5: ████, █   , ████,    █, ████
	0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6: ████, █   , ████, █  █, ████
	0xF0, 0x10, 0x20, 0x40, 0x40, // 7: ████,    █,   █ ,  █  ,  █
	0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8: ████, █  █, ████, █  █, ████
	0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9: ████, █  █, ████,    █, ████
	0xF0, 0x90, 0xF0, 0x90, 0x90, // A: ████, █  █, ████, █  █, █  █
	0xE0, 0x90, 0xE0, 0x90, 0xE0, // B: ███ , █  █, ███ , █  █, ███
	0xF0, 0x80, 0x80, 0x80, 0xF0, // C: ████, █   , █   , █   , ████
	0xE0, 0x90, 0x90, 0x90, 0xE0, // D: ███ , █  █, █  █, █  █, ███
	0xF0, 0x80, 0xF0, 0x80, 0xF0, // E: ████, █   , ████, █   , ████
	0xF0, 0x80, 0xF0, 0x80, 0x80, // F: ████, █   , ████, █   , █
	// this is Sprites for hexadecimal digits 0-F
}

// loadFontset loads the fontset into memory
// The font data is stored at the beginning of the memory (0x000-0x050)
// This allows programs to reference the font characters using their indices
func (c *Chip8) loadFontset() {
	for i := 0; i < len(fontset); i++ {
		c.Memory[i] = fontset[i]
	}
	// this copies the fontset data into the CHIP-8 memoroy starting at 0x000
	// is go c.Memory is the first byte of the memory
	// so therefore c.memeory 1 would be the second byte of the memory
	//so the loop c.memory[i] = fontset[i] is copying the fontset data into the CHIP-8 memory
	// starting at 0x000
}

// LoadROM loads a ROM file into memory starting at address 0x200
// This simulates loading a program into the CHIP-8 system

func (c *Chip8) LoadROM(romData []byte) error {
	// Check if ROM fits in memory (most CHIP-8 programs start at 0x200)
	// This ensures we don't attempt to load a ROM that's too large
	if len(romData) > 4096-0x200 {
		return fmt.Errorf("ROM too large: %d bytes (max: %d)", len(romData), 4096-0x200)
	}

	// Copy ROM data to memory starting at 0x200
	// This is where the program code will be located and executed from
	for i := 0; i < len(romData); i++ {
		c.Memory[0x200+i] = romData[i]
	}

	return nil
}

// Cycle executes one complete CPU cycle, which includes:
// 1. Fetching the next opcode from memory
// 2. Incrementing the program counter
// 3. Decoding and executing the opcode
// 4. Updating system timers
// This function is called repeatedly in the main emulation loop
func (c *Chip8) Cycle() {
	// Fetch opcode (2 bytes)
	// CHIP-8 instructions are 2 bytes long, stored big-endian
	// We read two consecutive bytes and combine them into one 16-bit value
	opcode := uint16(c.Memory[c.PC])<<8 | uint16(c.Memory[c.PC+1])

	// Increment PC before execution
	// The PC points to the next instruction to execute
	// We increment by 2 since each instruction is 2 bytes
	c.PC += 2

	// Decode and execute opcode
	c.executeOpcode(opcode)

	// Update timers
	// Both timers count down at 60Hz when they're non-zero
	if c.DelayTimer > 0 {
		c.DelayTimer--
	}
	if c.SoundTimer > 0 {
		c.SoundTimer--
		// Typically, the sound timer would trigger a beep while non-zero
		// The actual sound generation is typically handled by the frontend
	}
}

// executeOpcode decodes and executes a CHIP-8 opcode.
// This function is the heart of the emulator, acting as the CPU's instruction interpreter.
// It takes a 16-bit opcode and performs the action defined by that opcode.

func (c *Chip8) executeOpcode(opcode uint16) {

	// CHIP-8 opcodes are 16 bits (2 bytes) long. We need to break them down
	// - X: A 4-bit value, often representing a register index (V0-VF). From bits 8-11.
	// - Y: A 4-bit value, often representing another register index (V0-VF). From bits 4-7.
	// - N: A 4-bit value (a nibble). From bits 0-3.
	// - NN: An 8-bit value (a byte). From bits 0-7.
	// - NNN: A 12-bit value, often an address. From bits 0-11.

	x := (opcode & 0x0F00) >> 8
	// value of the second nibble of the opcode.
	//
	// 1. opcode & 0x0F00:
	//    - 0x0F00 in binary is 0000 1111 0000 0000. This is a "mask".
	//    - The & (bitwise AND) operation keeps only the bits that are set to 1 in BOTH the opcode and the mask.
	//      Opcode: 0001 1010 0010 0011 (0x1A23)
	//      Mask:   0000 1111 0000 0000 (0x0F00)
	//      Result: 0000 1010 0000 0000 (0x0A00)
	// 2. >> 8:
	//    - This "right shifts" the bits of the result (0x0A00) by 8 positions.
	//      0000 1010 0000 0000  becomes  0000 0000 0000 1010
	//    - This effectively divides the number by 2^8 (256), moving the 'A' nibble to the lowest position.
	//      So, 0x0A00 becomes 0x000A (or just 0xA).
	// For opcode 0x1A23, x would be 0xA (decimal 10).

	y := (opcode & 0x00F0) >> 4
	// Purpose: To get the value of the third nibble of the opcode.
	//
	//    - 0x00F0 in binary is 0000 0000 1111 0000.
	//      Opcode: 0001 1010 0010 0011 (0x1A23)
	//      Mask:   0000 0000 1111 0000 (0x00F0)
	//      Result: 0000 0000 0010 0000 (0x0020)
	//    - This isolates the third nibble ('2' in 0x1A23).
	// 2. >> 4:
	//    - Right shifts the bits of 0x0020 by 4 positions.
	//      0000 0000 0010 0000  becomes  0000 0000 0000 0010
	//    - So, 0x0020 becomes 0x0002 (or just 0x2).
	// For opcode 0x1A23, y would be 0x2 (decimal 2).

	n := opcode & 0x000F
	//get the value of the fourth (last) nibble of the opcode.

	//    - 0x000F in binary is 0000 0000 0000 1111.
	//      Opcode: 0001 1010 0010 0011 (0x1A23)
	//      Mask:   0000 0000 0000 1111 (0x000F)
	//      Result: 0000 0000 0000 0011 (0x0003)

	nn := byte(opcode & 0x00FF)
	// Purpose: To get the value of the last byte (last two nibbles) of the opcode.
	// This is often an 8-bit constant.
	//
	// Breakdown:
	// 1. opcode & 0x00FF:
	//    - 0x00FF in binary is 0000 0000 1111 1111.
	//      Opcode: 0001 1010 0010 0011 (0x1A23)
	//      Mask:   0000 0000 1111 1111 (0x00FF)
	//      Result: 0000 0000 0010 0011 (0x0023)
	//    - This isolates the last byte ('23' in 0x1A23).
	// 2. byte(...):
	//    - This converts the result (which is uint16) to a byte type (an 8-bit unsigned integer).
	// For opcode 0x1A23, nn would be 0x23 (decimal 35).

	nnn := opcode & 0x0FFF
	// Purpose: To get the value of the last three nibbles (12 bits) of the opcode.
	// This is almost always a memory address.
	//
	// Breakdown:
	// 1. opcode & 0x0FFF:
	//    - 0x0FFF in binary is 0000 1111 1111 1111.
	//      Opcode: 0001 1010 0010 0011 (0x1A23)
	//      Mask:   0000 1111 1111 1111 (0x0FFF)
	//      Result: 0000 1010 0010 0011 (0x0A23)
	//    - This isolates the last three nibbles ('A23' in 0x1A23).
	// For opcode 0x1A23, nnn would be 0xA23 (decimal 2595).

	// --- Opcode Decoding Strategy (Modified for "Simpler" Case Names) ---
	// We first extract the numerical value of the most significant nibble (first hex digit).
	// (opcode & 0xF000) isolates the first nibble (e.g., 0x1234 becomes 0x1000).
	// >> 12 shifts this isolated nibble to the rightmost position, so it becomes its actual value (0x0 to 0xF).
	// Example: if opcode is 0x1234, firstNibble becomes 0x1.
	// Example: if opcode is 0x8AB2, firstNibble becomes 0x8.
	firstNibble := (opcode & 0xF000) >> 12

	switch firstNibble { // Switch on the value of the first nibble (0x0 to 0xF).

	// --- 0xxx Instructions ---//
	// Opcodes starting with 0 (firstNibble == 0x0)
	case 0x0:
		// For 0xxx opcodes, the first nibble alone isn't enough.
		// We need to look at the entire opcode for specifics like 00E0 (CLS) or 00EE (RET).
		// The SYS 0NNN instruction is also in this group but usually ignored by modern emulators.
		switch opcode {
		case 0x00E0: // Opcode 00E0: CLS - Clear the display.
			// Sets all pixels on the 64x32 display to off (false).
			for i := range c.Display {
				c.Display[i] = false
			}
		case 0x00EE: // Opcode 00EE: RET - Return from a subroutine.
			// 1. Decrement the stack pointer (SP).
			// 2. Set the program counter (PC) to the address at the new SP location on the stack.
			// This effectively goes back to where the subroutine was called from.
			c.SP--
			c.PC = c.Stack[c.SP]
		}

	// --- 1NNN: JP addr - Jump to location NNN. ---
	case 0x1: // First nibble is 1 (opcodes like 0x1NNN).
		// Sets the program counter (PC) to the address NNN.
		// NNN is the lower 12 bits of the opcode (already extracted as 'nnn').
		c.PC = nnn

	// --- 2NNN: CALL addr - Call subroutine at NNN. ---
	case 0x2: // First nibble is 2 (opcodes like 0x2NNN).
		// 1. Store the current program counter (PC) on the stack.
		//    (PC already points to the *next* instruction because we incremented it after fetching).
		// 2. Increment the stack pointer (SP).
		// 3. Set the PC to the address NNN (the subroutine's starting address).
		c.Stack[c.SP] = c.PC
		c.SP++
		c.PC = nnn

	// --- 3XNN: SE Vx, byte - Skip next instruction if Vx == NN. ---
	case 0x3: // First nibble is 3 (opcodes like 0x3XNN).
		// Compares register VX (where X is 'x') with the value NN (extracted as 'nn').
		// If they are equal, increment the PC by 2, thus skipping the next instruction.
		// This is a conditional jump instruction.
		// if the register VX is equal to the value NN, then the next instruction is skipped
		if c.V[x] == nn {
			c.PC += 2
		}
		// so how this works is we get the opcode of for exmaple 0x340A
		// first for the first nibble we see its 0x3
		// next the 2nd nibble we see its 0x4
		//
		// next the 3rd nibble we see its 0x0
		// but remeber 3 and 4th nibble is the value of NN
		// also we get the value of x from the 2nd nibble from the mask 0000 1111 0000 0000
		// now we know that the regitery of c.V[x] = 0x4 and that the value of NN is 0xA
		// therefore we would not skip this instruction

	// --- 4XNN: SNE Vx, byte - Skip next instruction if Vx != NN. ---
	case 0x4: // First nibble is 4 (opcodes like 0x4XNN).
		// Compares register VX ('x') with the value NN ('nn').
		// If they are NOT equal, increment the PC by 2, skipping the next instruction.
		if c.V[x] != nn {
			c.PC += 2
		}

	// --- 5XY0: SE Vx, Vy - Skip next instruction if Vx == Vy. ---
	case 0x5: // First nibble is 5 (opcodes like 0x5XY0).
		// This instruction family expects the last nibble (N) to be 0.
		// We check n (which is opcode & 0x000F).
		if n == 0x0 { // Ensure it's specifically a 5XY0 opcode.
			// Compares register VX ('x') with register VY ('y').
			// If they are equal, increment PC by 2.
			if c.V[x] == c.V[y] {
				c.PC += 2
			}
		}
		// Other 5XYN opcodes (where N is not 0) are not standard.

	// --- 6XNN: LD Vx, byte - Set Vx = NN. ---
	case 0x6: // First nibble is 6 (opcodes like 0x6XNN).
		// Puts the value NN ('nn', the last byte of opcode) into register VX ('x').
		c.V[x] = nn

	// --- 7XNN: ADD Vx, byte - Set Vx = Vx + NN. ---
	case 0x7: // First nibble is 7 (opcodes like 0x7XNN).
		// Adds the value NN ('nn') to register VX ('x'). The result is stored in VX.
		// VF (carry flag) is NOT affected by this instruction.
		c.V[x] += nn

	// --- 8xxx Instructions ---//
	// Opcodes starting with 8 (firstNibble == 0x8).
	// These are arithmetic and logical operations. The specific operation
	// is determined by the last nibble 'n' (opcode & 0x000F).
	case 0x8:
		switch n { // Check the last nibble 'n' for the specific 8XYN operation.
		case 0x0: // Opcode pattern 8XY0: LD Vx, Vy - Set Vx = Vy.
			// Stores the value of register VY ('y') in register VX ('x').
			c.V[x] = c.V[y]
		case 0x1: // Opcode pattern 8XY1: OR Vx, Vy - Set Vx = Vx OR Vy.
			// Performs a bitwise OR on VX and VY. Stores the result in VX.
			c.V[x] |= c.V[y]
		case 0x2: // Opcode pattern 8XY2: AND Vx, Vy - Set Vx = Vx AND Vy.
			// Performs a bitwise AND on VX and VY. Stores the result in VX.
			c.V[x] &= c.V[y]
		case 0x3: // Opcode pattern 8XY3: XOR Vx, Vy - Set Vx = Vx XOR Vy.
			// Performs a bitwise XOR on VX and VY. Stores the result in VX.
			c.V[x] ^= c.V[y]
		case 0x4: // Opcode pattern 8XY4: ADD Vx, Vy - Set Vx = Vx + Vy, set VF = carry.
			// Adds VY to VX. If the result is > 255 (overflows 8 bits),
			// VF (register V[15]) is set to 1 (carry), otherwise 0.
			// Only the lowest 8 bits of the result are stored in VX.
			if uint16(c.V[x])+uint16(c.V[y]) > 0xFF {
				c.V[0xF] = 1 // Set carry flag
			} else {
				c.V[0xF] = 0 // Clear carry flag
			}
			c.V[x] += c.V[y] // Perform the addition.
		case 0x5: // Opcode pattern 8XY5: SUB Vx, Vy - Set Vx = Vx - Vy, set VF = NOT borrow.
			// Subtracts VY from VX.
			// If VX >= VY, VF is set to 1 (NO borrow). Otherwise, VF is set to 0 (borrow occurred).
			if c.V[x] > c.V[y] { // Note: some sources say >= for no borrow. Common use is >.
				c.V[0xF] = 1
			} else {
				c.V[0xF] = 0
			}
			c.V[x] -= c.V[y]
		case 0x6: // Opcode pattern 8XY6: SHR Vx {, Vy} - Set Vx = Vx SHR 1.
			// VY is typically ignored in modern interpretations.
			// Stores the least significant bit of VX
			// Then shifts VX right by 1 (divides VX by 2).

			if c.V[x]&0x1 != 0 {
				c.V[0xF] = 1
			} else {
				c.V[0xF] = 0
			}
			c.V[x] >>= 1
		case 0x7: // Opcode pattern 8XY7: SUBN Vx, Vy - Set Vx = Vy - Vx, set VF = NOT borrow.
			// Subtracts VX from VY.
			// If VY >= VX, VF is set to 1 (NO borrow). Otherwise, 0.
			if c.V[y] > c.V[x] {
				c.V[0xF] = 1
			} else {
				c.V[0xF] = 0
			}
			c.V[x] = c.V[y] - c.V[x]

		case 0xE: // Opcode pattern 8XYE: SHL Vx {, Vy} - Set Vx = Vx SHL 1.
			// VY is typically ignored.
			// Stores the most significant bit of VX in VF.
			// Then shifts VX left by 1 (multiplies VX by 2).
			// checking if the most signifit bit is not 0 which would mean
			// that it would have to be one
			if c.V[x]&0x80 != 0 {
				c.V[0xF] = 1
			} else {
				c.V[0xF] = 0
			}
			c.V[x] <<= 1
		}

	case 0x9: // First nibble is 9 (opcodes like 0x9XY0).
		// This instruction family expects the last nibble (N) to be 0.
		// if n == 0x0 { // Ensure it's specifically a 9XY0 opcode.

		if n == 0x0 {
			if c.V[x] != c.V[y] {
				c.PC += 2
			}
		}

	case 0xA: // First nibble is A (opcodes like 0xANNN).
		// Sets the index register (I) to the address NNN ('nnn').
		c.I = nnn

	// --- BNNN: JP V0, addr - Jump to location NNN + V0. ---
	case 0xB: // First nibble is B (opcodes like 0xBNNN).
		// Sets the program counter (PC) to NNN ('nnn') plus the value in register V0.
		c.PC = nnn + uint16(c.V[0])

	// --- CXNN: RND Vx, byte - Set Vx = random byte AND NN. ---
	case 0xC: // First nibble is C (opcodes like 0xCXNN).
		// Generates a random 8-bit number (0-255).
		// Performs a bitwise AND with the value NN ('nn').
		// Stores the result in register VX ('x').
		c.V[x] = byte(c.rng.Intn(256)) & nn

	// --- DXYN: DRW Vx, Vy, nibble - Display N-byte sprite. ---
	case 0xD: // First nibble is D (opcodes like 0xDXYN).
		// Draws an N-byte sprite ('n' is the height) starting at memory location I,
		// at screen coordinates (VX, VY). Sets VF = collision.
		// VX is c.V[x], VY is c.V[y].
		c.drawSprite(c.V[x], c.V[y], n)

	//starting with E (firstNibble == 0xE).
	// These are key-based skip instructions.
	// is determined by the last byte 'nn' (opcode & 0x00FF).
	case 0xE:
		switch nn { // Check the last byte 'nn'.
		case 0x9E: //  EX9E: SKP Vx - Skip if key Vx is pressed.
			// Checks if the key corresponding to the value in register VX ('x') is pressed.
			// If yes, increment PC by 2.
			if c.Keys[c.V[x]] {
				c.PC += 2
			}
		case 0xA1: //  EXA1: SKNP Vx - Skip if key Vx is NOT pressed.
			// Checks if the key corresponding to the value in register VX ('x') is NOT pressed.
			// If yes, increment PC by 2.
			if !c.Keys[c.V[x]] {
				c.PC += 2
			}
		}

	// Opcodes starting with F (firstNibble == 0xF).
	// is determinneddsed by the last byte 'nn' (opcode & 0x00FF).
	case 0xF:
		switch nn { // Check the last byte nn -
		case 0x07: // FX07: LD Vx, DT - Set Vx = delay timer.
			c.V[x] = c.DelayTimer
		case 0x0A: // FX0A: LD Vx, K - Wait for key press, store in Vx.
			keyPressed := false
			for i := 0; i < 16; i++ {
				if c.Keys[i] {
					c.V[x] = byte(i)
					keyPressed = true
					break
				}
			}
			if !keyPressed {
				c.PC -= 2
			}
		case 0x15: // Opcode pattern FX15: LD DT, Vx - Set delay timer = Vx.
			c.DelayTimer = c.V[x]
		case 0x18: // Opcode pattern FX18: LD ST, Vx - Set sound timer = Vx.
			c.SoundTimer = c.V[x]
		case 0x1E: // Opcode pattern FX1E: ADD I, Vx - Set I = I + Vx.
			c.I += uint16(c.V[x])
		case 0x29: // Opcode pattern FX29: LD F, Vx - Set I = location of sprite for digit Vx.
			// Each digit sprite is 5 bytes long.
			c.I = uint16(c.V[x] * 5)
		case 0x33: // Opcode pattern FX33: LD B, Vx - Store BCD of Vx.
			// Memory[I] = hundreds, Memory[I+1] = tens, Memory[I+2] = ones.
			c.Memory[c.I] = c.V[x] / 100
			c.Memory[c.I+1] = (c.V[x] / 10) % 10
			c.Memory[c.I+2] = c.V[x] % 10
		case 0x55: // Opcode pattern FX55: LD [I], Vx - Store V0-Vx in memory starting at I.
			for i := uint16(0); i <= x; i++ {
				c.Memory[c.I+i] = c.V[i]
			}
		case 0x65: // Opcode pattern FX65: LD Vx, [I] - Read V0-Vx from memory starting at I.
			for i := uint16(0); i <= x; i++ {
				c.V[i] = c.Memory[c.I+i]
			}
		}
	default:
	}
}

// drawSprite draws a sprite at position (x, y) with height n
// This is the implementation of the DXYN instruction, which handles graphics
// Sprites are XORed onto the display, and VF is set if any pixels are flipped from on to off
func (c *Chip8) drawSprite(x, y byte, height uint16) {
	// Reset collision flag
	c.V[0xF] = 0

	// Loop over each row of the sprite
	for yLine := uint16(0); yLine < height; yLine++ {
		// Get the pixel data from memory at location I + yLine
		// Each byte represents 8 horizontal pixels (one row of the sprite)
		pixel := c.Memory[c.I+yLine]

		// Loop over each bit in the pixel data (8 bits, from left to right)
		for xLine := uint16(0); xLine < 8; xLine++ {
			// Check if the current pixel is set (1)
			// We check from the most significant bit (leftmost) to the least significant bit (rightmost)
			if (pixel & (0x80 >> xLine)) != 0 {
				// Calculate the position on the display
				// Apply modulo to wrap around the edges of the screen (sprites can wrap)
				xPos := (uint16(x) + xLine) % 64
				yPos := (uint16(y) + yLine) % 32

				// Convert x,y coordinates to the 1D array index
				position := yPos*64 + xPos

				// Check for collision (if a pixel is already set and we're trying to set it again)
				if c.Display[position] {
					c.V[0xF] = 1 // Set collision flag
				}

				// XOR the pixel - this flips the pixel's state
				// This means: if it was off, turn it on; if it was on, turn it off
				c.Display[position] = !c.Display[position]
			}
		}
	}
}
