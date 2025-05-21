package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"time"

	"github.com/faiface/pixel"
	"github.com/faiface/pixel/imdraw"
	"github.com/faiface/pixel/pixelgl"
	"golang.org/x/image/colornames"
)

const (
	scale          = 10
	cyclesPerFrame = 10
)

func run() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: chip8 <ROM file>")
		return
	}

	// Load ROM
	romData, err := ioutil.ReadFile(os.Args[1])
	if err != nil {
		fmt.Printf("Error loading ROM: %v\n", err)
		return
	}

	// Initialize CHIP-8
	chip8 := NewChip8()
	err = chip8.LoadROM(romData)
	if err != nil {
		fmt.Printf("Error loading ROM: %v\n", err)
		return
	}

	// Initialize window
	cfg := pixelgl.WindowConfig{
		Title:  "CHIP-8 Emulator",
		Bounds: pixel.R(0, 0, 64*scale, 32*scale),
		VSync:  true,
	}
	win, err := pixelgl.NewWindow(cfg)
	if err != nil {
		panic(err)
	}

	// Map keyboard keys to CHIP-8 keypad
	keyMap := map[pixelgl.Button]int{
		pixelgl.Key1: 0x1, pixelgl.Key2: 0x2, pixelgl.Key3: 0x3, pixelgl.Key4: 0xC,
		pixelgl.KeyQ: 0x4, pixelgl.KeyW: 0x5, pixelgl.KeyE: 0x6, pixelgl.KeyR: 0xD,
		pixelgl.KeyA: 0x7, pixelgl.KeyS: 0x8, pixelgl.KeyD: 0x9, pixelgl.KeyF: 0xE,
		pixelgl.KeyZ: 0xA, pixelgl.KeyX: 0x0, pixelgl.KeyC: 0xB, pixelgl.KeyV: 0xF,
	}

	imd := imdraw.New(nil)
	ticker := time.NewTicker(time.Second / 60)

	for !win.Closed() {
		// Handle input
		for key, chipKey := range keyMap {
			chip8.Keys[chipKey] = win.Pressed(key)
		}

		// Run multiple cycles per frame for speed
		for i := 0; i < cyclesPerFrame; i++ {
			chip8.Cycle()
		}

		// Draw display
		win.Clear(colornames.Black)
		imd.Clear()

		for y := 0; y < 32; y++ {
			for x := 0; x < 64; x++ {
				if chip8.Display[y*64+x] {
					imd.Color = colornames.White
					imd.Push(
						pixel.V(float64(x*scale), float64((31-y)*scale)),
						pixel.V(float64((x+1)*scale), float64((31-y+1)*scale)),
					)
					imd.Rectangle(0)
				}
			}
		}

		imd.Draw(win)
		win.Update()

		<-ticker.C
	}
}

func main() {
	pixelgl.Run(run)
}
