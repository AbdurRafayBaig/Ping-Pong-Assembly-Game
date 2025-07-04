# Ping-Pong-Assembly-Game
**Ping Pong Game in x86 Assembly Language** <br>
A classic two-player Ping Pong game built using Intel 8086 Assembly language. This project was developed to demonstrate how interactive applications can be implemented directly at the hardware level using low-level programming concepts such as BIOS video memory, keyboard interrupt handling, and direct screen rendering. It runs smoothly in DOSBox and showcases foundational principles of system-level game development.

**Problem Statement** <br>
Modern developers rarely get hands-on exposure to the core workings of input handling, screen control, and memory management. This project addresses that gap by creating a fully interactive game using only assembly code, without the aid of high-level libraries. It serves both as an educational exercise and a technical demonstration of what's possible using pure assembly.

**Objectives** <br>
Implement a fully functional two-player Ping Pong game in NASM (8086 architecture)
Handle real-time keyboard input using BIOS interrupts
Render paddles, ball, and scores using text-mode graphics via video memory
Introduce core game elements like collision detection, ball movement, and scoring
Optionally enhance the game with sound and visual improvements

**Development Environment** <br>
Language: Assembly (Intel 8086) <br>
Assembler: NASM <br>
Emulator: DOSBox <br>
Video Mode: Text-based screen rendering using BIOS memory segment B800h <br>
Input Method: Keyboard input using INT 16h <br>

**Game Description** <br>
This is a two-player Ping Pong game where each player controls a paddle on opposite sides of the screen. A ball moves back and forth between the paddles, and players must prevent it from crossing their side. The game continues until one of the players scores three points, after which a win message is displayed and the game resets.

**Core Features** <br>
**Player Controls** <br>
Player 1: W (move up), S (move down) <br>
Player 2: Page Up (move up), Page Down (move down)
**Ball Physics** <br>
The ball moves diagonally across the screen and bounces off paddles and walls
Direction changes dynamically based on collision logic

**Scoring System** <br>
Each time a player misses the ball, the opponent scores a point
First player to reach 3 points wins
Rendering
The entire game screen (paddles, ball, and score) is drawn using BIOS video memory
Custom routines are used to clear the screen and display text

**Additional Features** <br>
Optional background scrolling for visual interest
Sound effects on scoring and paddle collision
Restart functionality after game over

**Challenges and Solutions** <br>
Challenge: Handling simultaneous paddle and ball movement with minimal flicker <br>
Solution: Optimized code execution and delayed rendering for smoother gameplay <br>
Challenge: Detecting collisions accurately at high ball speeds <br>
Solution: Introduced directional flags and improved conditional checks <br>
**
Testing and Debugging** <br>
Used modular testing to verify paddle response, ball logic, and scoring behavior
Debugged using DOSBox’s debugger to step through register-level changes
Performed trial runs to refine movement smoothness and eliminate input lag

**Conclusion** <br>
This project demonstrates that complex, real-time applications like games can be created using only assembly language. From manually detecting keypresses to drawing shapes on the screen using video memory, this project offers deep insight into how early video games operated. It is a useful reference for anyone learning systems programming, computer architecture, or game development at the hardware level.
