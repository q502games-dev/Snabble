# Snabble

Snake meets Scrabble. Eat letters, form words, survive the chaos.

**[Play Now](https://q502games-dev.github.io/Snabble/)**

## How to Play

- **Move** - Arrow keys or swipe (mobile)
- **Word Mode** - Press W or tap the board (need 2+ letters)
- **Boost** - Hold Up arrow or long-press (uses stored boosts)
- **Restart** - Spacebar

## Rules

- Collect letters by eating them on the grid
- Press W to enter Word Mode and type a word from your collected letters
- Words that use at least half your snake length shrink the snake
- Words too short grow the snake by unused letter count
- Score enough points to advance through 8 levels
- 5 minutes per level. Time's up = game over
- Hitting a wall or yourself = game over

## Features

- 8 progressively harder levels with walls, speed pads, portals, and pitfalls
- AI enemy snake (from level 5) that hunts vowels
- Falling letters to catch (from level 6)
- Wildcard events with bonus star tiles
- Combo multiplier for consecutive words
- Perfect shed bonus (2x) for using all collected letters
- Boost powerups granted over time
- Lifeline pickups that shrink your snake
- 16,800+ word dictionary

## Tech

Built with React + Vite. Deployed on GitHub Pages.

## Development

```bash
npm install
npm run dev
```

Open http://localhost:5173/Snabble/

## License

MIT
