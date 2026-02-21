import { COLS, ROWS, DIR, DIRS_ARR, SP, VOWELS, STICKY_SET, LEVELS, BOOST_DURATION } from "./constants.js";

// ─── Trie for anagram solving ───────────────────────────────────────────────
class TrieNode {
  constructor() { this.children = {}; this.isWord = false; }
}

function buildTrie(dict) {
  const root = new TrieNode();
  for (const w of dict) {
    let node = root;
    for (const ch of w.toUpperCase()) {
      if (!node.children[ch]) node.children[ch] = new TrieNode();
      node = node.children[ch];
    }
    node.isWord = true;
  }
  return root;
}

// ─── SnabbleBot ─────────────────────────────────────────────────────────────
export default class SnabbleBot {
  constructor(gsRef, dictRef, submitWord, exitWordMode, setUi) {
    this.gs = gsRef;          // React ref to game state
    this.dictRef = dictRef;   // React ref to dictionary Set
    this.submitWord = submitWord;
    this.exitWordMode = exitWordMode;
    this.setUi = setUi;
    this.trie = null;
    this.interval = null;
    this.active = false;
    this._lastWordAttempt = 0;
    this._lastWordFailed = false; // track if last attempt found no word
    this._targetPos = null;
    this._currentPath = null;
    this._cachedObstacles = null;
    this._cachedObstaclesTick = -1;
  }

  // ─── Lifecycle ──────────────────────────────────────────────────────────────
  start() {
    if (this.active) return;
    this.active = true;
    if (!this.trie && this.dictRef.current) {
      this.trie = buildTrie(this.dictRef.current);
    }
    this.interval = setInterval(() => this.tick(), 50);
  }

  stop() {
    this.active = false;
    if (this.interval) { clearInterval(this.interval); this.interval = null; }
    this._targetPos = null;
    this._currentPath = null;
  }

  // ─── Main tick ──────────────────────────────────────────────────────────────
  tick() {
    const g = this.gs.current;
    if (!g || g.gameOver) { this.stop(); return; }
    if (g.levelAdvancePending) return;

    // If in word mode, the bot submitted a word and is waiting — do nothing
    if (g.wordMode) return;

    // Invalidate obstacle cache each tick
    this._cachedObstacles = null;

    const now = performance.now();

    // 1. Should we form a word?
    if (this.shouldFormWord(g, now)) {
      this.attemptWord(g, now);
      return;
    }

    // 2. Should we use boost?
    if (this.shouldUseBoost(g, now)) {
      this.activateBoost(g, now);
    }

    // 3. Pick a target and navigate
    this.navigate(g, now);
  }

  // ─── Strategy: should form a word? ──────────────────────────────────────────
  shouldFormWord(g, now) {
    if (g.collected.length < 3) return false;

    // Shorter cooldown if last attempt failed to find a word (keep trying with new letters)
    const cooldown = this._lastWordFailed ? 300 : 800;
    if (now - this._lastWordAttempt < cooldown) return false;

    const lvlIdx = g.level;
    const target = LEVELS[Math.min(lvlIdx, LEVELS.length - 1)].target;
    const scoreReached = g.score >= target && lvlIdx < LEVELS.length;
    const halfLen = Math.ceil(g.snake.length / 2);
    const levelElapsed = (now - g.levelStartTime) / 1000;
    const timeLeft = Math.max(0, 300 - levelElapsed);

    // Score reached — keep forming words for more points
    if (scoreReached && g.collected.length >= 3) return true;

    // Snake getting long — form words early to prevent death spiral
    if (g.snake.length > 10 && g.collected.length >= halfLen) return true;

    // Poison deadline approaching
    if (g.poisonLetters.length > 0) {
      const soonest = g.poisonLetters.reduce((min, p) => Math.min(min, p.deadline - now), Infinity);
      if (soonest < 5000 && g.collected.length >= 3) return true;
    }

    // Combo opportunity (within 12s of last word, up to 5x)
    if (g.comboCount > 0 && g.comboCount < 5 && g.lastWordTime > 0 && now - g.lastWordTime < 12000) {
      if (g.collected.length >= halfLen) return true;
    }

    // Time pressure
    if (timeLeft < 60 && g.collected.length >= 3) return true;

    // Have enough letters to shed safely — try at 3+ collected
    if (g.collected.length >= halfLen && g.collected.length >= 3) return true;

    return false;
  }

  // ─── Attempt to form and submit a word ──────────────────────────────────────
  attemptWord(g, now) {
    this._lastWordAttempt = now;
    if (!this.trie) return;

    const halfLen = Math.ceil(g.snake.length / 2);
    const desperate = g.snake.length > 14 ||
      (g.poisonLetters.length > 0 && g.poisonLetters.some(p => p.deadline - now < 3000));

    let word = null;

    // Try to find a word that meets the shed threshold (no penalty)
    const minLen = Math.max(3, halfLen);
    word = this.findBestWord(g.collected, minLen);

    // Fallback: any 3+ letter word — but ONLY if it won't cause a penalty,
    // or if we're desperate (snake critically long / poison imminent)
    if (!word) {
      const fallback = this.findBestWord(g.collected, 3);
      if (fallback) {
        if (fallback.length >= halfLen || desperate) {
          word = fallback;
        }
        // else: don't submit — a penalty word would make things worse
      }
    }

    if (word) {
      this._lastWordFailed = false;
      // Enter word mode and immediately submit
      g.wordMode = true;
      this.setUi(p => ({ ...p, wordMode: true, wordInput: word.toUpperCase(), wordTimeLeft: 10 }));
      // Small delay to let UI update, then submit
      setTimeout(() => {
        if (g.wordMode) {
          this.submitWord(word);
        }
      }, 80);
    } else {
      // Couldn't find a usable word — use shorter cooldown for retry
      this._lastWordFailed = true;
    }
  }

  // ─── Trie-based anagram solver ──────────────────────────────────────────────
  findBestWord(collected, minLen) {
    const pool = [...collected];
    const wildcards = pool.filter(c => c === "★").length;
    const letters = pool.filter(c => c !== "★");
    const freq = {};
    for (const ch of letters) freq[ch] = (freq[ch] || 0) + 1;

    let best = null;
    let bestScore = -1;

    const search = (node, depth, used, wc) => {
      if (node.isWord && depth >= minLen) {
        const w = used.join("");
        const score = this.scoreWord(w, collected);
        if (score > bestScore) {
          bestScore = score;
          best = w;
        }
      }
      if (depth >= 10) return; // max word length

      for (const ch in node.children) {
        if (freq[ch] && freq[ch] > 0) {
          freq[ch]--;
          used.push(ch);
          search(node.children[ch], depth + 1, used, wc);
          used.pop();
          freq[ch]++;
        }
        if (wc > 0) {
          used.push(ch);
          search(node.children[ch], depth + 1, used, wc - 1);
          used.pop();
        }
      }
    };

    search(this.trie, 0, [], wildcards);
    return best ? best.toLowerCase() : null;
  }

  findWordToMaximizeShed(collected) {
    // Try to find a single word that uses ALL collected letters
    const word = this.findBestWordExact(collected, collected.length);
    if (word) return word;

    // Otherwise find the longest valid word we can
    for (let len = Math.min(collected.length, 10); len >= 3; len--) {
      const w = this.findBestWordExact(collected, len);
      if (w) return w;
    }
    return null;
  }

  findBestWordExact(collected, targetLen) {
    const pool = [...collected];
    const wildcards = pool.filter(c => c === "★").length;
    const letters = pool.filter(c => c !== "★");
    const freq = {};
    for (const ch of letters) freq[ch] = (freq[ch] || 0) + 1;

    let best = null;
    let bestScore = -1;

    const search = (node, depth, used, wc) => {
      if (depth === targetLen) {
        if (node.isWord) {
          const w = used.join("");
          const score = this.scoreWord(w, collected);
          if (score > bestScore) {
            bestScore = score;
            best = w;
          }
        }
        return;
      }

      for (const ch in node.children) {
        if (freq[ch] && freq[ch] > 0) {
          freq[ch]--;
          used.push(ch);
          search(node.children[ch], depth + 1, used, wc);
          used.pop();
          freq[ch]++;
        }
        if (wc > 0) {
          used.push(ch);
          search(node.children[ch], depth + 1, used, wc - 1);
          used.pop();
        }
      }
    };

    search(this.trie, 0, [], wildcards);
    return best ? best.toLowerCase() : null;
  }

  scoreWord(word, collected) {
    let pts = 0;
    for (const ch of word) pts += (SP[ch] || 1);
    // Bonus for length (longer sheds more)
    pts += word.length * 5;
    // Bonus for using more of collected (better shed ratio)
    pts += (word.length / Math.max(1, collected.length)) * 20;
    return pts;
  }

  // ─── Boost strategy ────────────────────────────────────────────────────────
  shouldUseBoost(g, now) {
    if (g.boosts <= 0) return false;
    if (g.boostActiveUntil > now) return false;

    const levelElapsed = (now - g.levelStartTime) / 1000;
    const timeLeft = Math.max(0, 300 - levelElapsed);

    // Snake is long and we need to move fast
    if (g.snake.length > 14) return true;
    // Time running low
    if (timeLeft < 45) return true;

    return false;
  }

  activateBoost(g, now) {
    g.boosts--;
    g.boostActiveUntil = now + BOOST_DURATION;
    g.speed = Math.max(60, g.baseSpeed * 0.4);
    this.setUi(p => ({ ...p, boosts: g.boosts, boostActive: true }));
  }

  // ─── Navigation ─────────────────────────────────────────────────────────────
  navigate(g, now) {
    const head = g.snake[0];
    const obstacles = this.getObstacles(g);
    const key = (x, y) => y * COLS + x;
    const bestTarget = this.evaluateTargets(g, now);

    if (bestTarget) {
      const path = this.bfs(head, bestTarget, g, obstacles);
      if (path && path.length > 1) {
        const next = path[1];
        let dir = this.vecToDir(head, next);
        if (dir && !this.is180(dir, g.dir)) {
          const nx = (head.x + dir.x + COLS) % COLS;
          const ny = (head.y + dir.y + ROWS) % ROWS;
          if (!obstacles.has(key(nx, ny))) {
            // Verify the chosen direction doesn't lead into a dead end
            const reachable = this.floodFillCount(nx, ny, g, obstacles);
            if (reachable >= g.snake.length) {
              g.nextDir = dir;
              return;
            }
            // Path leads to tight space — fall through to flood fill
          }
        }
      }
    }

    // Fallback: flood-fill safest direction
    this.floodFillFallback(g, obstacles);
  }

  vecToDir(from, to) {
    // Handle toroidal wrapping — check all 4 cardinal directions
    if (to.x === (from.x + 1) % COLS && to.y === from.y) return DIR.RIGHT;
    if (to.x === (from.x - 1 + COLS) % COLS && to.y === from.y) return DIR.LEFT;
    if (to.y === (from.y + 1) % ROWS && to.x === from.x) return DIR.DOWN;
    if (to.y === (from.y - 1 + ROWS) % ROWS && to.x === from.x) return DIR.UP;

    // Portal jump — pick the safest direction via flood fill
    const safeDirs = this.getSafeDirections(from, this.gs.current);
    if (safeDirs.length === 0) return DIR.RIGHT;
    if (safeDirs.length === 1) return safeDirs[0];

    // Among safe directions, pick the one with most reachable space
    let bestDir = safeDirs[0];
    let bestCount = -1;
    const obstacles = this.getObstacles(this.gs.current);
    for (const d of safeDirs) {
      const nx = (from.x + d.x + COLS) % COLS;
      const ny = (from.y + d.y + ROWS) % ROWS;
      const count = this.floodFillCount(nx, ny, this.gs.current, obstacles);
      if (count > bestCount) { bestCount = count; bestDir = d; }
    }
    return bestDir;
  }

  is180(newDir, curDir) {
    return newDir.x + curDir.x === 0 && newDir.y + curDir.y === 0;
  }

  // ─── Target evaluation ─────────────────────────────────────────────────────
  evaluateTargets(g, now) {
    const head = g.snake[0];
    const candidates = [];

    const lvlIdx = g.level;
    const target = LEVELS[Math.min(lvlIdx, LEVELS.length - 1)].target;
    const scoreReached = g.score >= target && lvlIdx < LEVELS.length;
    // Normal food
    for (const f of g.foods) {
      if (f.type === "poison") {
        // Soft avoid — low score
        candidates.push({ x: f.x, y: f.y, score: -50 });
        continue;
      }
      if (f.type === "wildcard") {
        // High priority
        candidates.push({ x: f.x, y: f.y, score: 200 });
        continue;
      }
      // Normal food
      let s = 10;
      if (VOWELS.has(f.letter)) s += 30; // vowel bonus
      if (STICKY_SET.has(f.letter)) s -= 20; // sticky penalty
      s += this.letterUtility(f.letter, g.collected);
      candidates.push({ x: f.x, y: f.y, score: s });
    }

    // Falling letters (free — no growth)
    if (g.fallingLetters) {
      for (const fl of g.fallingLetters) {
        let s = 60; // free letter bonus
        if (VOWELS.has(fl.letter)) s += 20;
        s += this.letterUtility(fl.letter, g.collected);
        candidates.push({ x: fl.x, y: fl.y, score: s });
      }
    }

    // Letter pitfalls (free letters)
    if (g.pitfalls) {
      for (const pf of g.pitfalls) {
        if (pf.type === "letter" && pf.letter) {
          let s = 40;
          if (VOWELS.has(pf.letter)) s += 15;
          candidates.push({ x: pf.x, y: pf.y, score: s });
        } else if (pf.type === "bomb") {
          candidates.push({ x: pf.x, y: pf.y, score: -100 });
        } else if (pf.type === "speed") {
          candidates.push({ x: pf.x, y: pf.y, score: -30 });
        }
      }
    }

    // Lifeline
    if (g.lifelineOnGrid) {
      const lifeScore = g.snake.length > 8 ? 300 : g.snake.length > 5 ? 150 : 50;
      candidates.push({ x: g.lifelineOnGrid.x, y: g.lifelineOnGrid.y, score: lifeScore });
    }

    // Shrink powerup
    for (const pu of g.powerups) {
      if (pu.type === "shrink") {
        const s = g.snake.length > 8 ? 180 : 60;
        candidates.push({ x: pu.x, y: pu.y, score: s });
      } else if (pu.type === "freeze") {
        candidates.push({ x: pu.x, y: pu.y, score: 20 });
      }
    }

    if (candidates.length === 0) return null;

    // Weight by distance (prefer closer targets)
    for (const c of candidates) {
      const dist = this.wrapDist(head.x, c.x, COLS) + this.wrapDist(head.y, c.y, ROWS);
      c.score -= dist * 2; // penalty for distance
    }

    candidates.sort((a, b) => b.score - a.score);
    return candidates[0];
  }

  letterUtility(letter, collected) {
    // How useful is this letter for forming words?
    // Favor letters that help complete common patterns
    const hasVowel = collected.some(c => VOWELS.has(c));
    const hasConsonant = collected.some(c => c !== "★" && !VOWELS.has(c));

    if (VOWELS.has(letter) && !hasVowel) return 25;
    if (!VOWELS.has(letter) && !hasConsonant) return 15;

    // Common letters
    if ("ETAOINSHRDL".includes(letter)) return 10;
    return 0;
  }

  wrapDist(a, b, max) {
    const d = Math.abs(a - b);
    return Math.min(d, max - d);
  }

  // ─── BFS pathfinding (toroidal, portal-aware) ──────────────────────────────
  bfs(start, goal, g, obstacles) {
    if (!obstacles) obstacles = this.getObstacles(g);
    const key = (x, y) => y * COLS + x;
    const visited = new Set();
    const queue = [{ x: start.x, y: start.y, path: [{ x: start.x, y: start.y }] }];
    visited.add(key(start.x, start.y));

    let iterations = 0;
    const maxIter = COLS * ROWS * 2;

    while (queue.length > 0 && iterations < maxIter) {
      iterations++;
      const cur = queue.shift();

      if (cur.x === goal.x && cur.y === goal.y) {
        return cur.path;
      }

      for (const d of DIRS_ARR) {
        let nx = (cur.x + d.x + COLS) % COLS;
        let ny = (cur.y + d.y + ROWS) % ROWS;

        // Portal teleportation
        for (const pt of g.portals) {
          if (nx === pt.x1 && ny === pt.y1) { nx = pt.x2; ny = pt.y2; break; }
          if (nx === pt.x2 && ny === pt.y2) { nx = pt.x1; ny = pt.y1; break; }
        }

        const k = key(nx, ny);
        if (visited.has(k)) continue;
        if (obstacles.has(k)) continue;

        visited.add(k);
        queue.push({ x: nx, y: ny, path: [...cur.path, { x: nx, y: ny }] });
      }
    }

    return null; // no path found
  }

  getObstacles(g) {
    const obs = new Set();
    const key = (x, y) => y * COLS + x;

    // Walls — include current position AND next position (walls move horizontally)
    for (const w of g.walls) {
      obs.add(key(w.x, w.y));
      obs.add(key((w.x + w.dx + COLS) % COLS, w.y)); // anticipate next wall move
    }

    // Snake body (skip head = index 0, include tail since snake may eat food and not shrink)
    for (let i = 1; i < g.snake.length; i++) {
      obs.add(key(g.snake[i].x, g.snake[i].y));
    }

    // AI snake
    if (g.aiSnake) {
      for (const s of g.aiSnake) obs.add(key(s.x, s.y));
    }

    // Bomb pitfalls
    if (g.pitfalls) {
      for (const pf of g.pitfalls) {
        if (pf.type === "bomb") obs.add(key(pf.x, pf.y));
      }
    }

    return obs;
  }

  // ─── Safety layer ──────────────────────────────────────────────────────────
  getSafeDirections(pos, g) {
    if (!g) return DIRS_ARR;
    const safe = [];
    const obstacles = this.getObstacles(g);
    const key = (x, y) => y * COLS + x;

    for (const d of DIRS_ARR) {
      // No 180-degree reversal
      if (this.is180(d, g.dir)) continue;

      let nx = (pos.x + d.x + COLS) % COLS;
      let ny = (pos.y + d.y + ROWS) % ROWS;

      if (obstacles.has(key(nx, ny))) continue;

      safe.push(d);
    }

    return safe;
  }

  floodFillCount(startX, startY, g, obstacles) {
    if (!obstacles) obstacles = this.getObstacles(g);
    const key = (x, y) => y * COLS + x;
    const visited = new Set();
    const stack = [{ x: startX, y: startY }];
    const startKey = key(startX, startY);
    if (obstacles.has(startKey)) return 0;
    visited.add(startKey);
    let count = 0;

    while (stack.length > 0 && count < COLS * ROWS) {
      const cur = stack.pop();
      count++;

      for (const d of DIRS_ARR) {
        const nx = (cur.x + d.x + COLS) % COLS;
        const ny = (cur.y + d.y + ROWS) % ROWS;
        const k = key(nx, ny);
        if (visited.has(k) || obstacles.has(k)) continue;
        visited.add(k);
        stack.push({ x: nx, y: ny });
      }
    }

    return count;
  }

  floodFillFallback(g, obstacles) {
    if (!obstacles) obstacles = this.getObstacles(g);
    const head = g.snake[0];
    const key = (x, y) => y * COLS + x;

    // First try: safe directions (no 180)
    const safeDirs = [];
    for (const d of DIRS_ARR) {
      if (this.is180(d, g.dir)) continue;
      const nx = (head.x + d.x + COLS) % COLS;
      const ny = (head.y + d.y + ROWS) % ROWS;
      if (!obstacles.has(key(nx, ny))) safeDirs.push(d);
    }

    let dirs = safeDirs;

    // Last resort: if no safe non-180 directions, try ALL directions including 180
    // (180 will cause self-collision for snake length > 1, but we're dead anyway)
    if (dirs.length === 0) {
      for (const d of DIRS_ARR) {
        const nx = (head.x + d.x + COLS) % COLS;
        const ny = (head.y + d.y + ROWS) % ROWS;
        if (!obstacles.has(key(nx, ny))) dirs.push(d);
      }
    }

    if (dirs.length === 0) return; // truly trapped

    let bestDir = dirs[0];
    let bestCount = -1;

    for (const d of dirs) {
      const nx = (head.x + d.x + COLS) % COLS;
      const ny = (head.y + d.y + ROWS) % ROWS;
      const count = this.floodFillCount(nx, ny, g, obstacles);
      if (count > bestCount) {
        bestCount = count;
        bestDir = d;
      }
    }

    g.nextDir = bestDir;
  }
}
